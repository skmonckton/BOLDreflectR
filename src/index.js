// Original code: Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn
// Additions: Copyright (c) 2025 Jinhwan Kim
// Further modifications by Spencer K. Monckton

const { app, session, autoUpdater } = require("electron");
const { spawn } = require('child_process');
const path = require('path');
const os = require("os");
const ErrorHandler = require("./error-handler");
const ProcessManager = require("./process-manager"); // <-- import ProcessManager
const ServerUtils = require("./server-utils"); // 추가
const AppState = require("./app-state");
const WindowManager = require("./window-manager");
const [, gitOwner, gitRepo] = new URL(require('../package.json').repository.url).pathname.split('/');

// Initialize global state
let appState;

// Auto-updater setup (Windows only, skip in development)
if (os.platform() === "win32" && app.isPackaged) {
  const feedURL = `https://update.electronjs.org/${gitOwner}/${gitRepo}/win32/${app.getVersion()}`;
  autoUpdater.setFeedURL({ url: feedURL });

  autoUpdater.on("update-available", () => {
    console.log("Update available. Downloading...");
  });

  autoUpdater.on("update-downloaded", (event, releaseNotes, releaseName) => {
    const { dialog } = require("electron");
    dialog.showMessageBox({
      type: "info",
      title: "Update Ready",
      message: `Version ${releaseName} is ready to install. ${app.getName()} will restart to apply the update.`,
      buttons: ["Restart Now", "Later"],
    }).then((result) => {
      if (result.response === 0) {
        autoUpdater.quitAndInstall();
      }
    });
  });

  autoUpdater.on("error", (err) => {
    ErrorHandler.logError("autoUpdater", err);
  });
}

// 1. Start R process
async function startShinyProcessWithState(appState) {
  try {
    const rShinyProcess = await ProcessManager.startShinyProcess(appState);
    appState.setShinyProcess(rShinyProcess);
    return true;
  } catch (e) {
    ErrorHandler.logError("startShinyProcessWithState", e);
    return false;
  }
}

// 2. Check if server is ready
async function waitForServerReady(appState, strategy = "exponential") {
  const url = appState.getServerUrl();
  const statusCallback = (msg) => {
    if (appState.loadingSplashScreen) {
      appState.loadingSplashScreen.webContents.send(
        "server-status-message",
        msg
      );
    }
  };
  return await ServerUtils.waitForServerSmart(url, strategy, statusCallback);
}

// 3. Calculate retry delay
function getRetryDelay(attempt, base = 2000, factor = 1.5, max = 10000) {
  return Math.min(base * Math.pow(factor, attempt), max);
}

// 4. Terminate process and reset state
async function cleanupShinyProcess(appState) {
  await ProcessManager.killProcess(appState.rShinyProcess, "ShinyProcess");
  appState.setShinyProcess(null);
}

// 5. Main retry loop for starting webserver
async function tryStartWebserver(
  attempt,
  progressCallback,
  onErrorStartup,
  onErrorLater,
  onSuccess
) {
  if (attempt > appState.config.maxRetryAttempts) {
    await progressCallback({ attempt, code: "failed" });
    await onErrorStartup();
    return;
  }

  if (appState.rShinyProcess !== null) {
    await onErrorStartup();
    return;
  }

  await progressCallback({ attempt, code: "start" });

  const started = await startShinyProcessWithState(appState);
  if (!started) {
    await progressCallback({ attempt, code: "process_failed" });
    return tryStartWebserver(
      attempt + 1,
      progressCallback,
      onErrorStartup,
      onErrorLater,
      onSuccess
    );
  }

  const serverReady = await waitForServerReady(appState, "exponential");
  if (serverReady) {
    await progressCallback({ attempt, code: "success" });
    onSuccess(appState.getServerUrl());
    return;
  }

  const retryDelay = getRetryDelay(attempt);
  await progressCallback({
    attempt,
    code: "notresponding",
    message: `Server check attempt ${attempt} failed. Next delay: ${retryDelay}ms`,
  });
  await cleanupShinyProcess(appState);

  await ServerUtils.waitFor(retryDelay);

  return tryStartWebserver(
    attempt + 1,
    progressCallback,
    onErrorStartup,
    onErrorLater,
    onSuccess
  );
}

// Squirrel startup handling with better error handling
// Handle Squirrel startup only on Windows
// const handleSquirrelStartup = () => {
//   if (os.platform() !== "win32") {
//     return false;
//   }
//   return ErrorHandler.handleSyncError(
//     "handleSquirrelStartup",
//     () => {
//       if (require("electron-squirrel-startup")) {
//         app.quit();
//         ProcessManager.terminateRProcesses();
//         return true;
//       }
//       return false;
//     },
//     false
//   );
// };

const handleSquirrelStartup = () => {
  if (os.platform() !== "win32") {
    return false;
  }
  return ErrorHandler.handleSyncError(
    "handleSquirrelStartup",
    () => {
      const squirrelCommand = process.argv[1];
      const appFolder = path.resolve(process.execPath, '..');
      const rootFolder = path.resolve(appFolder, '..');
      const updateExe = path.resolve(rootFolder, 'Update.exe');

      switch (squirrelCommand) {
        case '--squirrel-install':
        case '--squirrel-updated':
          spawn(updateExe, ['--createShortcut', path.basename(process.execPath)], { detached: true });
          app.quit();
          return true;
        case '--squirrel-uninstall':
          spawn(updateExe, ['--removeShortcut', path.basename(process.execPath)], { detached: true });
          app.quit();
          return true;
        case '--squirrel-obsolete':
          app.quit();
          return true;
      }
      return false;
    },
    false
  );
};



// Handle Squirrel events only on Windows
if (handleSquirrelStartup()) {
  // App is quitting due to squirrel event
  process.exit(0);
}

// Application bootstrap
app.on("ready", async () => {
  
  appState = new AppState();
  await appState.init(); // find a free port (in case of other running Shiny apps)

  // Clear electron cache (avoids loading app with cached styles from a previous version)
  await session.defaultSession.clearCache();

  // Set a content security policy
  session.defaultSession.webRequest.onHeadersReceived((_, callback) => {
    callback({
      responseHeaders: `
        default-src 'none';
        script-src 'self';
        img-src 'self' data:;
        style-src 'self';
        font-src 'self';
    `,
    });
  });

  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => {
    callback(false);
  });

  // Use WindowManager for window and splash screen creation
  const createWindow = (shinyUrl) =>
    WindowManager.createWindow(appState, shinyUrl);
  const createLoadingSplashScreen = () =>
    WindowManager.createLoadingSplashScreen(appState);
  const createErrorScreen = () => WindowManager.createErrorScreen(appState);

  createLoadingSplashScreen();

  // Emit events to the splash screen
  const emitSpashEvent = async (event, data) => {
    try {
      await appState.loadingSplashScreen.webContents.send(event, data);
    } catch (e) {
      console.error("Error emitting splash event:", e);
    }
  };

  // Callback for progress updates
  const progressCallback = async (event) => {
    await emitSpashEvent("start-webserver-event", event);

    if (event && event.message) {
      await emitSpashEvent("server-status-message", event.message);
    }
  };

  // Callback for errors after startup
  const onErrorLater = async () => {
    if (!appState.mainWindow) {
      return;
    }
    createErrorScreen();
    await appState.errorSplashScreen.show();
    appState.mainWindow.destroy();
  };

  // Callback for errors during startup
  const onErrorStartup = async () => {
    await ServerUtils.waitFor(appState.config.serverStartTimeout); // TODO: hack, only emit if the loading screen is ready
    await emitSpashEvent("failed");
  };

  // Check for updates while the splash screen is showing
  if (os.platform() === "win32" && app.isPackaged) {
    autoUpdater.checkForUpdates();
  }

  try {
    await tryStartWebserver(
      0,
      progressCallback,
      onErrorStartup,
      onErrorLater,
      (url) => {
        createWindow(url);
        appState.loadingSplashScreen.destroy();
        appState.setLoadingSplashScreen(null);
        appState.mainWindow.show();
      }
    );
  } catch (e) {
    console.error("Error starting webserver:", e);
    await emitSpashEvent("failed");
  }
});

app.on("window-all-closed", async () => {
  appState.setShutdown(true);

  await ProcessManager.terminateRProcesses();

  console.log("Shutting down...");
  await appState.cleanup();
  app.quit();
});
