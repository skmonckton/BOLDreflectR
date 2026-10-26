// Original code: Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn
// Additions: Copyright (c) 2025 Jinhwan Kim
// Further modifications by Spencer K. Monckton

const { app, session, autoUpdater, dialog, shell, ipcMain, BrowserWindow, clipboard } = require("electron");
const { spawn } = require('child_process');
const fs = require("fs");
const path = require('path');
const os = require("os");
const ErrorHandler = require("./error-handler");
const ProcessManager = require("./process-manager"); // <-- import ProcessManager
const ServerUtils = require("./server-utils"); // 추가
const AppState = require("./app-state");
const WindowManager = require("./window-manager");
// GitHub owner and repository for update checks, from package.json's "repository" (a URL, { "url": ... } or
// "github:owner/repo"); update checks are skipped when there's none
const pkg = require("../package.json");
const repoUrl = typeof pkg.repository === "string" ? pkg.repository : (pkg.repository && pkg.repository.url) || "";
const [gitOwner, gitRepo] = (repoUrl.match(/(?:github\.com[/:]|^github:)([^/]+)\/([^/#?]+?)(?:\.git)?\/?$/) || []).slice(1);

// Windows matches the Start menu shortcut that Squirrel creates to the running app by this ID; without it, the
// shortcut can show a generic icon and the app can get a separate taskbar entry
if (os.platform() === "win32") {
  app.setAppUserModelId(`com.squirrel.${pkg.name}.${path.basename(process.execPath, ".exe")}`);
}
const contextMenu = require('electron-context-menu');

// Initialize global state
let appState;

// The page bridge (preload.js) only answers the app's own pages: the R server's, and the loading and error pages
function fromAppPage(event) {
  const url = (event.senderFrame && event.senderFrame.url) || "";
  return url.startsWith("file://") || (!!appState && url.startsWith(appState.getServerUrl() + "/"));
}

// Native file picker for the Shiny app (attached to the requesting window, so it opens in the foreground)
ipcMain.handle("dialog:openFile", async (event, options = {}) => {
  if (!fromAppPage(event)) return null;
  const win = BrowserWindow.fromWebContents(event.sender);
  const result = await dialog.showOpenDialog(win, {
    title: typeof options.title === "string" ? options.title : undefined,
    properties: ["openFile"],
    filters: Array.isArray(options.filters) ? options.filters : [],
  });
  return result.canceled || result.filePaths.length === 0 ? null : result.filePaths[0];
});

// Open a file or folder with the default application; opening from the (foreground) electron process also brings
// the new window to the front. Only paths inside the app's user data folder are allowed.
ipcMain.handle("shell:openPath", async (event, targetPath) => {
  if (!fromAppPage(event)) return "This location cannot be opened from the app.";
  const normalize = (p) => {
    const resolved = path.resolve(String(p));
    return os.platform() === "win32" ? resolved.toLowerCase() : resolved;
  };
  const target = normalize(targetPath);
  const allowed = normalize(app.getPath("userData"));
  if (target !== allowed && !target.startsWith(allowed + path.sep)) {
    return "This location cannot be opened from the app.";
  }
  // (a missing file would otherwise bring up a Windows error box, and the call would wait until it was closed)
  if (!fs.existsSync(path.resolve(String(targetPath)))) {
    return "The file or folder no longer exists.";
  }
  return shell.openPath(path.resolve(String(targetPath)));
});

// Copy text to the system clipboard for the Shiny app (works on all platforms without external tools, and isn't
// affected by the permission handler below, which denies the renderer's own clipboard permission requests)
ipcMain.handle("clipboard:writeText", async (event, text) => {
  if (!fromAppPage(event)) return false;
  clipboard.writeText(String(text));
  return true;
});

// Squirrel updates need the installed app (Update.exe beside the app's folder); other builds (e.g. the zip, macOS,
// Linux) check GitHub's latest release instead
const squirrelInstalled = os.platform() === "win32" && fs.existsSync(path.resolve(process.execPath, "..", "..", "Update.exe"));

// Auto-updater setup (installed Windows app only, skip in development)
if (squirrelInstalled && app.isPackaged && gitRepo) {
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

// Compares versions such as 1.2.0, 1.2.0-beta and 1.2.0+1; returns > 0 when a is newer than b
// (build metadata after "+" is ignored, and a pre-release comes before its release)
function compareVersions(a, b) {
  const parse = (v) => {
    const [core, pre = ""] = String(v).replace(/^v/, "").split("+")[0].split(/-(.*)/s);
    return { nums: core.split(".").map((n) => parseInt(n, 10) || 0), pre };
  };
  const x = parse(a);
  const y = parse(b);
  for (let i = 0; i < Math.max(x.nums.length, y.nums.length); i++) {
    const d = (x.nums[i] || 0) - (y.nums[i] || 0);
    if (d !== 0) return d;
  }
  if (x.pre === y.pre) return 0;
  if (!x.pre) return 1;
  if (!y.pre) return -1;
  return x.pre.localeCompare(y.pre, undefined, { numeric: true });
}

// 0. Check for updates (when autoUpdater is unavailable)
async function notifyIfUpdates() {
  try {
    const response = await fetch(
      `https://api.github.com/repos/${gitOwner}/${gitRepo}/releases/latest`
    );
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const release = await response.json();
    const latestVersion = release.tag_name.replace(/^v/, "");
    const currentVersion = app.getVersion();

    if (compareVersions(latestVersion, currentVersion) > 0) {
      const { response: buttonIndex } = await dialog.showMessageBox({
        type: "info",
        title: "Update Available",
        message: `A new version of ${app.getName()} is available: ${latestVersion}`,
        detail: `You are running version ${currentVersion}. Would you like to download the update?`,
        buttons: ["View Release", "Later"]
      });

      if (buttonIndex === 0) {
        WindowManager.openExternalSafe(release.html_url);
      }
    }
  } catch (err) {
    console.error("Update check failed:", err.message);
  }
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
  // one of Shiny's own files: asking for the app's page would make R build the whole page for each check
  const url = appState.getServerUrl() + "/shared/shiny.min.js";
  const statusCallback = (msg) => {
    if (appState.loadingSplashScreen) {
      appState.loadingSplashScreen.webContents.send(
        "server-status-message",
        msg
      );
    }
  };
  // stop waiting if R exits
  const rExited = () => {
    const p = appState.rShinyProcess;
    return !!p && (p.exitCode !== null || p.signalCode !== null);
  };
  return await ServerUtils.waitForServerSmart(url, strategy, statusCallback, rExited);
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

// Squirrel startup handling (handle only on Windows)
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

// One copy of the app at a time (two would start two R servers that write the same settings); launching it again
// brings the open window to the front
const gotSingleInstanceLock = app.requestSingleInstanceLock();
if (!gotSingleInstanceLock) {
  app.quit();
} else {
  app.on("second-instance", () => {
    const win = appState && [appState.mainWindow, appState.loadingSplashScreen, appState.errorSplashScreen]
      .find((w) => w && !w.isDestroyed());
    if (win) {
      if (win.isMinimized()) win.restore();
      win.focus();
    }
  });
}

// R runs as a separate process, which isn't stopped with the app (on Windows, child processes outlive their parent)
// stop R when the app quits in any manner
app.on("window-all-closed", () => app.quit());
app.on("will-quit", () => {
  if (appState) {
    appState.setShutdown(true);
    ProcessManager.killProcessTree(appState.rShinyProcess);
  }
});

// Application bootstrap
app.on("ready", async () => {
  if (!gotSingleInstanceLock) return;

  appState = new AppState();
  ErrorHandler.useLogFolder(appState.paths.logPath);
  await appState.init(); // find a free port (in case of other running Shiny apps)

  // Clear electron cache (avoids loading app with cached styles from a previous version)
  const cacheClearing = session.defaultSession.clearCache();

  // only the app's own windows can use the R server: their requests to it carry the per-launch secret
  // (other pages in the user's browser can reach 127.0.0.1 too; server.R closes sessions without the secret)
  const port = appState.config.serverPort;

  // Content security policy for the app's pages. Shiny and its widgets need inline scripts and eval, so this
  // doesn't stop injected script from running; it limits what such script can reach: the page can only connect to
  // the R server and load images from it and the map tile servers.
  const csp = [
    "default-src 'self'",
    "script-src 'self' 'unsafe-inline' 'unsafe-eval'",
    "style-src 'self' 'unsafe-inline'",
    "img-src 'self' data: blob: https://tile.openstreetmap.org https://*.tile.openstreetmap.org " +
      "https://tiles.stadiamaps.com https://server.arcgisonline.com",
    "font-src 'self' data:",
    `connect-src 'self' ws://127.0.0.1:${port}`,
    "object-src 'none'",
    "base-uri 'self'",
    "form-action 'self'",
    "frame-ancestors 'none'",
  ].join("; ");
  session.defaultSession.webRequest.onHeadersReceived(
    { urls: [`http://127.0.0.1:${port}/*`] },
    (details, callback) => {
      // DNS prefetching is off to prevent injected script from leaking data through DNS lookups (not covered by security policy)
      callback({ responseHeaders: { ...details.responseHeaders, "Content-Security-Policy": [csp], "X-DNS-Prefetch-Control": ["off"] } });
    }
  );

  session.defaultSession.webRequest.onBeforeSendHeaders(
    { urls: [`http://127.0.0.1:${port}/*`, `ws://127.0.0.1:${port}/*`] },
    (details, callback) => {
      details.requestHeaders["X-BR-Token"] = appState.launchToken;
      callback({ requestHeaders: details.requestHeaders });
    }
  );

  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => {
    callback(false);
  });

  // Save dialogs for downloads: a title and the Downloads folder
  session.defaultSession.on("will-download", (_event, item) => {
    item.setSaveDialogOptions({
      title: "Save file",
      defaultPath: path.join(app.getPath("downloads"), item.getFilename()),
    });
  });

  // Initialize the context menu (for additional options, see: https://github.com/sindresorhus/electron-context-menu)
  contextMenu.default({
      showSearchWithGoogle: false,
      showSelectAll: false,
      showCopyImage: true,
      showSaveImageAs: true,
      showInspectElement: false,
      showCopyLink: false, 
      // Custom actions
      prepend: (defaultActions, parameters) => [
        // There is a known issue with electron-context-menu's 'Copy Link' action - this resolves it (comment out to disable)
        {
          label: 'Copy Link',
          visible: parameters.linkURL.length > 0,
          click: () => {
            require('electron').clipboard.writeText(parameters.linkURL);
          }
        },
        // Option to copy link text
        {
          label: 'Copy Link Text',
          visible: !!(parameters.linkText && parameters.linkText.length > 0),
          click: () => {
            require('electron').clipboard.writeText(parameters.linkText);
          }
        }
      ]
  });

  // Use WindowManager for window and splash screen creation
  const createWindow = (shinyUrl, bounds) =>
    WindowManager.createWindow(appState, shinyUrl, bounds);
  const createLoadingSplashScreen = () =>
    WindowManager.createLoadingSplashScreen(appState);
  const createErrorScreen = (bounds) => WindowManager.createErrorScreen(appState, bounds);

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

  // Callback for errors after startup (R stopped while the app is open): the error page replaces the main window
  const onErrorLater = async () => {
    if (!appState.mainWindow || appState.mainWindow.isDestroyed()) {
      return;
    }
    ErrorHandler.logError("onErrorLater", new Error("R stopped while the app was open"));
    createErrorScreen(appState.mainWindow.getBounds());
    appState.mainWindow.destroy();
  };

  // Callback for errors during startup: the error page replaces the loading screen
  const onErrorStartup = async () => {
    ErrorHandler.logError("onErrorStartup", new Error("R did not start"));
    if (appState.loadingSplashScreen && !appState.loadingSplashScreen.isDestroyed()) {
      WindowManager.showErrorPage(appState, appState.loadingSplashScreen, "startup");
    }
  };

  // Check for updates while the splash screen is showing
  if (squirrelInstalled && app.isPackaged && gitRepo) {
    autoUpdater.checkForUpdates();
  } else if (app.isPackaged && gitRepo) {
    notifyIfUpdates();
  }

  await cacheClearing;
  try {
    await tryStartWebserver(
      0,
      progressCallback,
      onErrorStartup,
      onErrorLater,
      (url) => {
        const splashBounds = appState.loadingSplashScreen.getBounds();
        createWindow(url, splashBounds);
        appState.loadingSplashScreen.destroy();
        appState.setLoadingSplashScreen(null);
        appState.mainWindow.show();
        // from now on, R stopping is an error (when the app quits, appState.shutdown is set before R is stopped)
        appState.rShinyProcess.once("exit", () => {
          if (!appState.shutdown) onErrorLater();
        });
      }
    );
  } catch (e) {
    ErrorHandler.logError("tryStartWebserver", e);
    await onErrorStartup();
  }
});
