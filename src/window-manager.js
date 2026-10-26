const { BrowserWindow, dialog, app } = require("electron");
const { shell } = require('electron');
const ErrorHandler = require("./error-handler");
const path = require("path");
const fs = require("fs");

// Links that open without asking:
// - URLs/domains (each with its subdomains) listed in package.json under "nhyris": { "trustedLinkHosts": [...] }
// - web addresses written in the app's shiny files, plus descendants (but not the rest of its site)
let trustedLinks = null;
function getTrustedLinks() {
  if (trustedLinks) return trustedLinks;
  let hosts = [];
  try {
    const pkg = JSON.parse(fs.readFileSync(path.join(app.getAppPath(), "package.json"), "utf-8"));
    hosts = ((pkg.nhyris && pkg.nhyris.trustedLinkHosts) || []).map((h) => String(h).toLowerCase());
  } catch (e) {
    ErrorHandler.logError("getTrustedLinks", e);
  }
  const addresses = new Set();
  const scan = (dir, depth) => {
    let entries = [];
    try {
      entries = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      const p = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        // (the user data folder holds data, not the app's own addresses)
        if (depth < 5 && entry.name !== "user_data") scan(p, depth + 1);
      } else if (/\.(r|js|html|css|ya?ml|json|md)$/i.test(entry.name)) {
        try {
          for (const m of fs.readFileSync(p, "utf-8").matchAll(/https?:\/\/[^\s"'`<>()\\]+/gi)) {
            addresses.add(new URL(m[0].replace(/[.,;:]+$/, "")).href);
          }
        } catch {}
      }
    }
  };
  scan(path.join(app.getAppPath(), "shiny"), 0);
  trustedLinks = { hosts, addresses: [...addresses] };
  return trustedLinks;
}

function isTrustedLink(parsed) {
  const { hosts, addresses } = getTrustedLinks();
  const host = parsed.hostname.toLowerCase();
  if (hosts.some((h) => host === h || host.endsWith("." + h))) return true;
  // an address ending in "/", "?", "=", "&" or "#" is a base the app adds to (e.g. "...?processid=" + an ID)
  return addresses.some((a) => parsed.href === a || (/[/?=&#]$/.test(a) && parsed.href.startsWith(a)) ||
    ["/", "?", "#"].some((sep) => parsed.href.startsWith(a + sep)));
}

// Open links in the system's default application; only web and email links are passed on
// (other URLs - e.g. file:, custom protocol handlers - could launch programs if the page were ever made to open them)
// links to other sites are confirmed first, so script injected into the page couldn't quietly send data to a site
function openExternalSafe(url) {
  try {
    const parsed = new URL(url);
    if (["https:", "http:", "mailto:"].includes(parsed.protocol)) {
      const trusted = parsed.protocol !== "mailto:" && isTrustedLink(parsed);
      if (!trusted) {
        const choice = dialog.showMessageBoxSync(BrowserWindow.getFocusedWindow(), {
          type: "question",
          buttons: ["Open", "Cancel"],
          defaultId: 1,
          cancelId: 1,
          message: "Open this link outside the app?",
          detail: parsed.href.length > 300 ? parsed.href.slice(0, 300) + "..." : parsed.href,
        });
        if (choice !== 0) return;
      }
      shell.openExternal(parsed.href);
    } else {
      ErrorHandler.logError("openExternalSafe", new Error(`blocked link with protocol ${parsed.protocol}`));
    }
  } catch (e) {
    ErrorHandler.logError("openExternalSafe", e);
  }
}

// Create the main window for the Shiny app
function createWindow(appState, shinyUrl, bounds) {
  return ErrorHandler.handleSyncError("createWindow", () => {
    const mainWindow = new BrowserWindow({
      width:  bounds ? bounds.width  : appState.config.mainWindow.width,
      height: bounds ? bounds.height : appState.config.mainWindow.height,
      x:      bounds ? bounds.x      : undefined,
      y:      bounds ? bounds.y      : undefined,
      show: false,
      autoHideMenuBar: true,
      webPreferences: {
        preload: path.join(__dirname, "preload.js"),
        nodeIntegration: false,
        contextIsolation: true,
        sandbox: true,
        spellcheck: false
      },
    });

    mainWindow.loadURL(shinyUrl);

    mainWindow.webContents.setWindowOpenHandler(({ url }) => {
      openExternalSafe(url);
      return { action: 'deny' };
    });

    mainWindow.webContents.on('will-navigate', (event, url) => {
      if (url !== mainWindow.webContents.getURL()) {
        event.preventDefault();
        openExternalSafe(url);
        }
    });

    mainWindow.on("closed", () => {
      appState.setMainWindow(null);
    });

    mainWindow.on("unresponsive", () => {
      ErrorHandler.logError(
        "MainWindow",
        new Error("Window became unresponsive")
      );
    });

    appState.setMainWindow(mainWindow);
    return mainWindow;
  });
}

// Options for splash screens
function getSplashScreenOptions(appState) {
  return {
    width: appState.config.mainWindow.width,
    height: appState.config.mainWindow.height,
    backgroundColor: appState.config.backgroundColor,
  };
}

// Show loading splash screen
function createLoadingSplashScreen(appState) {
  const win = new BrowserWindow({
    width: appState.config.mainWindow.width,
    height: appState.config.mainWindow.height,
    backgroundColor: appState.config.backgroundColor,
    show: false,
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
    },
  });

  win.loadFile(path.join(__dirname, "loading.html"));
  win.once("ready-to-show", () => win.show());
  appState.setLoadingSplashScreen(win); // Use the setter method here
  return win;
}

// Show the error page (failed.html) in a new window, where the main window was
function createErrorScreen(appState, bounds) {
  const win = new BrowserWindow({
    ...getSplashScreenOptions(appState),
    ...(bounds || {}),
    show: false,
    autoHideMenuBar: true,
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
    },
  });
  showErrorPage(appState, win, "later");
  win.once("ready-to-show", () => win.show());
  appState.setErrorSplashScreen(win);
  return win;
}

// Load the error page into a window ("startup": R didn't start; "later": R stopped while the app was open)
function showErrorPage(appState, win, when) {
  win.loadFile(path.join(__dirname, "failed.html"), { query: { when, logs: appState.paths.logPath } });
}

module.exports = {
  openExternalSafe,
  createWindow,
  createLoadingSplashScreen,
  createErrorScreen,
  showErrorPage,
};
