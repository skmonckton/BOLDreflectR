const { app } = require("electron");
const { spawn, execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");
const ErrorHandler = require("./error-handler");

// Environment variables passed through from the user's session to R. The rest of the environment is withheld so
// the bundled R stays isolated from any other R installation (R_LIBS, R_HOME, etc.), but these are needed for the
// home/temp folders, the locale, and (on Linux) the desktop session used by the keyring's Secret Service backend.
const PASSTHROUGH_ENV = [
  // home, locale, and time zone
  "HOME", "LANG", "LC_ALL", "LC_CTYPE", "TZ",
  // temporary folders
  "TMPDIR", "TMP", "TEMP",
  // Linux desktop session
  "DISPLAY", "WAYLAND_DISPLAY", "XAUTHORITY", "XDG_RUNTIME_DIR", "XDG_CONFIG_HOME", "XDG_DATA_HOME",
  "DBUS_SESSION_BUS_ADDRESS",
  // Windows system and user folders
  "SystemRoot", "WINDIR", "USERPROFILE", "APPDATA", "LOCALAPPDATA", "HOMEDRIVE", "HOMEPATH",
];

function passthroughEnv() {
  const env = {};
  for (const name of PASSTHROUGH_ENV) {
    if (process.env[name] !== undefined) {
      env[name] = process.env[name];
    }
  }
  // apps launched from the macOS Finder or some Linux launchers have no locale set, which would leave R in the
  // C locale (non-ASCII text can then be mangled); fall back to a UTF-8 locale that each platform provides
  if (os.platform() !== "win32" && !env.LANG && !env.LC_ALL) {
    env.LANG = os.platform() === "darwin" ? "en_US.UTF-8" : "C.UTF-8";
  }
  return env;
}

// Process Management with better error handling
class ProcessManager {
  static async startShinyProcess(appState) {
    return new Promise((resolve, reject) => {
      let settled = false;
      // R's output goes to R.log in the log folder (see ErrorHandler.useLogFolder), so failures can be diagnosed
      const rLog = ErrorHandler.logPath("R");
      let logFd = "ignore";
      try {
        if (rLog) {
          logFd = fs.openSync(rLog, "a");
          fs.writeSync(logFd, `--- R started ${new Date().toISOString()}
`);
        }
      } catch (e) {
        ErrorHandler.logError("ProcessManager.startShinyProcess", e);
        logFd = "ignore";
      }

      const rShinyProcess = spawn(
        appState.paths.rscript,
        [
          "--vanilla",
          "-f",
          path.join(app.getAppPath(), "src", "start-shiny.R"),
        ],
        {
          env: {
            ...passthroughEnv(),
            WITHIN_ELECTRON: "1",
            RE_SHINY_PATH: appState.paths.shinyAppPath,
            R_LIB_PATHS: appState.paths.libPath,
            R_HOME_DIR: appState.paths.rpath,
            RHOME: appState.paths.rpath,
            RE_SHINY_PORT: appState.config.serverPort,
            R_LIBS: appState.paths.libPath,
            R_LIBS_USER: appState.paths.libPath,
            R_LIBS_SITE: appState.paths.libPath,
            USER_DATA_PATH: appState.paths.userDataPath,
            BR_LAUNCH_TOKEN: appState.launchToken,
            APP_VER: app.getVersion()
          },
          stdio: ["ignore", logFd, logFd],
        }
      );
      // R has its own copy of the log file handle
      if (typeof logFd === "number") fs.closeSync(logFd);

      rShinyProcess.on("error", (err) => {
        if (!settled) {
          settled = true;
          ErrorHandler.logError("ProcessManager.startShinyProcess", err, {
            pid: rShinyProcess.pid,
            command: appState.paths.rscript,
          });
          reject(err);
        }
      });

      rShinyProcess.on("exit", (code, signal) => {
        if (!settled) {
          settled = true;
          const exitInfo = { code, signal, pid: rShinyProcess.pid };
          if (code === 0) {
            console.log("Shiny process exited normally");
            resolve(rShinyProcess);
          } else {
            const error = new Error(`Shiny process exited with code ${code}`);
            ErrorHandler.logError(
              "ProcessManager.startShinyProcess",
              error,
              exitInfo
            );
            reject(error);
          }
        }
      });

      // Resolve after successful start (but only if not already settled)
      setTimeout(() => {
        if (!settled) {
          settled = true;
          resolve(rShinyProcess);
        }
      }, 100);
    });
  }

  static async killProcess(process, processName = "process") {
    if (!process) {
      return;
    }

    return ErrorHandler.handleAsyncError(
      `ProcessManager.killProcess.${processName}`,
      async () => {
        ProcessManager.killProcessTree(process);
        console.log(`${processName} killed successfully`);
      }
    );
  }

  // Stops R and the processes it started. Synchronous, so it can run while the app quits.
  static killProcessTree(child) {
    if (!child || child.exitCode !== null) return;
    try {
      if (os.platform() === "win32") {
        execSync(`taskkill /PID ${child.pid} /T /F`, { stdio: "ignore" });
      } else {
        child.kill();
      }
    } catch (e) {
      ErrorHandler.logError("ProcessManager.killProcessTree", e);
    }
  }
}

module.exports = ProcessManager;
