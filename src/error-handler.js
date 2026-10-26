const fs = require("fs");
const path = require("path");
const os = require("os");

// Error Handling System
class ErrorHandler {
  // folder for the app's log files (app.log, and R.log from process-manager.js); null until useLogFolder()
  static logFolder = null;

  // Each launch starts new logs; the last launch's are kept as *.previous.log
  static useLogFolder(folder) {
    try {
      fs.mkdirSync(folder, { recursive: true });
      for (const name of ["app", "R"]) {
        const log = path.join(folder, `${name}.log`);
        if (fs.existsSync(log)) fs.renameSync(log, path.join(folder, `${name}.previous.log`));
      }
      this.logFolder = folder;
    } catch (e) {
      console.error("Could not set up the log folder:", e.message);
    }
  }

  static logPath(name) {
    return this.logFolder ? path.join(this.logFolder, `${name}.log`) : null;
  }

  static logError(context, error, metadata = {}) {
    const errorInfo = {
      context,
      message: error.message,
      stack: error.stack,
      timestamp: new Date().toISOString(),
      ...metadata,
    };

    console.error(`[${context}] Error:`, errorInfo);
    const appLog = this.logPath("app");
    if (appLog) {
      try {
        fs.appendFileSync(appLog, JSON.stringify(errorInfo) + os.EOL);
      } catch {}
    }

    // TODO: Integrate with error reporting service in the future
    // this.reportToService(errorInfo);
  }

  static async handleAsyncError(context, asyncFn, fallbackValue = null) {
    try {
      return await asyncFn();
    } catch (error) {
      this.logError(context, error);
      return fallbackValue;
    }
  }

  static handleSyncError(context, syncFn, fallbackValue = null) {
    try {
      return syncFn();
    } catch (error) {
      this.logError(context, error);
      return fallbackValue;
    }
  }
}

module.exports = ErrorHandler;
