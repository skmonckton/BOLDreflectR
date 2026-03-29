const path = require("path");
const net = require("net");
const { app } = require("electron");
const ProcessManager = require("./process-manager");

class AppState {
  constructor() {
    this.shutdown = false;
    this.rShinyProcess = null;
    this.mainWindow = null;
    this.loadingSplashScreen = null;
    this.errorSplashScreen = null;

    // Configuration
    let rPath = "r-nhyris";

    this.config = {
      rPath: rPath,
      backgroundColor: "#cccccc",
      serverPort: 1124,
      maxRetryAttempts: 100,
      serverCheckTimeout: 3000,
      serverStartTimeout: 10000,
      mainWindow: {
        width: 1600,
        height: 900,
      },
    };

    // Computed paths
    this.paths = {
      rpath: path.join(app.getAppPath(), this.config.rPath),
      get libPath() {
        return path.join(this.rpath, "library");
      },
      get rscript() {
        return path.join(this.rpath, "bin", "R");
      },
      shinyAppPath: path.join(app.getAppPath(), "shiny"),
      userDataPath: path.join(app.getPath('userData')),
    };
  }

  async init() {
    this.config.serverPort = await this._findFreePort(this.config.serverPort);
  }

  _findFreePort(startPort) {
    return new Promise((resolve, reject) => {
      const server = net.createServer();
      server.listen(startPort, '127.0.0.1', () => {
        const port = server.address().port;
        server.close(() => resolve(port));
      });
      server.on('error', () => {
        resolve(this._findFreePort(startPort + 1));
      });
    });
  }

  setShinyProcess(process) {
    this.rShinyProcess = process;
  }

  setMainWindow(window) {
    this.mainWindow = window;
  }

  setLoadingSplashScreen(screen) {
    this.loadingSplashScreen = screen;
  }

  setErrorSplashScreen(screen) {
    this.errorSplashScreen = screen;
  }

  setShutdown(value) {
    this.shutdown = value;
  }

  getServerUrl() {
    return `http://127.0.0.1:${this.config.serverPort}`;
  }

  async cleanup() {
    this.shutdown = true;

    if (this.rShinyProcess) {
      await ProcessManager.killProcess(this.rShinyProcess, "ShinyProcess");
      this.rShinyProcess = null;
    }

    // Clean up windows
    if (this.loadingSplashScreen && !this.loadingSplashScreen.isDestroyed()) {
      this.loadingSplashScreen.destroy();
    }

    if (this.errorSplashScreen && !this.errorSplashScreen.isDestroyed()) {
      this.errorSplashScreen.destroy();
    }
  }
}

module.exports = AppState;
