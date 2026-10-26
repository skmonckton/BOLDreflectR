const { contextBridge, ipcRenderer } = require("electron");

// Expose a safe API to the renderer
contextBridge.exposeInMainWorld("electronAPI", {
  onServerStatus: (callback) =>
    ipcRenderer.on("server-status-message", (event, message) =>
      callback(message)
    ),
  // startup progress for the loading screen ({ attempt, code })
  onStartupEvent: (callback) =>
    ipcRenderer.on("start-webserver-event", (event, data) => callback(data)),
  // native file picker (resolves to the selected path, or null if cancelled)
  openFile: (options) => ipcRenderer.invoke("dialog:openFile", options),
  // open a file or folder with the system default application (resolves to "" on success, or an error message)
  openPath: (targetPath) => ipcRenderer.invoke("shell:openPath", targetPath),
  // copy text to the system clipboard (resolves to true on success)
  copyText: (text) => ipcRenderer.invoke("clipboard:writeText", text),
});
