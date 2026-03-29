const path = require("path");
const fs = require("fs");

module.exports = {
  packagerConfig: {
    icon: "icon",
    ignore: [
      /^\/dev/,
      /^\/bin/,
      /^\/shiny\/\.Rproj\.user/,
      /^\/shiny\/\.git/,
      /^\/shiny\/\.gitignore$/,
      /^\/shiny\/\.gitattributes/,
      /^\/shiny\/LICENSE/,
      /^\/shiny\/data\/user_config\.yaml/,
      /^\/shiny\/bin\/.*$/,
      /\.Rproj$/,
      /\.RData$/,
      /\.Rhistory$/,
      /\.Rprofile$/
    ]
  },
  makers: [
    {
      name: "@electron-forge/maker-zip"
    },
    {
      name: "@electron-forge/maker-squirrel",
      config: {
        setupIcon: "icon.ico"
      }
    }
  ],
  hooks: {
    postMake: async (config, makeResults) => {
      const fs = require("fs");
      const path = require("path");
  
      // Remove all package output dirs (e.g. out/app-win32-x64/)
      const outDir = path.join(__dirname, "out");
      for (const entry of fs.readdirSync(outDir)) {
        const fullPath = path.join(outDir, entry);
        // Skip the make/ subdirectory — that's your artifacts
        if (entry === "make") continue;
        if (fs.statSync(fullPath).isDirectory()) {
          fs.rmSync(fullPath, { recursive: true, force: true });
          console.log(`Cleaned up package dir: ${entry}`);
        }
      }
    },
   packageAfterCopy: async (config, buildPath, electronVersion, platform, arch) => {
      // Map Forge's platform string to your folder names
      const platformMap = {
        darwin: "mac",
        linux:  "linux",
        win32:  "win"
      };

      const osFolderName = platformMap[platform];
      if (!osFolderName) throw new Error(`Unknown platform: ${platform}`);

      const srcDir  = path.join(__dirname, "bin", osFolderName);
      const destDir = path.join(buildPath, "shiny", "bin");

      fs.mkdirSync(destDir, { recursive: true });

      for (const file of fs.readdirSync(srcDir)) {
        const srcFile  = path.join(srcDir, file);
        const destFile = path.join(destDir, file);
        fs.copyFileSync(srcFile, destFile);

        // Preserve executable permissions on mac/linux
        if (platform !== "win32") {
          fs.chmodSync(destFile, 0o755);
        }
      }
    }
  }
};