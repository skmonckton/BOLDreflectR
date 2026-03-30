const path = require("path");
const fs = require("fs");
const { name } = require("./package.json");

module.exports = {
  packagerConfig: {
    icon: "assets/icon",
    ignore: [
      /^\/dev/,
      /^\/bin/,
      /\.git/,
      /\.github/,
      /\.gitignore$/,
      /\.gitattributes$/,
      /^\/shiny\/dev/,
      /^\/shiny\/bin\/.*$/,
      /^\/shiny\/data\/user_config\.yaml$/,
      /\.Rproj\.user/,
      /\.Rproj$/,
      /\.RData$/,
      /\.Rhistory$/,
      /\.Rprofile$/
    ]
  },
  makers: [
    {
      name: "@electron-forge/maker-zip",
      platforms: ["win32"],
    },
    {
      name: "@electron-forge/maker-squirrel",
      platforms: ["win32"],
      config: {
        setupIcon: "assets/icon.ico",
        loadingGif: "assets/install-spinner.gif"
      }
    },
    {
      name: "@electron-forge/maker-dmg",
      platforms: ["darwin"],
      config: {
        icon: "assets/icon.icns",
        overwrite: true
      }
    },
    {
      name: "@reforged/maker-appimage",
      platforms: ["linux"],
        config: {
          options: {
            bin: name,
            icon: "assets/icon.png"
          }
        }
    }
  ],
  hooks: {
    postMake: async (config, makeResults) => {
      const fs = require("fs");
      const path = require("path");
      const outDir = path.join(__dirname, "out");
      const makeDir = path.join(outDir, "make");

      // Remove intermediate package dirs (e.g. out/app-win32-x64/)
      for (const entry of fs.readdirSync(outDir)) {
        if (entry === "make") continue;
        const fullPath = path.join(outDir, entry);
        if (fs.statSync(fullPath).isDirectory()) {
          fs.rmSync(fullPath, { recursive: true, force: true });
          console.log(`Cleaned up package dir: ${entry}`);
        }
      }

      // Flatten make/ by hoisting all files up, zip artifacts go into make/zip/
      const zipDir = path.join(makeDir, "zip");
      fs.mkdirSync(zipDir, { recursive: true });

      const hoistDir = (dir) => {
        for (const entry of fs.readdirSync(dir)) {
          const fullPath = path.join(dir, entry);
          if (fs.statSync(fullPath).isDirectory()) {
            hoistDir(fullPath);
          } else {
            const renamedEntry = entry.replace("-darwin-", "-macOS-");
            const isZip = renamedEntry.endsWith(".zip");
            const dest = path.join(isZip ? zipDir : makeDir, renamedEntry);
            fs.renameSync(fullPath, dest);
          }
        }
        const rel = path.relative(makeDir, dir);
        if (rel !== "" && rel !== "zip") {
          fs.rmdirSync(dir);
        }
      };

      hoistDir(makeDir);
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
