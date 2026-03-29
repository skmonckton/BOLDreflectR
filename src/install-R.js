// scripts/install-R.js
import { execSync } from "child_process";
import { pathToFileURL } from "url";

const nhyrisRoot = execSync("npm root -g").toString().trim();
const nhyrisInstall = pathToFileURL(`${nhyrisRoot}/nhyris/utils/install.js`).href;
const { installStandaloneR, installDependencies } = await import(nhyrisInstall);

installStandaloneR(process.cwd());
installDependencies(process.cwd());