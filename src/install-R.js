// scripts/install-R.js
import { execSync } from "child_process";

const nhyrisRoot = execSync("npm root -g").toString().trim();
const { installStandaloneR, installDependencies } = await import(`${nhyrisRoot}/nhyris/utils/install.js`);

installStandaloneR(process.cwd());
installDependencies(process.cwd());