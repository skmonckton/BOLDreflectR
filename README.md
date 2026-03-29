<img src="shiny/www/reflectR-name.png" alt="BOLDreflectR" height="60px" style="display:block; margin:auto;">

A graphical user interface for [BOLDconnectR](https://doi.org/10.32614/CRAN.package.BOLDconnectR) allowing basic exploration and analysis of data from BOLD.

**BOLDreflectR** is a Shiny app built in R, and packaged as a desktop application using a [fork](https://github.com/skmonckton/nhyris) of [nhyris](https://github.com/jahnen/nhyris). This repository includes all the necessary files to build the standalone application using the modified nhyris code (i.e., while excluding the bundled R installation and Node.js packages used by the Electron framework that powers the desktop app).

Bundled executables are available from the [releases](https://github.com/skmonckton/BOLDreflectR/releases/latest) page. Executables are automatically built using GitHub Actions with each new version. The app is developed and tested on Windows 11; other platforms are more-or-less untested and may therefore have issues (please report any you encounter).

The R code can also be downloaded from the [Shiny sub-directory](https://github.com/skmonckton/BOLDreflectR/tree/main/shiny), should you wish to run the app using RStudio.
