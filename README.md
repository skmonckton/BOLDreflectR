# BOLDreflectR

A GUI for BOLDconnectR, built using the following template: [https://github.com/zarathucorp/shiny-electron-template-windows](/zarathucorp/shiny-electron-template-windows)

This repository excludes any files that are created during the setup process, which includes the packaged-in R.

One modification to the protocol was necessary to get it working, at **Step 15** (and related footnote '**Add not-CRAN packages**'): for BOLDconnectR, rather than manually copying the package from the system's R library, it is necessary to install the package from within R. To do so:
1. In RStudio, set the library path for the R session to the app's library: `.libPaths('path/to/myapp/r-win/library')`.
2. Install BOLDconnectR using `install.packages("devtools")`, `devtools::install_github("https://github.com/boldsystems-central/BOLDconnectR")`, then optionally: `remove.packages("devtools")`.

This should install any dependencies that BOLDconnectR requires, which would otherwise not be achieved via manyally copying the package.
