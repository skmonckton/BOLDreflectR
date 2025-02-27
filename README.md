# BOLDreflectR

BOLDreflectR is a Shiny app, built to serve as an interactive GUI for the BOLD R package BOLDconnectR. For now, it is intended to support the day-to-day activities of CBG staff.

## To install the app on your machine:

A setup file is available as a release from this repo (currently Windows only).

## To run the from R or R Studio:

The R scripts and supporting files are available in the [`shiny`](https://github.com/skmonckton/BOLDreflectR/tree/main/shiny) directory of this repo.

## To build the executable:

The standalone version of the app is made with Electon Forge using the following template: [/zarathucorp/shiny-electron-template-windows](https://github.com/zarathucorp/shiny-electron-template-windows). The modified step-by-step instructions used to build the app are provided below. One change to the template protocol was particularly necessary to get this working (original **Step 15** and related footnote '**Add not-CRAN packages**'): for BOLDconnectR, rather than manually copying the package from the system's R library, it is necessary to install the package from within R in order to install all required dependencies. This is covered in **steps 8-9** below.

### A. Environment setup

1. Install **R**, **Rstudio**.

2. Install **Node.js**: from <a href = "https://nodejs.org/en/download/package-manager" target = "_blank"> offical page</a> (run in PowerShell as administrator):
    ```
    # installs fnm (Fast Node Manager)
    winget install Schniz.fnm

    # configure fnm environment
    fnm env --use-on-cd | Out-String | Invoke-Expression
    
    # download and install Node.js
    fnm use --install-if-missing 22

    # verifies the right Node.js version is in the environment
    node -v # should print `v22.11.0`

    # verifies the right npm version is in the environment
    npm -v # should print `10.9.0`
    ```

3. Install **Electron Forge** using `npm`:
    `npm i -g @electron-forge/cli`

4. Install **Innoextract** from <a href='https://constexpr.org/innoextract/' target ='_blank'>this link</a> (Windows Only) and move it to program files and add Environment variable PATH.

5. Install the `automagic` package in R: `install.packages("automagic")`.

### B. Setup R 

6. Using RStudio's terminal, navigate to the project directory (`path/to/BOLDreflectR`) and install R with `sh ./get-r-win.sh`
    **NOTE**: Ensure the R version listed in the script matches your machine's installed R version.

7. Also in the terminal, run `Rscript add-cran-binary-pkgs.R` to get packages for R.

8. In the R console, set the library path for the R session to the app's library: `.libPaths('path/to/myapp/r-win/library')`.

9. Install BOLDconnectR using `install.packages("devtools")`, `devtools::install_github("https://github.com/boldsystems-central/BOLDconnectR")`, then optionally: `remove.packages("devtools")`.

### C. Build executable

> [!NOTE]
> If returning to this step later, you will need to return to the fnm enviroment set up earlier, in a PowerShell terminal:
> `fnm env --use-on-cd | Out-String | Invoke-Expression`

10. Back in PowerShell, test that the application works using `electron-forge start`.

11. If the app runs successfully, it can be built using `electron-forge make`. The app will be placed in the **/out** folder. The ZIP archive can be found in **/out/make/zip/win32/x64**.

12. Optionally, you can package the app into a Windows installer using [Inno Setup](https://jrsoftware.org/isinfo.php).
