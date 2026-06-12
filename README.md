<img src="shiny/www/reflectR-name.png" alt="BOLDreflectR" height="60px" style="display:block; margin:auto;">

A graphical user interface for [BOLDconnectR](https://doi.org/10.32614/CRAN.package.BOLDconnectR) allowing basic exploration and analysis of data from BOLD.

**BOLDreflectR** is a Shiny app built in R, and packaged as a desktop application using a [fork](https://github.com/skmonckton/nhyris) of [nhyris](https://github.com/jahnen/nhyris). This repository includes all the necessary files to build the standalone application using the modified nhyris code (i.e., while excluding the bundled R installation and Node.js packages used by the Electron framework that powers the desktop app).

Bundled executables are available from the [releases](https://github.com/skmonckton/BOLDreflectR/releases/latest) page. Executables are automatically built using GitHub Actions with each new version. The app is developed and tested on Windows 11; other platforms are more-or-less untested and may therefore have issues (please report any you encounter).

The R code can also be downloaded from the [Shiny sub-directory](https://github.com/skmonckton/BOLDreflectR/tree/main/shiny), should you wish to run the app using RStudio.

### Highlights
**Rapid data retrieval & inspection:**
* Fetch records by process ID, sample ID, BIN, or project/dataset code
* Search BOLD by taxonomy, geography, marker, etc.
* Get a hit/miss report for query terms
* Find additional BIN members for all retrieved records

**Explore and anlayze results:**
* Customize visible columns and pre-filter results in-app
* Generate simple summaries for any field
* Compute consensus BIN taxonomy
* Retrieve BIN statistics from BOLD Portal (e.g. pairwise divergence, nearest neighbours)
* Plot interactive occurrence maps (which update in real-time)

**Convenience features:**
* Copy FASTA-formatted sequences to clipboard
* Copy entire, single columns
* Save results as TSV, CSV, or XLSX
* Open results directly in Excel
* Parsed columns include separate lat/long, identification date, project code

---

>**VERSIONING NOTE**:
>
> BOLDreflectR's version history will be adjusted on or before July 15th, 2026, with the release of "v1.3.4" revised to v0.9.4. Earlier versions are retroactively revised as such:
>
>| Original | Revised |
>| :------: | :-----: |
>| 1.3.x | 0.9.x |
>| 1.2.x* | 0.3.x |
>| 1.1.x* | 0.2.x |
>| 1.0.x* | 0.1.x |
>
>_\* BOLDreflectR was refactored and redeployed via nhyris for the original "v1.3.0" (revised to v0.9.0); earlier versions are not documented in this repository._
