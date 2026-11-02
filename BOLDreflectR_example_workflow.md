# BOLDreflectR Example Workflow (iBOL 2026)

This example uses COI-5P data from geometrid moths (Geometridae) from Thailand, working through the following steps:

1. Search and fetch/collect data from the BOLD data API or a local data package
2. Examine results and customize fields
3. Summarize, transform, and export data
4. Analyze data

## Installing BOLDreflectR

1. Go to the releases page: <https://github.com/skmonckton/BOLDreflectR/releases>
2. Under **Assets**, download the installer for your operating system:
   - **Windows**: `BOLDreflectR-1.0.0-rc1.Setup.exe`
   - **macOS**: `BOLDreflectR-1.0.0-rc1-arm64.dmg`
   - **Linux**: `BOLDreflectR-1.0.0-rc1-x64.AppImage`
3. Install the app, confirming when your OS warns you:
   - **Windows**: click **More info**, then **Run anyway**.
   - **macOS**: drag and drop BOLDreflectR to Applications; to override security settings and run the app, either follow Apple's [suggested method](https://support.apple.com/en-gb/guide/mac-help/mh40616/mac) or open the Terminal and run `xattr -d com.apple.quarantine /Applications/BOLDreflectR.app`.
   - **Linux**: run `chmod +x BOLDreflectR-1.0.0-rc1-x64.AppImage` (or right-click and select **Properties > Permissions > Allow executing file as a program**), then run the file.

*Note:* BOLDreflectR has not been thoroughly tested on macOS and Linux.

## Launching BOLDreflectR for the first time

Run the app and wait a moment or two for it to launch. The **Get data** panel on the left offers three methods:

| Method | Equivalent to |
|---|---|
| **Fetch** (API) | `BOLDconnectR::bold.fetch()` |
| **Search** (API) | `BOLDconnectR::bold.full.search()`, then `bold.fetch()` |
| **Local data** (BOLDNODE) | `BOLDNODE::bold_parquet_search()` and `bold_search_collect()` |

### Input your API key

1. Select **Fetch** or **Search**.
2. Enter your API key in the **BOLD API key** box.
3. With **Remember key** checked, the key will be saved to your system's credential store the next time you press **Get Data**, then retrieved when needed thereafter.

You can also save, update, or forget your key from the Settings panel, which can be accessed by clicking the gear icon at the top right of the sidebar.

### Import a BOLDNODE data package

1. Select **Local data** and click **Add/Edit...** to open the data package manager.
2. Click **Add...**, select the downloaded data package (`BOLD_Public.23-Oct-2026.parquet`), and click **Open**. The data package is copied to the app's data folder, so you can delete the downloaded copy afterwards.
3. Close the package manager. The package is now selected for local searches.

## 1. Data retrieval

1. Select **Search** or **Local data**.
2. Enter:
   - **Taxonomy**: Geometridae
   - **Geography**: Thailand
   - **Marker**: COI-5P
3. Click **Get data** (or press Ctrl + Enter in any search field). The results will appear in the main panel.

To get records from a list of process IDs, sample IDs, BINs, or dataset or project codes, use **Fetch** instead.

> **Note:** All results are loaded into memory, even when searching a data pacakge using BOLDNODE. The app asks for confirmation for results above 100,000 records, and refuses searches above the maximum specified in **Settings > Default options** (500,000 by default).

## 2. Examine results and customize appearance

### Overview and query hit report

The **Overview** tab shows:

- **Data source**: the data package or the BOLD API
- **Search results**: counts of specimens, sequences (per marker, with length range), BINs, species, and institutions
- **Search terms**: each term is marked **FOUND** or **NOT FOUND**, with its number of records. Useful for catching misspelled taxa or places.

### Customize the table

In the **Customize** panel:

- **Columns to show**: choose a default set, a preset (e.g. *Taxonomic identification*, *Collection data*), or individual fields. Drag to reorder selections. Click the bookmark icon to save the current combination as a column set.
- **Filter by marker**: show only rows for the chosen marker(s). Records with several markers otherwise have one row per marker.
- **Collapse multiple markers into single rows**: one row per specimen, with each marker in its own columns.
- **Reset**: return columns and marker filter to your defaults.

### Sort and filter the table

- Click a column header to sort; click again to reverse.
- Use **Search** above the table to search all columns.
- Use the boxes under the headers to filter single columns (numeric columns use a range slider).
- Click **Reset filters** to clear them.

Summaries, maps, exports and analyses all use the rows that match the current filters.

> **Additional BIN members**
>
> BOLDreflectR can retrieve the other members of the BINs in your results, either:
> - during a search, with **Find additional BIN members** checked, or
> - afterwards, by switching on **Include additional BIN members** in the **Customize** panel.
>
> The app reports how many members it found and asks before adding them. Local results are matched within the same data package; API results on BOLD. Click the icon next to the switch to review the list, and use the switch to include or exclude them.

## 3. Summarize, transform, and export data

### Summarize

1. Open the **Summarize** panel.
2. Under **Summarize by**, choose:
   - **Taxonomic summary**: a collapsible tree of record counts per taxon, phylum to species (**Expand all** / **Collapse all**)
   - any single column (e.g. **BIN**, **Identification**, **Country/Ocean**) to count its unique values
3. Click **Generate**. Results appear in the **Summary** tab.

### Distribution map

Under **Distribution map**, click **Generate**. The **Occurrence map** tab shows all filtered records with valid coordinates. *Note: Maps require an internet connection.*

### Copy, save, and open

Single columns can be copied from the table by mousing over the column header and clicking the copy icon. 

Data from the full table can be copied or saved using the buttons below:

| Button | Action |
|---|---|
| **Copy table** / **Copy FASTA** | Copy rows or sequences to the clipboard (FASTA headers can be set in Settings) |
| **Save FASTA**, **TSV**, **CSV**, **XLSX** | Save to a file |
| **Open XLSX** | Open in your spreadsheet app (a temporary, read-only copy: use **Save As** to keep it) |

> **Note:** These functions export the current tab's filtered rows.

### Export as a BOLDNODE data package

1. Click the database icon (left of **TSV**).
2. If the table is filtered, choose all or filtered records.
3. Enter an **ID/alias** and save. The package is now available as a **Local data** source (or for use with BOLDNODE in R).

## 4. Analyze data

Each analysis in the **Analyze** panel uses the filtered rows and opens its results in a new tab.

### Sequence alignment

1. On the **Data** tab, filter `genus` to `Abraxas` and `nuc_basecount` to 600–665.
2. Under **Sequence alignment**, keep **Marker** as COI-5P and click **Align**.

The alignment tab can be edited by hand to adjust gaps (e.g. Ctrl + Left/Right to slide residues into gaps; Ctrl + Z to undo) and exported with **Copy alignment** or **Save FASTA**.

### Tree (using current alignment)

1. Under **Neighbour-joining tree**, check **Use current alignment** (manual edits are included).
2. Optionally choose **Tip labels** and **Colour by** (e.g. BIN).
3. Click **Build tree**.

Labels and colours can still be changed on the **Tree** tab. You can export the tree in **PDF** or **Newick** format, and you can save the data in tree order using the data export options.

### Diversity profile

1. Click **Reset filters** above the table, to use all records.
2. Under **Diversity profile**, set:
   - **Analyze by**: BIN
   - **Assemblage type**: Locations
   - **Use as sites**: Provinces/States
3. Check **Compute as presence-absence matrix** and **Perform rarefaction for all sites**.
4. Click **Run**.

For details of each measure, click the info icon next to **Diversity profile**.

### BIN consensus

Under **Consensus BIN taxonomy**, keep the defaults and click **Run**. *Include BOLD Portal stats* adds distances and nearest neighbours, but can take several minutes.

### BIN representatives

Under **BIN representatives**, keep 1 per BIN and the default criteria, and click **Run**. To set your own criteria, switch off **Use default selection criteria** and drag them into order.

### Tree (using BIN reps)

1. Under **Neighbour-joining tree**, check **BIN representatives only** (shown once BIN reps exist).
2. Click **Build tree**.
