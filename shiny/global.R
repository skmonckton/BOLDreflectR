library(data.table)
library(keyring)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(sortable)
library(yaml)

#' Packages loaded or lazy-loaded elsewhere:
#'  BOLDconnectR
#'  BOLDNODE
#'  iNEXT
#'  leaflet
#'  lubridate
#'  pbapply
#'  plotly
#'  vegan
#'  writexl

source("R/tctools.R")
source("R/alignment.R")
source("R/tree.R")

# files in www/ get modification time appended to ensure the latest file is loaded (not an outdated cached copy, e.g. from previous version)
asset_url <- function(file) {
  paste0(file, "?v=", format(file.mtime(file.path("www", file)), "%Y%m%d%H%M%S"))
}

keystore <- switch(Sys.info()['sysname'],
                  Windows = "Windows Credential Manager",
                  Darwin = "macOS Keychain",
                  Linux = "Linux Secret Service")

# in the packaged app, these values are passed in during start-up by electron
ver <- ifelse(Sys.getenv("APP_VER") != "", Sys.getenv("APP_VER"), jsonlite::read_json("../package.json")$version)
user_data_path <- ifelse(Sys.getenv("USER_DATA_PATH") != "", Sys.getenv("USER_DATA_PATH"), "user_data")
in_electron <- Sys.getenv("WITHIN_ELECTRON") == "1"

# the desktop app passes a per-launch secret that its window sends with every request (src/index.js)
# this local server can be reached from an ordinary browser
# sessions without the secret (or with a different Host or Origin header) are refused
# when launched without a secret (e.g. from RStudio, for testing & development) all sessions are allowed
launch_token <- Sys.getenv("BR_LAUNCH_TOKEN")
session_authorized <- function(request) {
  if (!nzchar(launch_token)) return(TRUE)
  local_url <- "^(http://)?(127\\.0\\.0\\.1|localhost)(:[0-9]+)?$"
  host <- request$HTTP_HOST %||% ""
  origin <- request$HTTP_ORIGIN %||% ""
  token <- request$HTTP_X_BR_TOKEN %||% ""
  grepl(local_url, host) && (origin == "" || grepl(local_url, origin)) && identical(token, launch_token)
}

# files written for opening in other applications (e.g. Excel) are kept here, inside the user data folder,
# so that electron can open them; anything left over from previous sessions is cleared at start-up
open_files_path <- file.path(user_data_path, "open_temp")
unlink(list.files(open_files_path, full.names = TRUE))
dir.create(open_files_path, showWarnings = FALSE, recursive = TRUE)

# --- BCDM field definitions ---
# BOLDconnectR and BOLDNODE read field_definitions.tsv straight from GitHub every time data is fetched, searched,
# or analysed, so these fail without a connection (and BOLDconnectR waits up to 350 s on a slow one).
# When each package loads, the app looks through its functions for that URL and replaces only the URL with a call
# that returns a local copy: a cached copy in the user data folder (refreshed from the package's own URL at most
# weekly, with a short timeout), or the copy bundled with the app in data/. Everything else in the package's
# functions is left as is, and functions without the URL (e.g. after a package update) are not modified.
field_defs_bundled <- normalizePath("data/field_definitions.tsv", winslash = "/", mustWork = FALSE)
field_defs_cached <- normalizePath(file.path(user_data_path, "field_definitions.tsv"), winslash = "/", mustWork = FALSE)
field_defs <- new.env()
field_defs$tried <- character(0)

# basic check that a file is a BCDM field definitions table (and not, e.g., an error page)
validate_field_defs <- function(path) {
  tryCatch({
    defs <- data.table::fread(path, sep = "\t", quote = "", fill = TRUE, showProgress = FALSE)
    nrow(defs) > 0 && "field" %in% tolower(names(defs))
  }, error = function(e) FALSE)
}

# download url to the cached copy if that is missing or older than max_age_days
# (failures are silent and the existing copy is kept)
refresh_field_defs <- function(url, max_age_days = 7, timeout = 5) {
  age <- if (file.exists(field_defs_cached)) difftime(Sys.time(), file.mtime(field_defs_cached), units = "days") else Inf
  if (age < max_age_days) return(invisible(FALSE))
  tmp <- tempfile(fileext = ".tsv")
  old <- options(timeout = timeout)
  on.exit({ options(old); unlink(tmp) })
  ok <- tryCatch(utils::download.file(url, tmp, quiet = TRUE, mode = "wb") == 0,
                 error = function(e) FALSE, warning = function(w) FALSE)
  ok <- ok && validate_field_defs(tmp)
  if (ok) file.copy(tmp, field_defs_cached, overwrite = TRUE)
  invisible(ok)
}

# called by the patched package functions in place of the URL; returns the path of a local copy
field_defs_source <- function(url) {
  # one routine refresh attempt per URL per session
  if (!url %in% field_defs$tried) {
    field_defs$tried <- c(field_defs$tried, url)
    refresh_field_defs(url)
  }
  for (path in c(field_defs_cached, field_defs_bundled)) {
    if (file.exists(path)) return(path)
  }
  # no local copy at all: try once more with a longer timeout (a copy is kept for offline use if it works)
  if (refresh_field_defs(url, max_age_days = 0, timeout = 30)) return(field_defs_cached)
  stop("BOLD field definitions could not be downloaded, and no local copy is available yet. ",
       "Please connect to the internet and try again (a copy is then saved for offline use).", call. = FALSE)
}

# return expr with any field_definitions.tsv URL replaced by a call to field_defs_source(<URL>)
# (attribute "replaced" gives the number of replacements)
replace_field_defs_url <- function(expr) {
  n <- 0
  walk <- function(e) {
    if (is.character(e) && length(e) == 1 && !is.na(e) && grepl("^https?://.*field_definitions\\.tsv$", e)) {
      n <<- n + 1
      as.call(list(field_defs_source, e))
    } else if (is.call(e)) {
      for (i in seq_along(e)) {
        # (elements are accessed directly, since empty arguments such as in x[, 1] can't be assigned to a variable)
        if (is.call(e[[i]]) || is.character(e[[i]])) e[[i]] <- walk(e[[i]])
      }
      e
    } else {
      e
    }
  }
  out <- walk(expr)
  attr(out, "replaced") <- n
  out
}

# patch every function in a package namespace that reads the field definitions URL
patch_field_defs <- function(pkg) {
  patch <- function(...) {
    ns <- asNamespace(pkg)
    for (name in ls(ns, all.names = TRUE)) {
      fun <- get(name, envir = ns)
      if (!is.function(fun) || is.primitive(fun)) next
      new_body <- replace_field_defs_url(body(fun))
      if (attr(new_body, "replaced") > 0) {
        attr(new_body, "replaced") <- NULL
        body(fun) <- new_body
        utils::assignInNamespace(name, fun, ns = pkg)
      }
    }
  }
  if (isNamespaceLoaded(pkg)) patch() else setHook(packageEvent(pkg, "onLoad"), patch)
}
patch_field_defs("BOLDconnectR")
patch_field_defs("BOLDNODE")

# open a file or folder with the system's default application
# (in the desktop app this goes through electron, so that the new window comes up in the foreground)
open_path <- function(path, session = shiny::getDefaultReactiveDomain()) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (in_electron) {
    session$sendCustomMessage("open_path", path)
  } else {
    open_path_native(path)
  }
}

# --- Fallbacks for running the app outside the desktop app (e.g. from RStudio or with shiny::runApp) ---

# open a file or folder using the operating system's own opener
open_path_native <- function(path, os = Sys.info()[["sysname"]]) {
  tryCatch({
    if (os == "Windows") {
      shell.exec(path)
    } else if (os == "Darwin") {
      system2("open", shQuote(path), wait = FALSE)
    } else if (nzchar(Sys.which("xdg-open"))) {
      system2("xdg-open", shQuote(path), wait = FALSE, stdout = FALSE, stderr = FALSE)
    } else {
      utils::browseURL(path)
    }
  }, error = function(e) {
    showNotification(paste0("Could not open file or folder: ", conditionMessage(e)), type = "error")
  })
  invisible()
}

# show a native file dialog
# returns the selected path, NULL if the user cancelled, or NA if no dialog is available on this system
# (in which case the caller should let the user type or paste a path instead)
choose_file_native <- function(caption = "Select a file", ext = NULL, os = Sys.info()[["sysname"]]) {
  # result of a command-line dialog: a non-zero exit status means it was cancelled
  dialog_result <- function(out) if (!is.null(attr(out, "status")) || length(out) == 0) NULL else out[1]
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::selectFile(caption = caption,
                             filter = if (!is.null(ext)) sprintf("%s files (*.%s)", toupper(ext), ext) else "All files (*)")
    } else if (os == "Windows") {
      filters <- if (!is.null(ext)) rbind(c(sprintf("%s files (*.%s)", toupper(ext), ext), paste0("*.", ext)), utils::Filters["All", ]) else utils::Filters["All", , drop = FALSE]
      path <- utils::choose.files(caption = caption, multi = FALSE, filters = filters)
      if (length(path) == 0) NULL else path
    } else if (os == "Darwin") {
      # shown by the frontmost application (i.e. the browser), so the dialog appears in front of the app
      script <- sprintf('tell application (path to frontmost application as text) to POSIX path of (choose file with prompt "%s"%s)',
                        caption, if (!is.null(ext)) sprintf(' of type {"%s"}', ext) else "")
      dialog_result(suppressWarnings(system2("osascript", c("-e", shQuote(script)), stdout = TRUE, stderr = FALSE)))
    } else if (nzchar(Sys.which("zenity"))) {
      dialog_result(suppressWarnings(system2("zenity", c("--file-selection", shQuote(paste0("--title=", caption)),
                                                         if (!is.null(ext)) shQuote(sprintf("--file-filter=%s files | *.%s", toupper(ext), ext))),
                                             stdout = TRUE, stderr = FALSE)))
    } else if (nzchar(Sys.which("kdialog"))) {
      dialog_result(suppressWarnings(system2("kdialog", c("--title", shQuote(caption), "--getopenfilename", shQuote(path.expand("~")),
                                                          if (!is.null(ext)) shQuote(paste0("*.", ext))),
                                             stdout = TRUE, stderr = FALSE)))
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}

# write text to the system clipboard; returns TRUE on success, FALSE if no clipboard method worked
copy_text_native <- function(text, max.size = 200 * 1000, os = Sys.info()[["sysname"]]) {
  write_pipe <- function(cmd) {
    con <- pipe(cmd, "w")
    writeLines(text, con)
    status <- close(con)
    is.null(status) || identical(as.integer(status), 0L)
  }
  tryCatch({
    if (os == "Windows") {
      con <- file(paste0("clipboard-", formatC(max.size, format = "f", digits = 0)), open = "w")
      writeLines(text, con)
      close(con)
      TRUE
    } else if (os == "Darwin" && nzchar(Sys.which("pbcopy"))) {
      write_pipe("pbcopy")
    } else if (nzchar(Sys.getenv("WAYLAND_DISPLAY")) && nzchar(Sys.which("wl-copy"))) {
      write_pipe("wl-copy")
    } else if (nzchar(Sys.which("xclip"))) {
      write_pipe("xclip -selection clipboard")
    } else if (nzchar(Sys.which("xsel"))) {
      write_pipe("xsel --clipboard --input")
    } else {
      FALSE
    }
  }, error = function(e) FALSE)
}

config <- read_yaml("data/config.yaml", readLines.warn=FALSE)

# labels for fields from config (fields not listed keep their own names)
field_labels <- unlist(config$fieldsets$all)
field_label <- function(fields) {
  labels <- names(field_labels)[match(fields, field_labels)]
  ifelse(is.na(labels), fields, labels)
}
# choices for select inputs (verbatim fields last)
field_choices <- function(fields) {
  labels <- field_label(fields)
  o <- order(startsWith(labels, "Verbatim"))
  stats::setNames(as.list(fields[o]), labels[o])
}

# summary options: common fields first, then the rest
summarize_by_choices <- function(fields = unlist(config$fieldsets$bcdm)) {
  common <- c("identification", "bin_uri", "id_date_parsed", "country.ocean")
  list(
    "Taxonomy" = list("Taxonomic summary" = "tax_summary"),
    "Unique values - selected fields" = c(field_choices(common[1:3]), list("Projects" = "projects", "Datasets" = "datasets"),
                                          field_choices(common[4])),
    "Unique values - all fields" = field_choices(setdiff(fields, c(common, "project_code", "bold_recordset_code_arr", "pre_md5hash")))
  )
}

# initialize user_config file in user data directory
user_config <- if(file.exists(file.path(user_data_path,"user_config.yaml"))) {
  read_yaml(file.path(user_data_path,"user_config.yaml"))
} else {
  list(fieldsets = list(custom = c()))
}
# user settings over the defaults in config.yaml; if settings not saved or saved empty, go with defaults
with_defaults <- function(settings) utils::modifyList(config$settings, Filter(length, as.list(settings)))
user_config$settings <- with_defaults(user_config$settings)

ranks <- names(config$ranknumeric) 

map_colours <- colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61",
                                  "#fee08b","#ffffbf","#e6f598","#abdda4",
                                  "#66c2a5","#3288bd","#5e4fa2"), alpha = TRUE)(11)

# citations for app
citations <- c(HTML(sprintf('Monckton SK & Padhye S (2026). <span style="font-weight:600;">BOLDreflectR</span> (Version %s) [Software]. <a href="https://github.com/skmonckton/BOLDreflectR">https://github.com/skmonckton/BOLDreflectR</a>', ver)),
               HTML(sprintf('Padhye S, Ballesteros-Mejia L, Agda T, Agda J, Ratnasingham S (2026). <span style="font-weight:600;">BOLDconnectR</span>: Retrieve, Transform and Analyze the Barcode of Life Data Systems Data. R package version %s. <a href="https://doi.org/10.32614/CRAN.package.BOLDconnectR">https://doi.org/10.32614/CRAN.package.BOLDconnectR</a>', packageVersion("BOLDconnectR"))),
               HTML(sprintf('Padhye S & Monckton SK (2026). <span style="font-weight:600;">BOLDNODE</span>: Search and Explore BOLD Data Packages Efficiently. R package version %s. <a href="https://doi.org/10.32614/CRAN.package.BOLDNODE">https://doi.org/10.32614/CRAN.package.BOLDNODE</a>', packageVersion("BOLDNODE"))),
               HTML('Ratnasingham S, Wei C, Chan D, Agda J, Agda J, Ballesteros-Mejia L, Ait Boutou H, El Bastami Z M, Ma E, Manjunath R, Rea D, Ho C, Telfer A, McKeowan J, Rahulan M, Steinke C, Dorsheimer J, Milton M, Hebert PDN (2024). <span style="font-weight:600;">BOLD v4: A Centralized Bioinformatics Platform for DNA-Based Biodiversity Data</span>. In DNA Barcoding: Methods and Protocols, pp. 403–441. Chapter 26. New York, NY: Springer US, 2024.'))

# function to save user options (called without arguments, it just writes the current config, e.g. after field set changes)
save_user_config <- function(save_settings = list()) {
  user_settings <- isolate(user_config$settings)
  new_settings <- c(user_settings[!names(user_settings) %in% names(save_settings)],
                    save_settings)
  user_config$settings <<- with_defaults(new_settings)
  write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
}

# alignment methods (DECIPHER functions, or none; see align_sequences() in R/alignment.R)
alignment_methods <- c("Nucleotides (DECIPHER AlignSeqs)" = "seqs",
                       "Nucleotides, refined (DECIPHER AlignSeqs)" = "seqs_refined",
                       "Amino acids (DECIPHER AlignTranslation)" = "translation",
                       "Amino acids, refined (DECIPHER AlignTranslation)" = "translation_refined",
                       "None (use submitted alignment)" = "none")

# NCBI translation tables per Biostrings::GENETIC_CODE_TABLE (listed here to avoid loading Biostrings at startup)
genetic_codes <- c("1. Standard" = 1, "2. Vertebrate mitochondrial" = 2, "3. Yeast mitochondrial" = 3,
                   "4. Mold, protozoan and coelenterate mitochondrial" = 4, "5. Invertebrate mitochondrial" = 5,
                   "6. Ciliate, dasycladacean and Hexamita nuclear" = 6, "9. Echinoderm and flatworm mitochondrial" = 9,
                   "10. Euplotid nuclear" = 10, "11. Bacterial, archaeal and plant plastid" = 11,
                   "12. Alternative yeast nuclear" = 12, "13. Ascidian mitochondrial" = 13,
                   "14. Alternative flatworm mitochondrial" = 14, "15. Blepharisma macronuclear" = 15,
                   "16. Chlorophycean mitochondrial" = 16, "21. Trematode mitochondrial" = 21,
                   "22. Scenedesmus obliquus mitochondrial" = 22, "23. Thraustochytrium mitochondrial" = 23,
                   "24. Pterobranchia mitochondrial" = 24, "25. Candidate division SR1 and Gracilibacteria" = 25,
                   "26. Pachysolen tannophilus nuclear" = 26)

# alignment options shared by alignment and tree input options (prefix = "aln" or "tree")
alignment_marker_input <- function(prefix) {
  selectInput(paste0(prefix, "_marker"), "Marker:", choices = list("COI-5P"))
}
alignment_method_inputs <- function(prefix) {
  id <- function(x) paste0(prefix, "_", x)
  tagList(
    selectInput(id("method"),
                div("Alignment method:",
                    bslib::tooltip(
                      icon("circle-question"),
                      HTML("<b>Nucleotides</b> aligns the nucleotide sequences.<br><b>Amino acids</b> aligns the translated sequences and keeps codons intact (slower for small alignments, similar for large ones).<br><b>Refined</b> uses DECIPHER's iterative refinement (more accurate for divergent sequences, about twice as slow).<br><b>None</b> uses the sequences as they appear, with gaps added to pad the ends to equal length."))),
                choices = alignment_methods, selected = "seqs"),
    conditionalPanel(sprintf("input.%s && input.%s.indexOf('translation') === 0", id("method"), id("method")),
                     selectInput(id("gcode"),
                                 "Translation table:",
                                 choices = genetic_codes, selected = 1))
  )
}
alignment_options <- function(prefix) tagList(alignment_marker_input(prefix), alignment_method_inputs(prefix))

# fields for tree tip labels
tree_label_common <- c("processid", "sampleid", "tip_number", "bin_uri", "nuc_basecount", "country.ocean", "province.state")
tree_label_names <- c(tip_number = "Tip number", stats::setNames(names(field_labels), field_labels))
tree_label_other <- unlist(field_choices(setdiff(field_labels, c(
  tree_label_common, "marker_code", "nuc", "coord", "coord_accuracy", "primers_forward", "primers_reverse",
  "bold_recordset_code_arr", "location.verbatim_gps_accuracy", "location.verbatim_elev_accuracy",
  "location.verbatim_depth_accuracy"))), use.names = FALSE)
# when data is available, only fields that have at least one value are offered
tree_label_fields <- function(filled = NULL) {
  keep <- function(f) if (is.null(filled)) f else f[f %in% c("tip_number", filled)]
  groups <- list("Commonly used fields" = keep(tree_label_common), "Other fields" = keep(tree_label_other))
  lapply(groups[lengths(groups) > 0], function(f) stats::setNames(as.list(f), tree_label_names[f]))
}

# tip label input starts with the saved default
# note: selectize shows preselected items in option order, so the chosen order is passed through selectize's `items` option
tree_label_input <- function(id, label) {
  selectizeInput(id, label, choices = tree_label_fields(), selected = user_config$settings$tree_labels, multiple = TRUE,
                 options = list(plugins = list("drag_drop", "remove_button"), items = as.list(user_config$settings$tree_labels)))
}

# translation table auto-select; plastid markers use table 11, nuclear markers table 1, mitochondrial markers use table of most common taxon in the records
# this is for convenience only: if the wrong table is selected here, the user can simply choose the correct one in the dropdown
suggest_genetic_code <- function(marker, taxa = NULL) {
  if (grepl("^(rbcL|matK|ndhF|psb|trn|rpo|ycf|atpB|accD|ccsA|clpP)", marker)) return(11L)
  if (!grepl("^(CO[I123X]|COX|CYTB|cytb|ND[1-6]|NAD|ATP[68]|12S|16S|mtDNA)", marker)) return(1L)
  top <- function(col) {
    if (is.null(taxa) || !col %in% names(taxa)) return(NA_character_)
    x <- as.character(taxa[[col]])
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) NA_character_ else names(which.max(table(x)))
  }
  kingdom <- top("kingdom")
  phylum <- top("phylum")
  if (isTRUE(kingdom == "Fungi")) return(4L)
  if (isTRUE(kingdom == "Plantae")) return(1L)
  if (is.na(phylum)) return(5L)
  if (phylum == "Chordata") return(if (isTRUE(top("class") == "Ascidiacea")) 13L else 2L)
  if (phylum %in% c("Echinodermata", "Platyhelminthes")) return(9L)
  if (phylum %in% c("Cnidaria", "Porifera", "Placozoa")) return(4L)
  5L
}

# this is used to re-compute selectize input options when custom filter sets are created or removed
filter_options <- function(include_verbatim = TRUE) {
  if(include_verbatim) { 
    list("Default" = list("BCDM fields" = "bcdm", 
                          "All verbatim fields" = "all"),
         "Saved" = sapply(user_config$fieldsets$custom, function(s) stats::setNames(list(s$id), s$name)),
         "Presets" = list("Sample ID, Process ID, BIN" = "min",
                          "Taxonomic identification" = "acctax",
                          "Verbatim identification" = "verbtax",
                          "Sequence data" = "sequence",
                          "Voucher/specimen details" = "specimen",
                          "Verbatim specimen details" = "verbspec",
                          "Collection data" = "collection",
                          "Verbatim collection data" = "verbcollect",
                          "Projects & datasets" = "recset"),
         "Parsed fields" = list("Identification date" = "parsed|id_date_parsed",
                                "Project code" = "parsed|project_code",
                                "Latitude & longitude" = "parsed|lat|lon"),
         "All fields" = field_choices(field_labels))
  } else {
    list("Default" = list("BCDM fields" = "bcdm",
                          "BCDM and parsed fields" = "all"),
         "Saved" = sapply(user_config$fieldsets$custom, function(s) stats::setNames(list(s$id), s$name)),
         "Presets" = list("Sample ID, Process ID, BIN" = "min",
                          "Taxonomic identification" = "acctax",
                          "Sequence data" = "sequence",
                          "Voucher/specimen details" = "specimen",
                          "Collection data" = "collection",
                          "Projects & datasets" = "recset"),
         "Parsed fields" = list("Identification date" = "parsed|id_date_parsed",
                                "Project code" = "parsed|project_code",
                                "Latitude & longitude" = "parsed|lat|lon"),
         "All fields" = field_choices(intersect(field_labels, unlist(c(config$fieldsets$bcdm, config$fieldsets$parsed)))))
  }
  
} 

# list of barcode markers for select/dropdown inputs
marker_options <- list(" " = "",
                       Primary = stats::setNames(config$markercodes$major, config$markercodes$major),
                       Other = stats::setNames(config$markercodes$other, config$markercodes$other))


# dependencies and imports for an installed package are loaded first
package_imports <- function(pkg) {
  file <- system.file("DESCRIPTION", package = pkg)
  if (!nzchar(file)) return(character())
  desc <- read.dcf(file, fields = c("Depends", "Imports"))
  setdiff(trimws(sub("\\(.*", "", unlist(strsplit(desc[!is.na(desc)], ",")))), c("R", ""))
}

# function to set the API key for this session and either save to the system's credential store (remember = TRUE)
# or remove any saved key from there (remember = FALSE)
validate_apikey <- function(key, remember = TRUE) {
  stored <- ""
  try(stored <- key_get("BOLD.apikey"), silent = TRUE)
  key_set <- FALSE
  if (key == "") {
    Sys.setenv(api_key = "")
    shinyjs::addClass(id = "api_key", class = "input_visible")
    shinyjs::show("api_key")
    shinyjs::reset("api_key")
  } else {
    tryCatch({
      set_apikey(key)
      key_set <- TRUE
      shinyjs::removeClass(id = "api_key", class = "input_visible")
      shinyjs::hide("api_key")
      # cleared once used, so later searches don't apply the sidebar's Remember setting to it again
      shinyjs::reset("api_key")
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  }
  if (remember && key_set && key != stored) {
    try(key_set_with_value("BOLD.apikey", password = key), silent = TRUE)
  } else if (!remember && nzchar(stored)) {
    try(key_delete("BOLD.apikey"), silent = TRUE)
  }
}

# function to update user_config.yaml with current list of available data package files
# (also returns a list of said files)
update_datapackages <- function() {
  files <- list.files(path = file.path(user_data_path, "BOLD_data_packages"), pattern = "\\.parquet$",
                      ignore.case = TRUE, full.names = TRUE)
  # files that can't be read (damaged, copy in-progress) are excluded and listed in datapkgs_skipped
  # otherwise they can stop the app from starting
  rows <- vapply(files, function(f) {
    try(upgrade_saved_datapackage(f), silent = TRUE)
    tryCatch(as.numeric(arrow::open_dataset(f)$num_rows), error = function(e) NA_real_)
  }, numeric(1), USE.NAMES = FALSE)
  datapkgs_skipped <<- basename(files[is.na(rows)])
  files <- files[!is.na(rows)]
  info <- file.info(files)
  # newest first
  datapkgs <- data.table(id = tools::file_path_sans_ext(basename(files)), path = files,
                         size_kb = format(info$size, scientific = FALSE), created = format(info$ctime, "%Y-%m-%d"),
                         rows = rows[!is.na(rows)])[order(info$ctime, decreasing = TRUE)]
  user_config[["datapackages"]] <- apply(datapkgs, 1, as.list)
  write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
  return(datapkgs)
}

# BOLDNODE minimally needs these columns in a data package
datapkg_required_cols <- c("processid", "country/ocean", "province/state", "coord", "bold_recordset_code_arr")

# NULL if a file can be used as a data package, otherwise returns the reason it can't
check_datapackage <- function(path) {
  cols <- tryCatch(names(arrow::open_dataset(path)$schema), error = function(e) NULL)
  if (is.null(cols)) return("The file could not be read as a Parquet file (it may be damaged or incomplete).")
  # packages with non-BCDM names for country/ocean & province/state are accepted and converted once added
  cols[cols == "country.ocean"] <- "country/ocean"
  cols[cols == "province.state"] <- "province/state"
  missing <- setdiff(datapkg_required_cols, cols)
  if (length(missing) > 0) {
    return(paste0("The file is not a BOLD data package (columns missing: ", paste(missing, collapse = ", "), ")."))
  }
  NULL
}

# renaming columns to BCDM standard
as_datapackage <- function(data) {
  data <- data[, setdiff(names(data), c("id_date_parsed", "project_code", "lat", "lon")), with = FALSE]
  setnames(data, c("country.ocean", "province.state"), c("country/ocean", "province/state"), skip_absent = TRUE)
  data
}

# catch data packages saved with non-BCDM column names, then correct and rewrite them
upgrade_saved_datapackage <- function(path) {
  cols <- names(arrow::open_dataset(path)$schema)
  if (!"country.ocean" %in% cols || "country/ocean" %in% cols) return(FALSE)
  temp <- tempfile(fileext = ".parquet")
  on.exit(unlink(temp))
  arrow::write_parquet(as_datapackage(data.table::as.data.table(arrow::read_parquet(path, mmap = FALSE))), temp,
                       compression = "zstd", compression_level = 3)
  # release arrow's handles on the file so Windows can replace it
  gc()
  file.copy(temp, path, overwrite = TRUE)
}

# keep list of BOLDNODE data packages in memory
datapkgs_skipped <- character(0)
datapkgs <- update_datapackages()

# prepare a table for saving
# TSV has no quoting, so tabs and line breaks in values become spaces
# in CSV and TSV, values a spreadsheet would run as formulas get a leading ' (negative numbers are left alone)
# in XLSX, values over Excel's 32,767-character cell limit are cut short and end in "[truncated]"
prep_export <- function(df, format, formulas = format %in% c("csv", "tsv")) {
  # (a copy: set() below changes the table in place)
  df <- data.table::copy(data.table::as.data.table(df))
  # an Excel sheet holds 1,048,576 rows, including the header
  if (format == "xlsx" && nrow(df) > 1048575) {
    showNotification(sprintf("Excel sheets hold at most 1,048,575 rows, so only the first 1,048,575 of %s were saved. Save as CSV or TSV to keep them all.",
                             format(nrow(df), big.mark = ",")), type = "warning", duration = NULL)
    df <- df[seq_len(1048575)]
  }
  if (formulas) {
    nm <- names(df)
    formula <- grepl("^[=@\t\r]", nm) | (grepl("^[+-]", nm) & !grepl("^[+-][0-9.]+([eE][+-]?[0-9]+)?$", nm))
    data.table::setnames(df, ifelse(formula, paste0("'", nm), nm))
  }
  # handle text columns, including those kept as factors for the table's filters
  for (col in names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]) {
    v <- as.character(df[[col]])
    if (format == "tsv") v <- gsub("[\t\r\n]+", " ", v)
    if (formulas) {
      formula <- grepl("^[=@\t\r]", v) | (grepl("^[+-]", v) & !grepl("^[+-][0-9.]+([eE][+-]?[0-9]+)?$", v))
      v[formula] <- paste0("'", v[formula])
    }
    if (format == "xlsx") {
      long <- !is.na(v) & nchar(v, allowNA = TRUE) > 32767
      long[is.na(long)] <- FALSE
      v[long] <- paste0(substr(v[long], 1, 32767 - nchar("[truncated]")), "[truncated]")
    }
    data.table::set(df, j = col, value = v)
  }
  df
}

# copy table to clipboard
# (in the desktop app, the text is passed to electron's clipboard, which works on all platforms without external
# tools such as xclip; otherwise the system clipboard is written directly, and if that isn't possible
# (e.g. no clipboard tool on Linux, or text too large for the Windows clipboard connection) the browser's
# clipboard is used instead)
cb <- function(df, header=TRUE, sep="\t", na="", max.size=(200*1000), session = shiny::getDefaultReactiveDomain()){
  os <- Sys.info()[["sysname"]]
  # tabs and line breaks in values would split the pasted cells
  if (is.data.frame(df)) df <- prep_export(df, "tsv", formulas = FALSE)
  text <- capture.output(
    write.table(df, stdout(), sep=sep, na=na, col.names=header, row.names=FALSE, quote=FALSE)
  )
  if (in_electron || !copy_text_native(text, max.size = max.size, os = os)) {
    eol <- if (os == "Windows") "\r\n" else "\n"
    session$sendCustomMessage("copy_text", paste0(paste(text, collapse = eol), eol))
  }
  showNotification("Copied to clipboard.", type="message", id="copy_msg")
}

# FASTA headers are one line: line breaks (and tabs) in field values become spaces
fasta_header <- function(x) gsub("[\r\n\t]+", " ", x)

# assemble FASTA (with optional copy to clipboard)
clip_fasta <- function(bold_df,
                       cols_for_fas_names = NULL,
                       collapse_mrkrs = NULL,
                       to_cb = TRUE) {
  # header definition
  if (is.null(cols_for_fas_names)) cols_for_fas_names <- user_config$settings$fasta_head
  # add marker code to distinguish between sequences for any process IDs with multiple markers
  if (any(duplicated(bold_df$processid)) && !any(c("marker_code", "record_id") %in% cols_for_fas_names)) {
    showNotification("Some records have multiple markers. Output FASTA will include marker in headers.", duration = 6, type = "warning")
    cols_for_fas_names <- c(cols_for_fas_names, "marker_code")
  }
  # assemble FASTA (conditional logic depending on whether input data is marker-collapsed)
  clip_fas <- if((!"nuc" %in% names(bold_df)) || (nrow(bold_df[!empty(nuc)]) == 0)) {
    if(!is.null(collapse_mrkrs)) {
      showNotification(if (to_cb) "Copying FASTA..." else "Preparing FASTA...", id="copy_msg", type = "message")
      cols_for_fas_names <- intersect(cols_for_fas_names, names(bold_df))
      mrkrs <- sapply(collapse_mrkrs, function(x) unlist(strsplit(x, "_"))[1])
      fas <- NULL
      for(i in seq_along(mrkrs)) {
        fas <- c(fas, paste0(">",
                             fasta_header(bold_df[!is.na(get(names(mrkrs)[i])), do.call(paste, c(.SD, unname(mrkrs[i]), sep = "|")), .SDcols = c(cols_for_fas_names)]),
                             "\n",
                             gsub("\\s+", "", bold_df[!is.na(get(names(mrkrs)[i])), get(names(mrkrs)[i])])))
      }
      fas
    } else {
      NULL
    }
  } else {
    showNotification(if (to_cb) "Copying FASTA..." else "Preparing FASTA...", id="copy_msg", type = "message")
    paste0(">",
           fasta_header(bold_df[!empty(nuc), do.call(paste, c(.SD, sep = "|")), .SDcols = c(cols_for_fas_names)]),
           "\n",
           gsub("\\s+", "", bold_df[!empty(nuc), nuc]))
  }
  # handle FASTA output
  if(is.null(clip_fas)) {
    showNotification(if (to_cb) "No sequences to copy." else "No sequences to save.", id="copy_msg", type = "error")
    if (!to_cb) return(character(0))
  } else if (to_cb) {
    tryCatch(
      cb(unlist(strsplit(clip_fas,"\n")), header = FALSE, sep = ""),
      error = function(e) {
        showNotification(paste0("FASTA output too large. Please try again with fewer sequences."), id="copy_msg", type = "error")
      }
    )
  } else {
    return(clip_fas)
  }
}

# summary of query yielding current data (recorded on fetch in outdata$query_info)
describe_query <- function(mode, query, datapackage = NULL) {
  id_labels <- c(processid = "Process IDs", sampleid = "Sample IDs", bin_uris = "BINs", dataset_codes = "Dataset codes",
                 project_codes = "Project codes", ids = "Process/Sample IDs", bins = "BINs",
                 dataset.projects = "Project/Dataset codes")
  term_labels <- c(taxonomy = "Taxonomy", geography = "Geography", institutes = "Institutes",
                   identified.by = "Identified by", biogeo.cat = "Biome/Realm/Ecoregion", seq.source = "Sequence run sites")
  list_terms <- function(x, max = 25) {
    x <- as.character(unlist(x))
    if (length(x) > max) paste0(paste(x[seq_len(max)], collapse = ", "), ", ... (", length(x), " in total)") else paste(x, collapse = ", ")
  }
  parts <- character(0)
  for (nm in names(query)) {
    v <- query[[nm]]
    if (nm %in% names(id_labels)) {
      parts <- c(parts, id_labels[[nm]])
    } else if (nm %in% names(term_labels)) {
      parts <- c(parts, paste0(term_labels[[nm]], ": ", list_terms(v)))
    } else if (nm == "marker") {
      lengths <- c(query$marker_min_length, query$marker_max_length)
      parts <- c(parts, paste0("Marker: ", list_terms(v), if (length(lengths) > 0) paste0(" (", paste(lengths, collapse = "-"), " bp)")))
    } else if (nm == "basecount") {
      parts <- c(parts, paste0("Sequence length: ", paste(sprintf("%s %g-%g bp", names(v), sapply(v, `[`, 1), sapply(v, `[`, 2)), collapse = ", ")))
    } else if (nm == "bounding.box") {
      parts <- c(parts, sprintf("Coordinates: longitude %g to %g, latitude %g to %g", v[1], v[2], v[3], v[4]))
    } else if (nm == "collection_start_date") {
      parts <- c(parts, paste0("Collection dates: ", format(as.Date(v)), " to ",
                               if (!is.null(query$collection_end_date)) format(as.Date(query$collection_end_date)) else "present"))
    }
  }
  source <- switch(mode,
                   fetch_opts = "Data API fetch (BOLDconnectR)",
                   search_opts = "Data API search (BOLDconnectR)",
                   "source-local" = paste("Local data package (BOLDNODE)", datapackage),
                   mode)
  list(source = source, terms = parts, time = Sys.time(),
       summary = if (length(parts) > 0) paste0(source, ": ", paste(parts, collapse = "; ")) else source)
}

# split and clean query terms
split_query <- function(query_input, list = FALSE, sep = c("\n", ",")) {
  if(query_input == "") {
    NULL
  } else {
    terms <- trimws(strsplit(query_input, paste(sep, collapse = "|"))[[1]])
    terms <- unique(terms[!empty(terms)])
    if(list == TRUE) {
      as.list(terms)
    } else {
      terms
    }
  }
}

# double quotes to prevent breaking on ' in file or folder name
node_path <- function(path) gsub("'", "''", path, fixed = TRUE)

# match whole dataset codes rather than treating them as patterns
whole_code_patterns <- function(codes) {
  paste0("(^|,)", gsub("([][{}()+*^$|\\?.-])", "\\\\\\1", trimws(unlist(codes))), "(,|$)")
}

# convert a user-supplied name into a data package ID that is safe to use as a file name on all platforms
# removes characters that are invalid in Windows/macOS/Linux file names; other characters are kept
clean_dtpkg_id <- function(name) {
  id <- gsub("\\.parquet$", "", trimws(name), ignore.case = TRUE)
  id <- gsub(" ", "_", id)
  id <- gsub("[<>:\"/\\\\|?*[:cntrl:]]", "", id)
  # Windows does not allow file names ending in a dot, and files starting with one aren't listed as packages
  gsub("^\\.+|\\.+$", "", id)
}

# save or delete local data packages
modify_datapackage <- function(dtpkg, filename = NULL, operation = "save", overwrite = FALSE) {
  if(operation == "save") {
    outdir <- file.path(user_data_path, "BOLD_data_packages")
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    save_id <- clean_dtpkg_id(if(is.null(filename)) basename(dtpkg) else filename)
    if(!nzchar(save_id)) stop("Please provide a valid ID/alias for the data package.")
    problem <- check_datapackage(dtpkg)
    if(!is.null(problem)) stop(problem)
    save_path <- file.path(outdir, paste0(save_id, ".parquet"))
    if(save_id %in% datapkgs$id && !overwrite) {
      showNotification("There is already a data package with this ID. If you wish to overwrite the existing file, please delete it first.", id = "fileio", type = "error")
    } else {
      if(file.exists(save_path)) {
        # release any open handles on the file (e.g. from a previous search) before overwriting it
        gc()
        if(!file.remove(save_path)) stop("The existing data package could not be overwritten (it may be in use).")
      }
      if(!file.copy(dtpkg, save_path)) stop("The data package file could not be copied to the user data folder.")
      upgrade_saved_datapackage(save_path)
      datapkg <- data.table(file.info(dtpkg)[c("size", "ctime")],
                            keep.rownames = TRUE)[, .(id = save_id, path = save_path,
                                                      size_kb = format(size, scientific = FALSE),
                                                      created = format(ctime, "%Y-%m-%d"),
                                                      rows = arrow::open_dataset(dtpkg)$num_rows)]
      datapkgs <<- rbindlist(list(datapkgs[id != save_id], datapkg))[order(-created)]
      user_config[["datapackages"]] <- apply(datapkgs, 1, as.list)
      write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
      showNotification("Data package saved!", id = "fileio", type = "message")
    }
  } else if(operation == "delete") {
    del_path <- datapkgs[id == dtpkg, path]
    # release any open handles on the file first (as when saving); it stays listed if it can't be removed
    if (length(del_path) == 1 && file.exists(del_path)) {
      gc()
      if (!file.remove(del_path)) stop("The data package could not be deleted (it may be in use).")
    }
    datapkgs <<- datapkgs[id != dtpkg][order(-created)]
    user_config[["datapackages"]] <- apply(datapkgs, 1, as.list)
    write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
  }
  invisible(datapkgs)
}

# save or delete user field sets
modify_custom_fieldset <- function(set_name, set_fields = NULL, operation = "save") {
  
  custom_config <- user_config$fieldsets$custom
  
  if(operation == "delete") {
    # remove selected field set
    idx <- match(set_name, sapply(custom_config, function(s) s$id))
    if (!is.na(idx)) {
      custom_config <- custom_config[-idx]
      user_config$fieldsets$custom <<- custom_config
    }
    # value to return (when saving, this is used to update the filter selectize input)
    new_select <- invisible()
  } else {
    # check for set with same name
    exists <- which(sapply(custom_config, function(s) s$name) == set_name)
    # determine target index and id for saving field set (if one by this name already exists, save over it)
    if(length(exists) > 0) {
      idx <- exists
      cfg_id <- custom_config[[exists]]$id
    } else {
      idx <- length(custom_config) + 1
      ids <- unlist(sapply(custom_config, function(s) as.integer(gsub("userset","",s$id))))
      cfg_id <- ifelse(length(ids) > 0, sprintf("userset%03d", min(setdiff((1:(max(ids)+1)), ids))), "userset001")
    }
    # for component field sets, translate any user-defined sets to their original fields (in case the referenced sets are later deleted)
    int_fields <- c()
    for(i in seq_along(set_fields)) {
      if(grepl("^userset", set_fields[i])) {
        pull <- which(sapply(custom_config, function(s) s$id) == set_fields[i])
        int_fields <- c(int_fields, custom_config[[pull]]$fields)
      } else {
        int_fields <- c(int_fields, set_fields[i])
      }
    }
    # record new config and update global object
    custom_config[[idx]] <- list(id = cfg_id, name = set_name, fields = unique(int_fields))
    user_config$fieldsets$custom <<- custom_config
    # value with which to update filter selectize input
    # (this is the set's id, which does not necessarily match its position in the list)
    new_select <- cfg_id
  }
  # update config file
  save_user_config()
  # return value 
  return(new_select)
}

# function to collapse table to a single row per record, and add extra columns to store marker information (similar to BOLD v4 spreadsheet format)
collapse_markers <- function(data) {
  # pivot to wide format
  pivot_cols <- c("insdc_acs","nuc","nuc_basecount","sequence_run_site","sequence_upload_date")
  collapsed <- dcast(data[, setdiff(names(data), c("record_id", "pre_md5hash")), with = FALSE],
                     ... ~ marker_code,
                     value.var = pivot_cols)
  # remove junk columns for records with no sequences
  na_cols <- names(collapsed)[grepl("_NA$", names(collapsed))]
  collapsed[, (na_cols) := NULL]
  # invert column names
  old <- grep(paste0("^(",paste(pivot_cols, collapse="|"),")_"), names(collapsed), value = TRUE)
  new <- sub(paste0("^(",paste(pivot_cols, collapse="|"),")_(.*)"), "\\2_\\1", old)
  setnames(collapsed, old, new)
  return(collapsed)
}

# return copy of table with factor columns converted to character (or vice-versa) for analyses
convert_factor_char <- function(data, to_factor = TRUE, copy = FALSE) {
  factor_cols <- intersect(config$fieldsets$factorfields, names(data))
  if(copy) data <- copy(data)
  convert_col <- if(to_factor) { as.factor } else { as.character }
  data[, (factor_cols) := lapply(.SD, convert_col), .SDcols = factor_cols]
  if(copy) return(data)
}

# decompose recordset column into a list of datasets or project codes
list_recordsets <- function(data, set_type, list_by) {
  parts <- strsplit(data$bold_recordset_code_arr, ",", fixed = TRUE)
  recordsets <- unique(data.table(id = rep(data[[list_by]], lengths(parts)), set = trimws(unlist(parts))))
  setnames(recordsets, "id", list_by)
  if(set_type == "parse_project") {
    # one value per row of data, in the same row order (merge() would sort the result by list_by)
    projects <- recordsets[!is.na(set) & set != "" & !grepl("^DS-|^DATASET", set),
                           .(set = paste(unique(set), collapse = ",")), by = list_by]
    projects$set[match(data[[list_by]], projects[[list_by]])]
  } else if(set_type == "projects") {
    recordsets[!grepl("^DS-|^DATASET", set), .(count = .N), by = .(project_code = set)][order(-count)]
  } else {
    recordsets[grepl("^DS-|^DATASET", set), .(count = .N), by = .(dataset_code = set)][order(-count)]
  }
}

# compute summary tables
summarize_table <- function(data, sum_type, list_by) {
  if (sum_type %in% c("projects","datasets")){
    list_recordsets(data, sum_type, list_by)
  } else if (sum_type == "tax_summary") {
    # each taxon counts its own records and those of all descendants (empty ranks are skipped)
    taxa <- unique(data, by = list_by)[, ..ranks]
    taxa[, (ranks) := lapply(.SD, function(x) { x <- as.character(x); x[empty(x)] <- NA_character_; x })]
    tax_count <- rbindlist(lapply(seq_along(ranks), function(i) {
      taxa[!is.na(get(ranks[i])), .(rank = ranks[i], count = .N), by = c(ranks[seq_len(i)])]
    }), fill = TRUE)
    tax_count[, taxon := fcoalesce(.SD), .SDcols = rev(ranks)]
    # parents first (missing lower ranks sort first)
    setorderv(tax_count, ranks, na.last = FALSE)[, .SD, .SDcols = c("taxon", "rank", ranks, "count")]
  } else if (is.null(sum_type)) {
    data
  } else {
    char_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
    unique(data, by = list_by)[, (char_cols) := 
                                 lapply(.SD, function(x) {
                                   if (is.factor(x)) {
                                     x <- as.character(x)
                                     x[x %in% c("", " ")] <- NA_character_
                                     as.factor(x)
                                   } else {
                                     replace(x, x == "", NA_character_)
                                     }
                                 }), .SDcols = char_cols][, .(count = .N), by = sum_type]
  }
}

# taxonomic summary shown as a collapsible tree (collapsing and search are handled in www/tax-tree.js)
tax_tree <- function(tax_count) {
  depth <- rowSums(!is.na(tax_count[, ..ranks]))
  has_children <- c(depth[-1] > depth[-length(depth)], FALSE)
  rows <- sprintf(
    '<div class="tax-row%s" data-depth="%d" data-name="%s" style="padding-left: %.1frem"><span class="tax-toggle"></span><span class="tax-name rank-%s">%s</span><span class="tax-rank">%s</span><span class="tax-count">%s</span></div>',
    ifelse(has_children, " has-children", ""), depth, htmltools::htmlEscape(tolower(tax_count$taxon), attribute = TRUE),
    (depth - 1) * 1.25, tax_count$rank, htmltools::htmlEscape(tax_count$taxon), tax_count$rank,
    format(tax_count$count, big.mark = ",", trim = TRUE)
  )
  div(
    class = "tax-tree",
    div(
      class = "tax-toolbar",
      tags$button(type = "button", class = "btn btn-default btn-sm", `data-tax-action` = "expand",
                  icon("square-plus"), "Expand all"),
      tags$button(type = "button", class = "btn btn-default btn-sm", `data-tax-action` = "collapse",
                  icon("square-minus"), "Collapse all"),
      span(class = "tax-status", paste(format(nrow(tax_count), big.mark = ","), "taxa")),
      tags$input(type = "search", class = "tax-search form-control", placeholder = "Search taxa",
                 title = "Shows matching taxa with their parents and descendants")
    ),
    div(class = "tax-rows", HTML(paste(rows, collapse = "")))
  )
}

# extract identification dates from taxonomy_notes
parse_id_date <- function(data) {
  string_match <- function(x, pat) regmatches(x, gregexpr(pat, x, perl=TRUE), invert = NA)
  id_date_verb <- if("specimendetails.verbatim_identification_method" %in% names(data)) {
    lubridate::my(sapply(string_match(data$specimendetails.verbatim_identification_method, "(?<=\\()[A-Za-z]{3,9} [0-9]{2,4}(?=\\))"), `[`, 2))
  } else {
    NULL
  }
  id_date_note <- lubridate::my(sapply(string_match(data$taxonomy_notes, "(?<=id-date:\\s?)[A-Za-z]{3,9} [0-9]{2,4}"), `[`, 2))
  id_date <- format(as.Date(do.call(pmax, c(data.table(id_date_verb,id_date_note), na.rm = TRUE)), format = "%Y-%m-%d"), format = "%b %Y")
  return(id_date)
}

# extract lats and longs from BCDM coord column
parse_lat_lon <- function(coord) {
  if(all(empty(coord))) {
    list(rep(as.numeric(NA), length(coord)), rep(as.numeric(NA), length(coord)))
  } else {
    lapply(tstrsplit(coord, ",", fixed = TRUE), as.numeric)
  }
}

# generate formatted labels for diversity analysis output
get_div_label <- function(type, capitalize = FALSE, plural = FALSE) {
  label <- switch(type,
                  bin_uri = c("BIN", "BINs"),
                  subspecies = c("subspecies", "subspecies"),
                  bin_fallback =, species = c("species", "species"),
                  genus = c("genus" , "genera"),
                  tribe = c("tribe", "tribes"),
                  subfamily = c("subfamily", "subfamilies"),
                  family = c("family", "families"),
                  class = c("class", "classes"),
                  order = c("order", "orders"),
                  grids = c("grid cell", "grid cells"),
                  site = c("site", "sites"),
                  sector = c("sector", "sectors"),
                  region = c("region", "regions"),
                  province.state = c("province/state", "provinces/states"),
                  country.ocean = c("country", "countries"),
                  ecoregion = c("ecoregion", "ecoregions"),
                  biome = c("biome", "biomes"),
                  realm = c("realm", "realms"))
  label <- ifelse(plural, label[2], label[1])
  if(capitalize) {
    paste(tools::toTitleCase(unlist(strsplit(label, "/"))), collapse = "/")  
  } else {
    label
  }
}

# clean up stray ANSI character formatting
clean_ansi <- function(x) {
  gsub("\033\\[[0-9;]*[mK]", "", x)
}

# wrapper to prepare BOLD fetch/search queries and prepare messages as notifications
# (note: initially intended to handle both fetch and search, but the two have not been harmonized)
bold.fetch.shiny <- function (get_by,
                              query,
                              BCDM_only = FALSE,
                              notification_id = "fetch_msg") {

  tryCatch({
    withCallingHandlers({
      if(get_by == "search") {
        recs <- BOLDconnectR::bold.full.search(taxonomy = query[["tax"]], geography = query[["geo"]])
        bold.fetch.plus("processid", recs$processid, BCDM_only = BCDM_only)
      } else {
        bold.fetch.plus(get_by, query, BCDM_only = BCDM_only)
      }
    }, message = function(m) {
      notif_txt <- clean_ansi(conditionMessage(m))
      dwell <- 2
      if(grepl("^Downloading", notif_txt, perl = TRUE)) dwell <- NULL
      showNotification(notif_txt, id = notification_id, duration = dwell, type = "message")
    })
  }, error = function(e) {
    text <- clean_ansi(conditionMessage(e))
    if(grepl("No records were found", text)) {
      msg <- text
      msg_type <- "warning"
      msg_dwell = 5
    } else {
      msg <- paste("Error fetching data:", text)
      msg_type <- "error"
      msg_dwell = NULL
    }
    showNotification(msg, id = notification_id, duration = msg_dwell, type = msg_type)
    return(NULL)
  })
}

# sort records by the order of the query terms that matched them
# query parameters are prioritized in the order they appear in the query list (i.e. the order of the input options),
# and terms within each parameter by the order they were entered; rows that tie keep their existing order
order_by_query <- function(data, query) {
  qnames <- names(query)[names(query) %in% names(config$queryterms)]
  if(length(qnames) == 0 || nrow(data) == 0) return(data)
  sort_keys <- lapply(qnames, function(q) {
    terms <- unique(tolower(trimws(as.character(unlist(query[[q]])))))
    cols <- intersect(config$queryterms[[q]]$fields, names(data))
    if(length(terms) == 0 || length(cols) == 0) return(NULL)
    # position of the earliest-entered term matching each row, in any of the parameter's fields
    key <- rep(NA_integer_, nrow(data))
    for(col in cols) {
      vals <- as.character(data[[col]])
      pos <- if(col == "bold_recordset_code_arr") {
        codes <- strsplit(vals, ",", fixed = TRUE)
        code_pos <- data.table(row = rep(seq_along(codes), lengths(codes)),
                               pos = match(tolower(trimws(unlist(codes))), terms))
        row_pos <- code_pos[!is.na(pos), .(pos = min(pos)), by = row]
        out <- rep(NA_integer_, nrow(data))
        out[row_pos$row] <- row_pos$pos
        out
      } else {
        match(tolower(vals), terms)
      }
      key <- pmin(key, pos, na.rm = TRUE)
    }
    if(all(is.na(key))) NULL else key
  })
  sort_keys <- Filter(Negate(is.null), sort_keys)
  if(length(sort_keys) == 0) return(data)
  row_order <- do.call(order, c(unname(sort_keys), list(na.last = TRUE, method = "radix")))
  data[row_order]
}

# counts for the overview
overview_counts <- function(data) {
  col <- function(name) if (name %in% names(data)) as.character(data[[name]]) else rep(NA_character_, nrow(data))
  n_unique <- function(x) data.table::uniqueN(x[!empty(x)])
  pid <- col("processid")
  bin <- col("bin_uri")
  species <- col("species")
  bp <- suppressWarnings(as.numeric(col("nuc_basecount")))
  seqs <- data.table(marker = col("marker_code"), bp = bp)[!is.na(bp) & bp > 0]
  seqs[empty(marker), marker := "(no marker)"]
  list(
    specimens = n_unique(pid),
    sequences = nrow(seqs),
    markers = seqs[, .(sequences = .N, min_bp = min(bp), max_bp = max(bp)), by = marker][order(-sequences)],
    specimens_with_bins = n_unique(pid[!empty(bin)]),
    bins = n_unique(bin),
    species = n_unique(species),
    species_with_bins = n_unique(species[!empty(bin)]),
    institutions = n_unique(col("inst"))
  )
}

# counts of records for each term in query
query_term_hits <- function(data, query) {
  qfields <- setdiff(intersect(names(query), names(config$queryterms)), "marker")
  rbindlist(lapply(qfields, function(qfield) {
    terms <- unique(trimws(as.character(unlist(query[[qfield]]))))
    terms <- terms[nzchar(terms)]
    if (length(terms) == 0) return(NULL)
    # every searched value of each record
    values <- rbindlist(lapply(intersect(config$queryterms[[qfield]]$fields, names(data)), function(col) {
      v <- as.character(data[[col]])
      if (col == "bold_recordset_code_arr") {
        v <- strsplit(v, ",", fixed = TRUE)
        data.table(row = rep(seq_along(v), lengths(v)), value = unlist(v))
      } else {
        data.table(row = seq_along(v), value = v)
      }
    }))
    hits <- if (nrow(values) > 0) unique(values[, .(row, value = tolower(trimws(value)))])[, .(records = .N), by = value] else
      data.table(value = character(), records = integer())
    out <- data.table(search = config$queryterms[[qfield]]$name, term = terms,
                      records = hits$records[match(tolower(terms), hits$value, nomatch = NA)])[is.na(records), records := 0L]
    out[, found := fifelse(records > 0, "FOUND", "NOT FOUND")]
    setcolorder(out, c("search", "term", "found", "records"))
  }))
}

# retrieve BIN mates
get_binmates <- function(df, dtpkg_parquet = NULL, batch_size=200, sleep=2) {
  bins <- unique(as.character(df[["bin_uri"]][!empty(as.character(df[["bin_uri"]]))]))
  add_pids <- character()
  if(length(bins) == 0) {
    showNotification("No BINs were found in the fetched data.", id = "bin_msg", duration=2, type = "message")
  } else if(is.null(dtpkg_parquet)) {
    batches = ceiling(length(bins) / batch_size)
    for (r in 1:batches) {
      start = (r-1) * batch_size + 1
      end = min(r * batch_size, length(bins))
      getids <- bins[start:end]
      showNotification(paste0("Checking for additional BIN members from ",length(bins)," BINs... (Batch ",r," of ",batches,")"), id = "bin_msg", duration=NULL, type = "message")
      out <- NA
      sleepmore <- sleep
      attempt <- 0
      max_attempts <- 5
      failed <- FALSE
      while(!is.data.frame(out) && !failed){
        attempt <- attempt + 1
        tryCatch({
          out <- BOLDconnectR:::get.bin.dataset.project.pids(data.input = unique(data.frame(col1 = getids)), query_param = "bin_uris")
        },
        error = function(e) {
          if(attempt >= max_attempts) {
            failed <<- TRUE
            showNotification(paste0("BIN member search stopped after ", max_attempts, " failed attempts (batch ", r, " of ", batches, "): ", e$message,
                                    "\nResults may be incomplete."), duration = NULL, type = "error")
          } else {
            showNotification(paste0("API returned an error: ", e$message, "\nReattempting batch ",r,"..."), type = "warning")
            sleepmore <<- sleepmore + 1
            Sys.sleep(sleepmore)
          }
        })
      }
      if(failed) break
      # the API may return record IDs (processid.marker) rather than process IDs, so reduce them to process IDs
      # before comparing; otherwise every existing record would be treated as an additional BIN member
      batch_pids <- unique(sub("\\..*$", "", trimws(as.character(out[["records"]]))))
      add_pids <- sort(unique(c(add_pids, setdiff(batch_pids, as.character(df[["processid"]])))))
      Sys.sleep(sleep)
    }
  } else {
    withInfProgress(paste0("Searching data package for additional BIN members from ", length(bins), " BINs..."), {
      current_pids <- unique(df[["processid"]])
      pkg_data <- BOLDNODE:::import_parquet_data(node_path(dtpkg_parquet))
      bins_tbl <- dbplyr::copy_inline(pkg_data$src$con, dplyr::tibble(bin_uri = bins))
      pids_tbl <- dbplyr::copy_inline(pkg_data$src$con, dplyr::tibble(processid = current_pids))
      add_pids <- pkg_data |>
        dplyr::semi_join(bins_tbl, by = "bin_uri") |>
        dplyr::anti_join(pids_tbl, by = "processid") |>
        dplyr::distinct(processid) |>
        dplyr::collect() |>
        (`[[`)("processid") 
    })
  }
  if(length(add_pids) == 0) {
    showNotification(paste0("No additional BIN members found."), id = "bin_msg", type = "message")
  } else {
    showNotification(paste0(length(add_pids), " additional BIN members found."), id = "bin_msg", type = "message")
  }
  return(add_pids)
}

# repeatable UI grouping
inputGroup <- function(...,
                       title = NULL,
                       tipTxt = NULL,
                       groupId = NULL,
                       groupClass = NULL) {
  div(class = paste("option-group", groupClass),
      id = groupId,
      div(class="group-header",
          if(!is.null(title)) h3(title),
          if(!is.null(tipTxt)) bslib::tooltip(icon("circle-question"), tipTxt)),
      ...)
}

# JavaScript used to render output data tables
callback_js <- DT::JS("
      window.mapBySID = window.mapBySID || {};
      window.mapByPID = window.mapByPID || {};
      $.fn.dataTable.ext.errMode = 'none';
    ")
header_js <- DT::JS("
  function(thead, data, start, end, display) {
    window.copyCol =  function(i) {
      Shiny.setInputValue('col_copy_clicked', { col: i }, { priority: 'event' });
    }
    $(thead).find('th').each(function(i) {
      if ($(this).find('.col-copy-btn').length > 0) return;
      var colName = $(this).text();
      $(this).empty()
      .append($('<span class=\"header-label\"></span>').text(colName))
      .append('<button class=\"col-copy-btn\" onclick=\"event.stopPropagation(); window.copyCol(' + i + ');\"><i class=\"far fa-copy\"></i></button>');
    });
  }
")
column_js <- DT::JS("
                    function(data, type, row, meta) {
                      if (type === 'display' && data !== null) {
                        var colName = meta.settings.aoColumns[meta.col].sTitle;
                        // cell values arrive HTML-escaped (except quotes) so values used in URLs are decoded and URL-encoded
                        var plain = function(s) { var t = document.createElement('textarea'); t.innerHTML = s; return t.value; };
                        var link = function(url, value, text) {
                          return '<div><a href=\"' + url + encodeURIComponent(value) + '\" target=\"_blank\">' + text + '</a></div>';
                        };
                        if (colName == 'sampleid') {
                          var pid = mapBySID[plain(data)];
                          return pid
                            ? link('https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSpecimen?selectedrecordid=', pid, data)
                            : '<div>' + data + '</div>';
                        }
                        if (colName == 'processid') {
                          return data
                            ? link('https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSequence?processid=', plain(data), data)
                            : '<div>' + data + '</div>';
                        }
                        if (colName == 'bin_uri' || colName == 'nn_bin_uri') {
                          return data
                            ? link('https://bench.boldsystems.org/index.php/Public_BarcodeCluster?clusteruri=', plain(data), data)
                            : '<div>' + data + '</div>';
                        }
                        if (colName == 'avg_dist' || colName == 'max_dist' || colName == 'nn_dist') {
                          var n = parseFloat(data);
                          return '<div>' + (isNaN(n) ? data : n.toFixed(3)) + '</div>';
                        }
                      }
                      if (type === 'display' && data === ' ') {
                        return '';
                      }
                      if (type === 'display' && data) {
                        return '<div>' + data + '</div>';
                      }
                      return data;
                    }
                  ")
reset_js <- DT::JS("
  function(e, dt, node, config) {
    dt.order([]);
    dt.search('');
    dt.columns().search('');
  
    $(dt.table().container()).find('input').val('');
    $(dt.table().container()).find('select').prop('selectedIndex', 0);
  
    $(dt.table().container()).find('.dt-range').each(function(){
      if (this.noUiSlider) this.noUiSlider.reset();
    });
  
    dt.draw();
  }
")

# add indeterminate progress bar to notification
withInfProgress <- function(message, expr, env = parent.frame()) {
  notif_id <- showNotification(
    HTML(paste0(
      "<div class='inf-progress-wrap'>",
      "  <div class='inf-progress-bar'></div>",
      "</div>",
      "<div>", htmltools::htmlEscape(message), "</div>"
    )),
    duration = NULL,
    type = "message"
  )
  tryCatch(
    eval(substitute(expr), envir = env),
    finally = removeNotification(notif_id)
  )
}
