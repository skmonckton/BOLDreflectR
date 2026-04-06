library(BOLDconnectR)
library(data.table)
library(httr)
library(keyring)
library(leaflet)
library(lubridate)
library(pbapply)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(writexl)
library(yaml)

library(duckdb)
library(dplyr)
library(DBI)
library(dbplyr)
library(tidyr)
library(sf)

source("R/tctools.R")
source("R/bold.full.search.two.step.R")
for(s in list.files("R/node/", full.names = TRUE)) source(s)

keystore <- switch(Sys.info()['sysname'],
                  Windows = "Windows Credential Manager",
                  macOS = "macOS Keychain",
                  Linux = "Linux Secret Service")

# in the packaged app, these values are passed in during start-up by electron
ver <- ifelse(Sys.getenv("APP_VER") != "", Sys.getenv("APP_VER"), jsonlite::read_json("../package.json")$version)
user_data_path <- ifelse(Sys.getenv("USER_DATA_PATH") != "", Sys.getenv("USER_DATA_PATH"), "user_data")

# initialize user_config file in user data directory
# (currently only for custom field sets)
user_config <- if(file.exists(file.path(user_data_path,"user_config.yaml"))) {
  read_yaml(file.path(user_data_path,"user_config.yaml"))
} else {
  list(fieldsets = list(custom = c()))
}

config <- read_yaml("data/config.yaml", readLines.warn=FALSE)

ranks <- names(config$ranknumeric) 

map_colours <- colorRampPalette(c("#9e0142","#d53e4f","#f46d43","#fdae61",
                                  "#fee08b","#ffffbf","#e6f598","#abdda4",
                                  "#66c2a5","#3288bd","#5e4fa2"), alpha = TRUE)(11)

# this is used to re-compute selectize input options when custom filter sets are created or removed
filter_options <- function() {
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
                       "Project code" = "parsed|project_code"),
       "All fields" = config$fieldsets$all)
} 

marker_options <- list(" " = "",
                       Primary = stats::setNames(config$markercodes$major, config$markercodes$major),
                       Other = stats::setNames(config$markercodes$other, config$markercodes$other))

# datapkgs <- user_config$datapackages

update_datapackages <- function() {
  datapkgs <- data.table(file.info(list.files(path = file.path(user_data_path, "BOLD_data_packages"), full.names = TRUE))[c("size", "ctime")],
                         keep.rownames = TRUE)[, .(id = tools::file_path_sans_ext(basename(rn)),
                                                   path = rn, size_kb = format(size, scientific = FALSE),
                                                   created = format(ctime, "%Y-%m-%d"))]
  user_config[["datapackages"]] <- apply(datapkgs, 1, as.list)
  write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
  return(datapkgs)
}

datapkgs <- update_datapackages()

# datapkgs <- rbindlist(list(datapkgs, datapkgs))

#datapkgs <- data.table(do.call(unlist(cbind), user_config[["datapackages"]]))

#datapkgs <- rbindlist(user_config[["datapackages"]])

#if(nrow(datapkgs) > 0) datapkgs <- datapkgs[, .(id, path, size_kb = as.numeric(size_kb), created = as.Date(created, format = "%Y-%m-%d"))]

# copy to clipboard with size limit
cb <- function(df, header=TRUE, sep="\t", max.size=(200*1000)){
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, col.names=header, row.names=FALSE, quote=FALSE, na="NA")
  showNotification(paste0("Copied to clipboard."), type = "message", id = "copy_msg")
}

# convenience function to check for NA or ""
empty <- function(x) {
  (is.na(x) | (x == ""))
}

# assemble FASTA and copy to clipboard
clip_fasta <- function(bold_df, cols_for_fas_names = NULL, collapse_mrkrs = NULL) {
  
  if (is.null(cols_for_fas_names)) cols_for_fas_names <- c("processid", "marker_code")
  
  clip_fas <- if((!"nuc" %in% names(bold_df)) || (nrow(bold_df[!is.na(nuc)]) == 0)) {
    if(!is.null(collapse_mrkrs)) {
      showNotification(paste0("Copying FASTA..."), id="copy_msg", type = "message")
      cols_for_fas_names <- intersect(cols_for_fas_names, names(bold_df))
      mrkrs <- sapply(collapse_mrkrs, function(x) unlist(strsplit(x, "_"))[1])
      fas <- NULL
      for(i in seq_along(mrkrs)) {
        fas <- c(fas, paste0(">",
                             bold_df[!is.na(get(names(mrkrs)[i])), do.call(paste, c(.SD, unname(mrkrs[i]), sep = "|")), .SDcols = c(cols_for_fas_names)],
                             "\n",
                             bold_df[!is.na(get(names(mrkrs)[i])), get(names(mrkrs)[i])]))
      }
      fas
    } else {
      NULL
    }
  } else {
    showNotification(paste0("Copying FASTA..."), id="copy_msg", type = "message")
    paste0(">",
           bold_df[!is.na(nuc), do.call(paste, c(.SD, sep = "|")), .SDcols = c(cols_for_fas_names)],
           "\n",
           bold_df[!is.na(nuc), nuc])
  }
  
  if(is.null(clip_fas)) {
    showNotification(paste0("No sequences to copy."), id="copy_msg", type = "error")  
  } else {
    tryCatch(
      cb(unlist(strsplit(clip_fas,"\n")), header = FALSE, sep = ""),
      error = function(e) {
        showNotification(paste0("FASTA output too large. Please try again with fewer sequences."), id="copy_msg", type = "error")
      }
    )
  }
}

# split and clean query terms
split_query <- function(query_input, list = FALSE) {
  if(query_input == "") {
    NULL
  } else {
    terms <- trimws(strsplit(query_input, "\n|,")[[1]])
    terms <- terms[(!is.na(terms)) & (terms != "")]
    if(list == TRUE) {
      as.list(terms)
    } else {
      terms
    }
  }
}

# save or delete local data packages
modify_datapackage <- function(dtpkg, operation = "save") {
  if(operation == "save") {
    outdir <- file.path(user_data_path, "BOLD_data_packages")
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    save_id <- tools::file_path_sans_ext(basename(dtpkg))
    save_path <- file.path(outdir, basename(dtpkg))
    if(save_id %in% datapkgs$id) {
      showNotification("There is already a data package with this ID. If you wish to overwrite the existing file, please delete it first.", id = "fileio", type = "error")
    } else {
      file.copy(dtpkg, save_path)
      datapkg <- data.table(file.info(dtpkg)[c("size", "ctime")],
                            keep.rownames = TRUE)[, .(id = save_id, path = save_path,
                                                      size_kb = format(size, scientific = FALSE),
                                                      created = format(ctime, "%Y-%m-%d"))]
      datapkgs <<- rbindlist(list(datapkgs, datapkg))
      user_config[["datapackages"]] <- apply(datapkgs, 1, as.list)
      write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
      showNotification("Data package saved!", id = "fileio", type = "message")
    }
  } else if(operation == "delete") {
    del_path <- datapkgs[id == dtpkg, path]
    datapkgs <<- datapkgs[id != dtpkg]
    user_config[["datapackages"]] <- apply(datapkgs, 1, as.list)
    file.remove(del_path)
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
    custom_config[[idx]] <- list(id = cfg_id, name = set_name, fields = int_fields)
    user_config$fieldsets$custom <<- custom_config
    # value with which to update filter selectize input
    new_select <- sprintf("userset%03d", idx)
  }
  # update config file
  write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
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

# decompose recordset column into a list of dataset or project codes
list_recordsets <- function(data, set_type, list_by) {
  recordsets <- data[, .(set = unique(trimws(unlist(strsplit(gsub("\\'|\\[|\\]", "", bold_recordset_code_arr), ","))))), by = list_by] 
  if(set_type == "parse_project") {
    merge(data[, list_by, with = FALSE], unique(recordsets[!grepl("^DS-|^DATASET", set), c(list_by, "set"), with = FALSE]), all.x = TRUE, by = list_by)[, set]
  } else if(set_type == "projects") {
    recordsets[!grepl("^DS-|^DATASET", set), .(count = .N), by = .(project_code = set)][order(-count)]
  } else {
    recordsets[grepl("^DS-|^DATASET", set), .(count = .N), by = .(dataset_code = set)][order(-count)]
  }
}

# computes the summary table
summarize_table <- function(data, sum_type, list_by) {
  if (sum_type %in% c("projects","datasets")){
    list_recordsets(data, sum_type, list_by)
  } else if (sum_type == "tax_summary") { 
    tax_count <- unique(setorderv(unique(data, by = list_by)[, .(identification, count = .N), by = ranks], ranks))
    tax_count[, .SD, .SDcols = c("identification", ranks, "count")]
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

# extracts identification dates from taxonomy_notes
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

# parse_id_date <- function(data) {
#   str_match <- function(x, pat) regmatches(x, gregexpr(pat, x, perl=TRUE), invert = NA)
#   id_date_verb <- lubridate::my(sapply(str_match(data$specimendetails.verbatim_identification_method, "(?<=\\()[A-Za-z]{3,9} [0-9]{2,4}(?=\\))"), `[`, 2))
#   id_date_note <- lubridate::my(sapply(str_match(data$taxonomy_notes, "(?<=id-date:\\s?)[A-Za-z]{3,9} [0-9]{2,4}"), `[`, 2))
#   id_date <-  format(as.Date(do.call(pmax, c(data.table(id_date_verb,id_date_note), na.rm = TRUE)), format = "%Y-%m-%d"), format = "%b %Y")
#   return(id_date)
# }

# extracts lats and longs from the BCDM coord column
parse_lat_lon <- function(coord) {
  if(all(is.na(coord))) {
    list(rep(as.numeric(NA), length(coord)), rep(as.numeric(NA), length(coord)))
  } else {
    lapply(tstrsplit(gsub("\\[|\\]|\\s", "", coord), ",", fixed = TRUE), as.numeric)
  }
}

# rounds to a multiple, with optional offset (e.g. to account for reading frame)
round_to <- function(x, mult = 1, offset = 0) {
  (round((x+offset)/mult) * mult) - offset
}

# deduces modal sequence length; in case of multiple modes, the value closest to 658 is selected
deduce_len_in_frame <- function(x) {
  t <- table(sapply(x, round_to, mult = 3, offset = -1))
  modal <- names(t)[t == max(t)]
  as.numeric(unname(modal)[which.min(sapply(modal, function (x) abs(as.numeric(x) - 658)))])
}

# selected BIN representatives
get_bin_reps <- function(df) {
  
  data <- df[(!is.na(bin_uri))]
  bp_col <- head(c("nuc_basecount", "COI-5P_nuc_basecount")[c("nuc_basecount", "COI-5P_nuc_basecount") %in% names(df)], 1)

  # get series of distances between sequence length and in-frame modal barcode length for BIN
  diff_mode <- merge(data,
                     data[!is.na(bin_uri), .(mode = deduce_len_in_frame(get(bp_col))), by = "bin_uri"],
                     by = "bin_uri",
                     all.x = TRUE)[, abs(get(bp_col) - mode)]
  
  # get Boolean indexers for other features
  inst_pref <- grepl("Centre for Biodiversity Genomics", data$inst)
  cbg_pref <- grepl("BIOUG|CBG", data$sampleid)
  id_pref <- grepl("(?i)morphology|(?i)image", data$identification_method, perl=T)
  gb_avoid <- grepl("(?i)GenBank", data$inst, perl=T)
  
  # sort data and select first row for each unique BIN/taxon combination
  data[data[order(gb_avoid, diff_mode, -id_pref, -inst_pref, -cbg_pref, -collection_date_start), .I[head(1)], by = c("bin_uri", "identification")]$V1, paste0(processid, ".COI-5P")]
  
}

clean_ansi <- function(x) {
  gsub("\033\\[[0-9;]*[mK]", "", x)
}

# wrapper to prepare BOLD fetch/search queries and prepare messages as notifications
# (note: was intended to handle both fetch and search, but I haven't harmonized the two yet)
bold.fetch.shiny <- function (get_by,
                              query,
                              BCDM_only = FALSE,
                              notification_id = "fetch_msg") {

  tryCatch({
    withCallingHandlers({
      if(get_by == "search") {
        recs <- bold.full.search(taxonomy = query[["tax"]], geography = query[["geo"]])
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
    showNotification(paste("Error fetching data:", clean_ansi(conditionMessage(e))), id = notification_id, duration = NULL, type = "error")
    return(NULL)
  })
}

# retrieve process IDs of BIN mates
get_binmate_pids <- function(df, batch_size=200, sleep=2) {
  bins <- unique(df[["bin_uri"]][(!is.na(df[["bin_uri"]])) & (df[["bin_uri"]] != "")])
  
  add_pids <- character()
  
  if(length(bins) == 0) {
    showNotification("No BINs were found in the fetched data.", id = "bin_msg", duration=2, type = "message")
  } else {
    batches = ceiling(length(bins) / batch_size)
    for (r in 1:batches) {
      start = (r-1) * batch_size + 1
      end = min(r * batch_size, length(bins))
      getids <- levels(bins[start:end])
      showNotification(paste0("Checking for additional BIN members from ",length(bins)," BINs... (Batch ",r," of ",batches,")"), id = "bin_msg", duration=NULL, type = "message")
      out <- NA
      fill <- NA
      sleepmore = sleep
      while(!is.data.frame(out)){
        tryCatch({
          out <- BOLDconnectR:::get.bin.dataset.project.pids(data.input = unique(data.frame(col1 = getids)), query_param = "bin_uris")
        },
        error = function(e) {
          showNotification(paste0("API returned an error: ", e$message, "\nReattempting batch ",r,"..."), type = "warning")
          sleepmore = sleepmore + 1
          Sys.sleep(sleepmore)
        })
      }
      add_pids <- sort(unique(c(add_pids, setdiff(out[["records"]], df[["processid"]]))))
      Sys.sleep(sleep)
    }
    
    if(length(add_pids) == 0) {
      showNotification(paste0("No additional BIN members found."), id = "bin_msg", type = "message")
    } else {
      showNotification(paste0(length(add_pids), " additional BIN members found."), id = "bin_msg", type = "message")
    }
  }
  
  return(add_pids)
}

# JavaScript used to render output data tables
callback_js <- DT::JS("
      window.mapBySID = window.mapBySID || {};
      window.mapByPID = window.mapByPID || {};
    ")
header_js <- DT::JS("
  function(thead, data, start, end, display) {
    window.copyCol=  function(i) {
      Shiny.setInputValue('col_copy_clicked', { col: i }, { priority: 'event' });
    }
    $(thead).find('th').each(function(i) {
      var colName = $(this).text();
      $(this).html(
        '<span class=\"header-label\">' + colName +
        '</span><button class=\"col-copy-btn\" onclick=\"event.stopPropagation(); window.copyCol(' + i + ');\"><i class=\"far fa-copy\" role=\"presentation\" aria-label=\"copy icon\"></i></button>'
      );
    });
  }
")
column_js <- DT::JS("
                    function(data, type, row, meta) {
                      if (type === 'display' && data !== null) {
                        var colName = meta.settings.aoColumns[meta.col].sTitle;
                        if (colName == 'sampleid') {
                          var pid = mapBySID[data];
                          return pid
                            ? '<div><a href=\"https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSpecimen?selectedrecordid=' + pid + '\" target=\"_blank\">' + data + '</a></div>'
                            : '<div>' + data + '</div>';
                        }
                        if (colName == 'processid') {
                          var spid = mapByPID[data];
                          return spid
                            ? '<div><a href=\"https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSequence?selectedrecordid=' + spid + '\" target=\"_blank\">' + data + '</a></div>'
                            : '<div>' + data + '</div>';
                        }
                        if (colName == 'bin_uri' || colName == 'nn_bin_uri') {
                          return '<div><a href=\"https://bench.boldsystems.org/index.php/Public_BarcodeCluster?clusteruri=' + data + '\" target=\"_blank\">' + data + '</a></div>';
                        }
                        if (colName == 'avg_dist' || colName == 'max_dist' || colName == 'nn_dist') {
                          return '<div>' + parseFloat(data).toFixed(3) + '</div>';
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
