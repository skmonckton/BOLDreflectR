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
library(shinyWidgets)
library(writexl)
library(yaml)

source("R/tctools.R")
source("R/bold.full.search.two.step.R")

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

# function to save user options
save_user_config <- function() {
  write_yaml(user_config, file.path(user_data_path,"user_config.yaml"))
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
         "All fields" = config$fieldsets$all)
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
         "All fields" = intersect(config$fieldsets$all, c(config$fieldsets$bcdm, config$fieldsets$parsed)))
  }
  
} 

marker_options <- list(" " = "",
                       Primary = stats::setNames(config$markercodes$major, config$markercodes$major),
                       Other = stats::setNames(config$markercodes$other, config$markercodes$other))

validate_apikey <- function(key) {
  stored <- Sys.getenv("api_key")
  try(stored <- key_get("BOLD.apikey"), silent=TRUE)
  if ((stored != key) | (all(stored == "", key == ""))) {
    if(key != "") {
      tryCatch({
        bold.apikey(key)
        shinyjs::hide("api_key")
      }, error = function(e) {
        showNotification(e$message, id="error", id="fetch_msg", type = "error")
      })
    } else {
      Sys.setenv(api_key = "")
      shinyjs::show("api_key")
      shinyjs::reset("api_key")
    }
    try(key_set_with_value("BOLD.apikey", password = key), silent = TRUE)
  }
}

# copy to clipboard with size limit
cb <- function(df, header=TRUE, sep="\t", na="", max.size=(200*1000)){
  os <- Sys.info()["sysname"]
  text <- capture.output(
    write.table(df, stdout(), sep=sep, na=na, col.names=header, row.names=FALSE, quote=FALSE)
  )
  if (os == "Windows") {
    con <- file(paste0("clipboard-", formatC(max.size, format="f", digits=0)), open="w")
  } else if (os == "Darwin") {
    con <- pipe("pbcopy", "w")
  } else {
    con <- pipe("xclip -selection clipboard", "w")
  }
  writeLines(text, con)
  close(con)
  showNotification("Copied to clipboard.", type="message", id="copy_msg")
}

# assemble FASTA and copy to clipboard
clip_fasta <- function(bold_df,
                       cols_for_fas_names = NULL,
                       collapse_mrkrs = NULL,
                       to_cb = TRUE) {
  
  cols_for_fas_names <- if (!is.null(cols_for_fas_names)) {
    cols_for_fas_names
  } else if (!is.null(user_config$settings$fasta_head)) {
    user_config$settings$fasta_head
  } else {
    c("processid", "marker_code")
  }
  
  if (any(duplicated(bold_df$processid)) && !any(c("marker_code", "record_id") %in% cols_for_fas_names)) {
    showNotification("Some records have multiple markers. Output FASTA will include marker in headers.", duration = 6, type = "warning")
    cols_for_fas_names <- c(cols_for_fas_names, "marker_code")
  }
  
  clip_fas <- if((!"nuc" %in% names(bold_df)) || (nrow(bold_df[!empty(nuc)]) == 0)) {
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
           bold_df[!empty(nuc), do.call(paste, c(.SD, sep = "|")), .SDcols = c(cols_for_fas_names)],
           "\n",
           bold_df[!empty(nuc), nuc])
  }
  
  
  
  if(is.null(clip_fas)) {
    showNotification(paste0("No sequences to copy."), id="copy_msg", type = "error")  
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

# split and clean query terms
split_query <- function(query_input, list = FALSE) {
  if(query_input == "") {
    NULL
  } else {
    terms <- trimws(strsplit(query_input, "\n|,")[[1]])
    terms <- unique(terms[!empty(terms)])
    if(list == TRUE) {
      as.list(terms)
    } else {
      terms
    }
  }
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

# decompose recordset column into a list of dataset or project codes
list_recordsets <- function(data, set_type, list_by) {
  recordsets <- data[, .(set = unique(trimws(trimws(unlist(strsplit(bold_recordset_code_arr, ",")))))), by = list_by] 
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
    by_id <- unique(setorderv(unique(data, by = list_by)[, .(identification, .N), by = ranks], ranks))
    tax_count <- rbindlist(lapply(seq_along(ranks), function(i) {
      r <- ranks[i]
      by_rank <- by_id[, .(count = sum(N)), by = eval(ranks[seq_len(i)])]
      by_rank[get(r) %in% data$identification]
    }), fill = TRUE)
    tax_count[, (ranks) := lapply(.SD, as.character), .SDcols = ranks]
    tax_count[, taxon := fcoalesce(.SD), .SDcols = rev(ranks)]
    setorderv(tax_count, ranks)[, .SD, .SDcols = c("taxon", ranks, "count")]
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

# extracts lats and longs from the BCDM coord column
parse_lat_lon <- function(coord) {
  if(all(empty(coord))) {
    list(rep(as.numeric(NA), length(coord)), rep(as.numeric(NA), length(coord)))
  } else {
    lapply(tstrsplit(coord, ",", fixed = TRUE), as.numeric)
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
  
  data <- df[!empty(bin_uri)]
  bp_col <- head(c("nuc_basecount", "COI-5P_nuc_basecount")[c("nuc_basecount", "COI-5P_nuc_basecount") %in% names(df)], 1)

  # get series of distances between sequence length and in-frame modal barcode length for BIN
  diff_mode <- merge(data,
                     data[, .(mode = deduce_len_in_frame(get(bp_col))), by = "bin_uri"],
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

# check for search terms in retrieved data
get_query_hits <- function(data = NULL, query) {
  
  search_terms <- query[names(config$queryterms)[names(config$queryterms) %in% names(query)]]

  query_dt <- rbindlist(lapply(names(search_terms), function(field) {
    data.table(field = field, term = unlist(search_terms[[field]]), found = "NOT FOUND", count = 0)
  }), fill = TRUE)
  
  qfields <- unique(query_dt$field)
  subheads <- list()
  tables <- list()
  
  for(qfield in qfields) {
    search_cols <- config$queryterms[[qfield]]$fields
    qterms <- unique(query_dt[field == qfield, .(term)])
    
    lookup_dt <- if(all(search_cols == "bold_recordset_code_arr")) { 
      data[, .(bold_recordset_code_arr = unique(trimws(trimws(unlist(strsplit(bold_recordset_code_arr, ",")))))), by = .I]  
    } else {
      data
    }
    
    matched_terms <- unique(unlist(lapply(search_cols, function(col) {
      lookup_dt[qterms, on = setNames("term", col), nomatch = NULL][[col]]
    })))
    
    counts_long <- unique(rbindlist(lapply(search_cols, function(col) {
      count <- lookup_dt[qterms, on = setNames("term", col), .N, by = .EACHI]
      setnames(count, 1, "term") 
      return(count)
    }), fill = TRUE))
    total_counts <- counts_long[, .(N = sum(N)), by = "term"]
    total_counts [, field := qfield]
    
    query_dt[field == qfield, found := fifelse(term %in% matched_terms, "FOUND", "NOT FOUND")]
    query_dt[total_counts, on = .(field, term), count := count + i.N]
    
    subheads[[qfield]] <- paste0(config$queryterms[[qfield]]$name, ":")
    tables[[qfield]] <- query_dt[field == qfield, .(term, found, count)]
  }
  
  report <- do.call(tagList, lapply(qfields, function(qfield) {
    tagList(div(class = "hit_rep_heading", subheads[[qfield]]), 
            DT::renderDataTable(DT::datatable({
              tables[[qfield]]
            },
            rownames = FALSE,
            options = list(
              dom = 'ft',
              autoWidth = FALSE,
              paging = FALSE,
              columnDefs = list(
                list(className = 'dt-left', width = '20rem', targets = 0), 
                list(className = 'dt-center', width = '6rem', targets = c(1, 2)
                )))) %>%
              DT::formatStyle(
                'found',
                color = DT::styleEqual(c("FOUND", "NOT FOUND"), c('#33691e', '#c62828')),
                fontWeight = "500"
              )))
    }))
  
  list(hits_dt = query_dt,
       report = report)
}

# retrieve BIN mates
get_binmates <- function(df, dtpkg_parquet = NULL, batch_size=200, sleep=2) {
  bins <- unique(as.character(df[["bin_uri"]][!empty(as.character(df[["bin_uri"]]))]))
  add_pids <- character()
  
  if(length(bins) == 0) {
    showNotification("No BINs were found in the fetched data.", id = "bin_msg", duration=2, type = "message")
  } else {
    batches = ceiling(length(bins) / batch_size)
    for (r in 1:batches) {
      start = (r-1) * batch_size + 1
      end = min(r * batch_size, length(bins))
      getids <- bins[start:end]
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
    ")
header_js <- DT::JS("
  function(thead, data, start, end, display) {
    window.copyCol=  function(i) {
      Shiny.setInputValue('col_copy_clicked', { col: i }, { priority: 'event' });
    }
    $(thead).find('th').each(function(i) {
      if ($(this).find('.col-copy-btn').length > 0) return;
      var colName = $(this).text();
      $(this).empty()
      .append('<span class=\"header-label\">' + colName + '</span>')
      .append('<button class=\"col-copy-btn\" onclick=\"event.stopPropagation(); window.copyCol(' + i + ');\"><i class=\"far fa-copy\"></i></button>');
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
                          return data
                            ? '<div><a href=\"https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSequence?processid=' + data + '\" target=\"_blank\">' + data + '</a></div>'
                            : '<div>' + data + '</div>';
                        }
                        if (colName == 'bin_uri' || colName == 'nn_bin_uri') {
                          return data
                          ? '<div><a href=\"https://bench.boldsystems.org/index.php/Public_BarcodeCluster?clusteruri=' + data + '\" target=\"_blank\">' + data + '</a></div>'
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

withInfProgress <- function(message, expr, env = parent.frame()) {
  notif_id <- showNotification(
    HTML(paste0(
      "<div class='inf-progress-wrap'>",
      "  <div class='inf-progress-bar'></div>",
      "</div>",
      "<div>", message, "</div>"
    )),
    duration = NULL,
    type = "message"
  )
  tryCatch(
    eval(substitute(expr), envir = env),
    finally = removeNotification(notif_id)
  )
}
