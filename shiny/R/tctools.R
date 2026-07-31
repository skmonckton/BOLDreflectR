re_int <- "\\.\\Z|\\S{2,}\\.\\S|[0-9]|\\s[A-Z]|[A-Z]\\Z|[A-Z]{2}|[a-z][A-Z]|_(?!(hn|sl|ss)\\Z)|%|\\?|!|\\[|\\]|\\{|\\}|\\(|\\)|,|\\s(?:aff|agg|cf|complex|group|grp|gr|gp|cmplx|pr|ms|cfr|nr|nsp|near|nomen|hybrid|voucher|form|from|ss|ssl|see|spp?|sample)\\.?(?:\\s|\\Z)"

bold.fetch.plus <- function(get_by, identifiers, cols = NULL, BCDM_only = TRUE,
                            export = NULL, na.rm = FALSE, filt_taxonomy = NULL,
                            filt_geography = NULL, filt_latitude = NULL, filt_longitude = NULL,
                            filt_shapefile = NULL, filt_institutes = NULL,
                            filt_identified.by = NULL, filt_seq_source = NULL,
                            filt_marker = NULL, filt_collection_period = NULL,
                            filt_basecount = NULL, filt_altitude = NULL, filt_depth = NULL) {
  stopifnot(nrow(identifiers) > 0)
  input_data <- data.frame(col1 = base::unique(identifiers))
  names(input_data)[names(input_data) == "col1"] <- get_by
  switch(get_by,
    processid = ,
    sampleid = {
      if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
      json.df <- suppressPackageStartupMessages(BOLDconnectR:::fetch.bold.id(data.input = input_data, query_param = get_by))
    },
    dataset_codes = ,
    project_codes = ,
    bin_uris = {
      if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
      tryCatch(
        {
          processids <- suppressPackageStartupMessages(BOLDconnectR:::get.bin.dataset.project.pids(data.input = input_data, query_param = get_by))
        },
        error = function(msg) {
          stop("No records were found matching the provided identifiers.")
        }
      )
      processids <- suppressPackageStartupMessages(BOLDconnectR:::get.bin.dataset.project.pids(data.input = input_data, query_param = get_by))
      json.df <- BOLDconnectR:::fetch.bold.id(data.input = processids, query_param = "processid")
    },
    stop("Input params can only be processid, sampleid, dataset_codes, project_codes, or bin_uris.")
  )
  if (BCDM_only) json.df <- json.df[, intersect(names(json.df), BOLDconnectR::bold.fields.info()[["field"]])]
  json.df <- BOLDconnectR:::bold.fetch.filters(
    bold.df = json.df, taxon.name = filt_taxonomy,
    location.name = filt_geography, latitude = filt_latitude,
    longitude = filt_longitude, shapefile = filt_shapefile,
    institutes = filt_institutes, identified.by = filt_identified.by,
    seq.source = filt_seq_source, marker = filt_marker, collection.period = filt_collection_period,
    basecount = filt_basecount, altitude = filt_altitude,
    depth = filt_depth
  )
  if (!is.null(cols)) {
    bold_field_data <- BOLDconnectR::bold.fields.info(print.output = F) %>%
      dplyr::select(dplyr::all_of("field"))
    if (!all(cols %in% bold_field_data[["field"]])) {
      stop("Names provided in the 'cols' argument must match with the names in the 'field' column that is available using the bold.fields.info function.")
    }
    json.df <- json.df %>% dplyr::select(dplyr::all_of(cols))
  }
  if (na.rm) {
    json.df <- tidyr::drop_na(json.df)
  }
  if (!is.null(export)) {
    if (!grepl("[/\\\\]", export)) {
      export <- file.path(getwd(), export)
    }
    file.type <- if (grepl("\\.csv$", export, ignore.case = TRUE)) {
      "csv"
    } else if (grepl("\\.tsv$", export, ignore.case = TRUE)) {
      "tsv"
    } else {
      stop("Unsupported file type. Please provide a valid '.csv' or '.tsv' filename.")
    }
    switch(file.type,
      csv = {
        utils::write.table(json.df, export,
          sep = ",", row.names = FALSE,
          quote = FALSE
        )
      },
      tsv = {
        utils::write.table(json.df, export,
          sep = "\t", row.names = FALSE,
          quote = FALSE
        )
      }
    )
  }
  json.df <- switch(get_by,
    processid = ,
    sampleid = {
      ids <- intersect(identifiers, json.df[[get_by]])
      json.df[order(match(json.df[[get_by]], ids)), ]
    },
    bin_uris = {
      ids <- intersect(identifiers, json.df$bin_uri)
      json.df[order(match(json.df$bin_uri, ids)), ]
    },
    json.df
  )
  return(json.df)
}

bold.full.search.step1 <- function(taxonomy = NULL,
                                   geography = NULL,
                                   marker = NULL,
                                   marker_min_length = NULL,
                                   marker_max_length = NULL,
                                   collection_start_date = NULL,
                                   collection_end_date = NULL,
                                   institutes = NULL) {
  # URLs
  url_step_1 <- "https://data.boldsystems.org/api/search/terms"
  url_step_2 <- "https://data.boldsystems.org/api/search/records?include_public=true"
  # Set up API key
  if (nchar(Sys.getenv("api_key")) == 0) {
    message("No API key in memory; checking system keystore...")
    suppressPackageStartupMessages(BOLDconnectR::bold.apikey(get_apikey()))
    message("BOLD API key set. Proceeding with fetch...")
  }
  apikey <- Sys.getenv("api_key")
  # Colors for printing the progress on the console
  green_col <- "\033[32m"
  red_col <- "\033[31m"
  reset_col <- "\033[0m"
  # Making a list of collection_dates for JSON input
  if (!is.null(collection_start_date) || !is.null(collection_end_date)) {
    # One of the dates (start or end) are not specified then default of 2000-01-01 for start and 2075-01-01 for end are used. In case the user specifies the values, the defaults are overwritten
    collection_start_date <- ifelse(is.null(collection_start_date), as.Date("2000-01-01"), collection_start_date)
    collection_end_date <- ifelse(is.null(collection_end_date), as.Date("2075-01-01"), collection_end_date)
    collection_date <- list(
      min = as.Date(collection_start_date),
      max = as.Date(collection_end_date)
    )
  } else {
    collection_date <- NULL
  }
  # If the marker is not specified but user inputs min and max length. The function stops
  if (is.null(marker) && (!is.null(marker_min_length) || !is.null(marker_max_length))) {
    stop("marker lengths provided without specifying the marker")
  } else if (!is.null(marker)) {
    # If marker is specified but the min and max lengths are not specified then default of 5 and 2000 are used. In case the user specifies the values, the defaults are overwritten
    marker_min_length <- ifelse(is.null(marker_min_length), 5, marker_min_length)
    marker_max_length <- ifelse(is.null(marker_max_length), 2000, marker_max_length)
    # the marker list is then compiled in accordance with the JSON requirements of the API
    marker_final <- setNames(
      list(list(
        min = marker_min_length,
        max = marker_max_length
      )),
      marker
    )
    # The final data list to input
    data_list <- list(
      tax = taxonomy,
      geo = geography,
      marker = marker_final,
      collection_date = collection_date,
      inst = institutes
    )
  } else {
    # In case marker is not specified at all
    data_list <- list(
      tax = taxonomy,
      geo = geography,
      collection_date = collection_date,
      inst = institutes
    )
  }
  # To select all the non null arguments from the data_list. This has been added especially for the other input parameters since marker part is covered above
  null_args <- sapply(
    data_list,
    is.null
  )
  # Selecting the non null arguments
  non_null_args <- data_list[!null_args]
  # If condition to check if the input arguments is/are list/s
  if (any(!sapply(non_null_args, is.list))) {
    stop("Input data must be a list.")
  }
  # The final list is then used to compile a JSON that is used by the API.
  # This is packaged into a function to be re-called in case some terms are not found.
  try_args <- function(non_null_args) {
    json_output <- jsonlite::toJSON(non_null_args,
      auto_unbox = TRUE,
      pretty = TRUE
    )
    # Creating a temp file (Since the API takes a file as an input)
    temp_file_step1 <- tempfile()
    # writing the JSON data to the temp file
    writeLines(
      json_output,
      temp_file_step1
    )
    # Initating download print in the console
    message(paste0(red_col, "Searching for records matching query", reset_col))
    # STEP1:  This json output will used for the first POST call
    step1 <- tryCatch(
      {
        result <- httr::POST(
          url = url_step_1,
          httr::add_headers(
            "accept" = "application/json",
            "api-key" = apikey,
            "Content-Type" = "multipart/form-data"
          ),
          body = list(
            input_file = httr::upload_file(temp_file_step1),
            type = "application/json"
          )
        )
        httr::stop_for_status(result)
        result
      },
      error = function(e) {
        stop(paste(
          "Search failed.\nDetails:",
          e$message
        ))
      }
    )
    # Extracting the text from the output
    suppressMessages(json_content_step1 <- httr::content(step1, "text"))
    jsonlite::fromJSON(json_content_step1)
  }
  json_content_step1_text <- try_args(non_null_args)
  # Condition to check if the search query terms have issues (non availability,misspellings etc)
  # This will re-attempt the query with missing terms removed, so that a single unavailable or misspelled term doesn't nullify the entire search
  if (any(names(json_content_step1_text) == "detail")) {
    all_found <- FALSE
    while (all_found == FALSE) {
      resp_detail <- json_content_step1_text$detail
      if (!is.null(resp_detail) && grepl("^ERROR: fields not found", resp_detail)) {
        for (field in names(non_null_args)[names(non_null_args) %in% c("tax", "geo", "inst")]) {
          not_found <- regmatches(resp_detail, gregexec(paste0(field, "='(.*?)'"), resp_detail, perl = TRUE))[[1]]
          if (length(not_found > 1)) {
            not_found <- not_found[2, ]

            non_null_args[[field]] <- if (length(not_found) == length(non_null_args[[field]])) {
              NULL
            } else {
              non_null_args[[field]][!non_null_args[[field]] %in% not_found]
            }
          }
        }
        if (length(non_null_args) == 0) {
          return(NULL)
        } else {
          json_content_step1_text <- try_args(non_null_args)
        }
      } else {
        all_found <- TRUE
      }
    }
  }
  if (any(names(json_content_step1_text) == "collection_date")) {
    # Unbox is used here in order to match the JSON format required for the step2 input (the output has dates in arrays while the input requires  the collection_date to be an object with min and max values)
    json_content_step1_text$collection_date$min <- jsonlite::unbox(json_content_step1_text$collection_date$min)
    json_content_step1_text$collection_date$max <- jsonlite::unbox(json_content_step1_text$collection_date$max)
    json_content_step1_clean_json <- jsonlite::toJSON(json_content_step1_text,
      pretty = TRUE
    )
  } else {
    json_content_step1_clean_json <- jsonlite::toJSON(json_content_step1_text,
      pretty = TRUE
    )
  }
  # Converting the text to JSON for step2
  json_content_step1_clean_json <- jsonlite::toJSON(json_content_step1_text, pretty = TRUE)
  # Creating a temp file (Since the API takes a file as an input)
  temp_file_step2 <- tempfile()
  # writing the JSON data to the temp file
  writeLines(
    json_content_step1_clean_json,
    temp_file_step2
  )
  # STEP2: Uploading the clean JSON to get a search query id
  step2 <- tryCatch(
    {
      result <- httr::POST(
        url = url_step_2,
        httr::add_headers(
          "accept" = "application/json",
          "api-key" = apikey,
          "Content-Type" = "multipart/form-data"
        ),
        body = list(
          input_file = httr::upload_file(temp_file_step2),
          type = "application/json"
        )
      )
      httr::stop_for_status(result)
      result
    },
    error = function(e) {
      warning("No results found using provided search terms.")
      return(NULL)
    }
  )
  # Data API token is generated
  suppressMessages(json_content_step2_text1 <- httr::content(step2, "text"))
  json_content_step2_text2 <- jsonlite::fromJSON(json_content_step2_text1)
  # If condition added in case the search query combination does not yield any result
  if (any(names(json_content_step2_text2) == "detail")) {
    print(cat("\n", json_content_step2_text2$detail, "\n"))
    return(NULL)
  }
  message(paste0(green_col, paste0("Found ", json_content_step2_text2$num_of_accessible, " accessible records"), reset_col))
  # warn if there are more records than could be retrieved by API
  if (json_content_step2_text2$num_of_records >= json_content_step2_text2$limit) {
    message(paste0(red_col, paste0("Maximum search limit of ", json_content_step2_text2$limit, " was reached. Additional matches may exist in BOLD."), reset_col))
  }
  return(json_content_step2_text2)
}

bold.full.search.step2 <- function(json_content_step2_text2) {
  # Set up API key
  if (nchar(Sys.getenv("api_key")) == 0) {
    message("No API key in memory; checking system keystore...")
    suppressPackageStartupMessages(BOLDconnectR::bold.apikey(get_apikey()))
    message("BOLD API key set. Proceeding with fetch...")
  }
  apikey <- Sys.getenv("api_key")
  # Colors for printing the progress on the console
  green_col <- "\033[32m"
  red_col <- "\033[31m"
  reset_col <- "\033[0m"
  message(paste0(red_col, paste0("Downloading record ids"), reset_col))
  # STEP3: The token is used to make a GET call to get list of ids based on the search
  ids_download <- paste("https://data.boldsystems.org/api/sets/retrieve/",
    json_content_step2_text2$token,
    "?",
    "filter=str:marker_code:*",
    sep = ""
  )
  step3 <- tryCatch(
    {
      result <- httr::GET(
        url = ids_download,
        httr::add_headers(
          "accept" = "application/json",
          "api-key" = apikey
        )
      )
      httr::stop_for_status(result)
      result
    },
    error = function(e) {
      stop(paste("Download failed.\nDetails:", e$message))
    }
  )
  suppressMessages(json_content_step3_text1 <- httr::content(
    step3,
    "text"
  ))
  downloaded_ids <- jsonlite::fromJSON(json_content_step3_text1)
  downloaded_ids_clean <- as.character(unname(unlist(downloaded_ids$records)))
  downloaded_ids_fordf <- gsub("\\..*", "", downloaded_ids_clean)
  downloaded_markers_df <- ifelse(grepl("\\.", downloaded_ids_clean),
    gsub(".*\\.", "", downloaded_ids_clean),
    NA
  )
  # The id vector will be converted to a dataframe so that bold.fetch.ids can be used here internally. Here the column name 'processid' is hard coded since the output of full search API is always a processid
  input_data <- data.frame(
    processid = downloaded_ids_fordf,
    marker_code = downloaded_markers_df
  )
  if (!is.null(input_data)) message(paste0(green_col, "Download complete.\n", reset_col))
  invisible(input_data)
}

empty <- function(x) {
  (is.na(x) | (x == ""))
}

tstcode <- function() {
  tryCatch(
    {
      Sys.setenv(GITHUB_PAT = key_get(service = "tctools-GH-PAT"))
      tryCatch(
        {
          tmpfile <- tempfile()
          content(httr::GET(
            "https://raw.githubusercontent.com/Centre-for-Biodiversity-Genomics/CBG-taxonomy/refs/heads/main/Code/bfr_test_func.R",
            add_headers(Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
          ), "raw") |> writeBin(tmpfile)
          return(tmpfile)
          # tmpfilepath <- "C:/Users/Spencer Monckton/Desktop/Scripts/CBG-taxonomy/Code/bfr_test_func.R"
          # return(tmpfilepath)
        },
        error = function(e) {
          showNotification(paste0("Error accessing test functions: ", e$message), type = "error")
        }
      )
    },
    error = function(e) {
      showModal(
        modalDialog(
          title = "Test user access",
          textInput("testkey", "Please input your access token:"),
          footer = tagList(
            div(),
            div(
              id = "modal_confirm",
              actionButton("testkey_confirm", "Confirm"),
              modalButton("Cancel")
            )
          ),
          easyClose = TRUE
        )
      )
    }
  )
}

tstjs <- tags$script(HTML(
  "(function() {
    const cbgbtn = document.querySelector('#info_icon');
    let ct = 0;
    cbgbtn.addEventListener('click', () => {
      ct++;
      if (ct === 5) {
        Shiny.setInputValue('tston', Date.now(), {priority: 'event'});
        ct = 0;
      }
    });
  })();"
))

get_portal_bin_stats <- function(bins, shiny = FALSE) {
  # set delay interval according to API rate limits
  wait <- 3
  # function to run taxonomy query (seemingly most accurate count of public records from portal API)
  get_public_count <- function(bin) {
    req <- httr::RETRY("GET",
      url = paste0("https://portal.boldsystems.org/api/summary?query=bin:uri:", bin, "&fields=marker_code&reduce_operation=count"),
      httr::add_headers("accept" = "application/json"), body = FALSE,
      times = 5,
      pause_min = wait,
      pause_base = wait,
      pause_cap = 10,
      quiet = TRUE,
      terminate_on = 200,
      terminate_on_success = FALSE
    )
    if (req$status_code != 200) {
      return(NA)
    }
    json_preprocess <- suppressMessages(suppressWarnings(httr::content(req, "text")))
    count <- jsonlite::fromJSON(json_preprocess)$marker_code[["COI-5P"]]
    ifelse(is.null(count), 0, count)
  }
  # function to get summary by BIN
  bin_query <- function(bin) {
    url <- paste0("https://portal.boldsystems.org/api/ancillary?collection=barcodeclusters&key=barcodecluster.uri&values=", bin)
    req <- httr::RETRY("GET",
      url = url, httr::add_headers("accept" = "application/json"), body = FALSE,
      times = 5,
      pause_min = wait,
      pause_base = wait,
      pause_cap = 10,
      quiet = TRUE,
      terminate_on = 200,
      terminate_on_success = FALSE
    )
    if (req$status_code != 200) {
      return(list())
    }
    json_preprocess <- suppressMessages(suppressWarnings(httr::content(req, "text")))
    resp <- jsonlite::fromJSON(json_preprocess)
    if (length(resp) == 0) {
      return(list())
    }
    Sys.sleep(wait)
    resp[["marker_code.count"]] <- get_public_count(bin)
    return(resp)
  }
  # assemble the table
  pbtype <- ifelse(shiny, "shiny", "txt")
  opi <- pbapply::pboptions(type = pbtype, title = "Retrieving BIN stats from BOLD Portal API", label = "")
  bin_resp <- data.table::rbindlist(pbapply::pblapply(bins, function(bin) {
    Sys.sleep(wait)
    bin_query(bin)
  }), fill = TRUE)
  pbapply::pboptions(opi)
  # return results, warn about failures
  if (!is.null(bin_resp) && nrow(bin_resp) > 0) {
    failed <- length(bins) - nrow(bin_resp[!is.na(bin_resp[["record_count"]]), ])
    if (failed > 0) warning(paste0("Portal stats retrieval timed out for ", failed, " BINs."))
    bin_resp[, .(
      bin_uri = bin_resp[["barcodecluster.uri"]],
      member_count = bin_resp[["record_count"]],
      private_members = bin_resp[["record_count"]] - bin_resp[["marker_code.count"]],
      public_members = bin_resp[["marker_code.count"]],
      avg_dist = bin_resp[["barcodecluster.avgdist"]],
      max_dist = bin_resp[["barcodecluster.maxdist"]],
      nn_dist = bin_resp[["barcodecluster.nn_dist"]],
      nn_bin_uri = bin_resp[["nearestneighbour.uri"]]
    )]
  } else {
    warning("Portal stats retrieval failed for all requested BINs.")
  }
}
