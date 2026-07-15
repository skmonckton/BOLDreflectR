server <- function(input, output, session) {
  ### Initialization ----

  session$onSessionEnded(function() {
    stopApp()
  })

    observeEvent(TRUE, {
      key <- ""
      try(key <- key_get("BOLD.apikey"), silent=TRUE)
      validate_apikey(key)
    }, ignoreInit = FALSE, once = TRUE)
    
  bslib::accordion_panel_close("more_opts", "Additional fields")

  # some manual JavaScript for placeholder values and multiple marker search
  session$onFlushed(function() {
    shinyjs::runjs("
        function setPlaceholders() {
          $('.single-marker div:has(.shiny-input-number):nth-child(1 of div:has(.shiny-input-number)) .shiny-input-number').attr('placeholder', 'Min');
          $('.single-marker div:has(.shiny-input-number):nth-last-child(1 of div:has(.shiny-input-number)) .shiny-input-number').attr('placeholder', 'Max');
        }

        var lastSent = null;

        function reportValues() {
          var vals = $('.tab-pane[data-value=\"source-local\"] .single-marker .shiny-input-select option').map(function() {
            return $(this).val();
          }).get();
          var serialized = JSON.stringify(vals);
          console.log('serialized:', serialized, 'lastSent:', lastSent);
          if (serialized === lastSent) return;
          lastSent = serialized;
          Shiny.setInputValue('node_markers', vals, {priority: 'event'});
        }

        setPlaceholders();
        reportValues();

        $(document).on('change', '.seq-value-input input', function() {
          reportValues();
        });

        var observer = new MutationObserver(function() {
          setPlaceholders();
          reportValues(); // re-emit when DOM changes (inputs added/removed)
        });

        observer.observe(document.body, { childList: true, subtree: true });
      ")
  }, once = TRUE)

  # initialize object to store search parameters
  fetch_params <- reactiveValues()

  init_fetch_params <- list(
    query = list(),
    fetch_ids = NULL,
    fetch_by = NULL,
    search_token = NULL,
    binmates_checked = FALSE,
    binmates_fetched = FALSE
  )

  # initialize object to store fetched records and associated data
  outdata <- reactiveValues()

  init_outdata <- list(
    data = NULL,
    hits = NULL,
    binmates = NULL,
    select_fields = config$fieldsets$bcdm,
    id_field = "processid",
    markers = NULL,
    filt_seq = NULL,
    filt_seq_idx = NULL,
    summary = NULL,
    bin_reps = NULL,
    div_profile = NULL,
    bin_consensus = NULL,
    bin_portal_stats = NULL
  )

  # keep track of output tabs
  tab_status <- reactiveValues(
    data = FALSE,
    qhits_tab = FALSE,
    summary = FALSE,
    bin_reps = FALSE,
    consensus_tab = FALSE,
    div_profile = FALSE,
    map_tab = FALSE
  )

  # convenience function for interrogating current tab status
  tab_monitor <- function(get = c("status", "current", "absent", "ins_target"), ins_tab = NULL) {
    tabs <- reactiveValuesToList(tab_status)
    switch(get,
      status = tabs,
      current = names(tabs)[tabs == TRUE],
      absent = names(tabs)[tabs == FALSE],
      ins_target = names(tabs)[max(which(tabs[1:match(ins_tab, names(tabs))] == TRUE))]
    )
  }

  ### Reactive UI ----

  source_test_code <- function() {
    tryCatch(
      {
        Sys.setenv(GITHUB_PAT = key_get(service = "tctools-GH-PAT"))
        tryCatch(
          {
            tmpfile <- tempfile()
            on.exit(unlink(tmpfile))
            content(httr::GET(
              "https://raw.githubusercontent.com/Centre-for-Biodiversity-Genomics/CBG-taxonomy/refs/heads/main/Code/bfr_test_func.R",
              add_headers(Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
            ), "raw") |> writeBin(tmpfile)
            # source(tmpfile, local = TRUE)
            source("C:/Users/Spencer Monckton/Desktop/Scripts/CBG-taxonomy/Code/bfr_test_func.R", local = TRUE)
          },
          error = function(e) {
            showNotification(paste0("Error accessing test functions:", e$message), type = "error")
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

  test_mode <- reactiveVal()
  session$onFlushed(function() {
    test_mode(user_config$settings$test_mode)
  }, once = TRUE)
  observeEvent(input$cbg_btn, source_test_code())
  observeEvent(test_mode(), if(isTRUE(test_mode())) source_test_code(), once = TRUE)
  observeEvent(input$testkey_confirm, {
    Sys.setenv(GITHUB_PAT = trimws(gsub('"', "", input$testkey)))

    try(
      {
        key_set_with_value("tctools-GH-PAT", password = Sys.getenv("GITHUB_PAT"))
      },
      silent = TRUE
    )
    source_test_code()
  })

  # conditional input logic to constrain marker search options (min/max requires a marker to be selected)
  observe({
    shinyjs::toggleState(id = "seq_min", condition = ((!is.null(input$seq_marker)) & (input$seq_marker != "")))
    shinyjs::toggleState(id = "seq_max", condition = ((!is.null(input$seq_marker)) & (input$seq_marker != "")))
    if((is.null(input$seq_marker)) | (input$seq_marker == "")) {
      updateNumericInput(inputId = "seq_min", value = character())
      updateNumericInput(inputId = "seq_max", value = character())
    }
  })

  # disable option to find additional BIN records when fetching by BIN
  # (in this case, fetching BIN mates will always return zero records)
  observe({
    if((input$fetch_by == "bin_uris") & (input$query_params == "fetch_opts")) {
      updateCheckboxInput(inputId = "fetch_binmates", value = FALSE)
    }
    shinyjs::toggleState(id = "fetch_binmates", condition = ((input$fetch_by != "bin_uris") | (input$query_params != "fetch_opts")))
  })

  # automatically update min/max marker length to avoid impossible ranges
  observeEvent(input$last_blurred,
    {
      id <- input$last_blurred
      value <- input[[id]]

      is_min <- grepl("_min", id)
      is_max <- grepl("_max", id)
      sibling_id <- if(is_min) sub("_min", "_max", id) else sub("_max", "_min", id)
      sibling_value <- input[[sibling_id]]

      val_range <- fcase(grepl("seq", id), list(c(5, 2000)))

      if (isTRUE(value < val_range[[1]][1])) updateNumericInput(inputId = id, value = val_range[[1]][1])
      if (isTRUE(value > val_range[[1]][2])) updateNumericInput(inputId = id, value = val_range[[1]][2])
      if (isTRUE(is_max && value < sibling_value)) updateNumericInput(inputId = id, value = sibling_value)
      if (isTRUE(is_min && value > sibling_value)) updateNumericInput(inputId = id, value = sibling_value)
    },
    ignoreInit = TRUE
  )

  # only show NTS toggle when data contains NTS records
  observe({
    nts_idx <- nts_idx()
    req(length(nts_idx) > 0)
    shinyjs::toggleElement(id = "include_nts", condition = (sum(nts_idx) > 0))
  })

  # reactively adjust available diversity analysis options
  observeEvent(input$div_presence, {
    if(input$div_presence) {
      updateCheckboxInput(inputId = "div_rare_by_site", value = FALSE)
      updateSelectizeInput(inputId = "div_profile",
                           choices = list("Rarefaction & extrapolation" = "rarefy",
                                          "Beta diversity index" = "beta"),
                           selected = input$div_profile[!input$div_profile %in% c("shannon", "preston")])
    } else {
      updateSelectizeInput(inputId = "div_profile",
                           choices = list("Rarefaction & extrapolation" = "rarefy",
                                          "Shannon diversity index" = "shannon",
                                          "Preston plot" = "preston",
                                          "Beta diversity index" = "beta"),
                           selected = input$div_profile)
    }
  }, ignoreInit = TRUE)
  observe({
    shinyjs::toggleElement(id = "div_grid_size", condition = (input$div_site_type == "grids"))
    shinyjs::toggleElement(id = "div_location_type", condition = (input$div_site_type == "locations"))
    shinyjs::toggleElement(id = "div_beta_type", condition = ("beta" %in% input$div_profile))
    shinyjs::toggleElement(id = "div_rare_by_site", condition = ("rarefy" %in% input$div_profile) & (!input$div_presence))
  })
  observe({
    req(filtered_data())
    data <- filtered_data()[input$data_table_rows_all, ]
    req(nrow(data) > 0)
    site_types <- list(
      "Sites" = "site",
      "Sectors" = "sector",
      "Regions" = "region",
      "Provinces/States" = "province.state",
      "Countries/Oceans" = "country.ocean",
      "Ecoregions" = "ecoregion",
      "Biomes" = "biome",
      "Realms" = "realm"
    )
    taxon_ranks <- c(
      "Species if known, BIN if not" = "bin_fallback",
      "BIN" = "bin_uri",
      "Species" = "species"
    )
    site_counts <- sapply(site_types, function(x) uniqueN(data[[x]][!empty(data[[x]])]))
    allow_sites <- setNames(
      unname(site_types)[site_counts >= 2],
      sapply(unlist(unname(c(site_types[site_counts >= 2]))), function(x) {
        val_count <- uniqueN(data[[x]][!empty(data[[x]])])
        paste0(
          names(site_types)[which(site_types == x)],
          " (n=", val_count, ")"
        )
      })
    )
    allow_tax <- names(which(unlist(data[, lapply(.SD, function(x) uniqueN(x[!empty(x)]) >= 2),
      .SDcols = unlist(unname(taxon_ranks[taxon_ranks != "bin_fallback"]))
    ])))
    if (any(c("bin_uri", "species") %in% allow_tax)) allow_tax <- c(allow_tax, "bin_fallback")
    updateSelectizeInput(session, "div_location_type",
      choices = allow_sites,
      selected = allow_sites[[1]]
    )
    updateSelectInput(session, "div_rank", choices = taxon_ranks[taxon_ranks %in% allow_tax], selected = allow_tax[1])
  })

  # reactively show/hide BIN rep options
  observe({
    shinyjs::toggleElement(
      id = "bin_rep_tax_opts",
      condition = (input$bin_rep_tax == TRUE)
    )
    shinyjs::toggleElement(
      id = "bin_rep_criteria_wrapper",
      condition = (input$bin_rep_default == FALSE)
    )
    shinyjs::toggleElement(
      id = "bin_rep_seq_len",
      condition = (input$bin_rep_seq_opt == "custom")
    )
  })


  ### Server helpers ----

  # function to reset filter options
  reset_filter <- function(include_verbatim = TRUE) {
    if (!isolate(input$tabs) %in% c("data", "bin_reps", "map_tab")) {
      bslib::nav_select(id = "tabs", selected = "data")
    }
    outdata$select_fields <- if (is.null(isolate(outdata$data))) {
      config$fieldsets$bcdm
    } else {
      c(config$fieldsets$bcdm, isolate(coll_mrkr_fields()))
    }
    updateSelectizeInput(session, "filt_opt",
      choices = filter_options(include_verbatim),
      selected = NULL
    )
    updateSelectizeInput(session, "filt_seq",
      choices = outdata$markers,
      selected = NULL
    )
  }

  # function to reset UI (and reactive values) to blank state
  reset_ui <- function() {
    for (i in seq_along(init_outdata)) {
      outdata[[names(init_outdata[i])]] <- unname(unlist(init_outdata[i]))
    }
    for (i in seq_along(init_fetch_params)) {
      fetch_params[[names(init_fetch_params[i])]] <- unname(unlist(init_fetch_params[i]))
    }
    bslib::nav_select(id = "tabs", selected = "data")
    for (t in names(tab_status)[names(tab_status) != "data"]) bslib::nav_remove(id = "tabs", target = t, session)
    for (i in seq_along(reactiveValuesToList(tab_status))) tab_status[[names(tab_status)[i]]] <- FALSE
    bslib::update_switch("include_binmates", value = FALSE)
    bslib::update_switch("include_nts", value = FALSE)
    bslib::update_switch("collapse_mrkrs", value = FALSE)
    updateCheckboxInput(inputId = "bc_portal_stats", value = FALSE)
  }

  # execute reset function when button is pressed
  observeEvent(input$reset_btn, {
    reset_filter()
  })

  ### Data fetching ----

  # set API key and generate fetch_ids
  observeEvent(input$fetch_btn | input$fetch_ctrl_enter,
    {
      shinyjs::hide("main_panel")
      key <- if(isTruthy(input$api_key)) input$api_key else Sys.getenv("api_key")
      validate_apikey(key)
      req(((isTruthy(input$fetch_id_list) || isTruthy(input$search_tax) || isTruthy(input$search_geo) || 
            isTruthy(input$seq_marker)) && isTruthy(Sys.getenv("api_key"))))
      shinyjs::show("table_area")
      shinyjs::hide("table_buttons")
      reset_ui()
      tab_status$data <- TRUE
      shinyjs::show("main_panel")
      tryCatch(
        {
          if (input$query_params == "search_opts") {
            showNotification("Searching for matching records...", id = "fetch_msg", duration = NULL, type = "message")
            search_query <- list(
              taxonomy = split_query(input$search_tax, list = TRUE),
              geography = split_query(input$search_geo, list = TRUE),
              marker = input$seq_marker[input$seq_marker != ""],
              marker_min_length = input$seq_min[!is.na(input$seq_min)],
              marker_max_length = input$seq_max[!is.na(input$seq_max)],
              collection_start_date = input$search_dates[1][!is.na(input$search_dates[1])],
              collection_end_date = input$search_dates[2][!is.na(input$search_dates[1])],
              institutes = split_query(input$search_inst, list = TRUE)
            )
            fetch_params$query <- search_query[sapply(search_query, length) > 0]
            search_token <- do.call(bold.full.search.step1, search_query[sapply(search_query, length) > 0])
            if (is.null(search_token)) {
              showNotification("No records found matching search terms.", id = "fetch_msg", type = "warning")
            } else if (search_token$num_of_accessible >= 100000) {
              removeNotification(id = "fetch_msg")
              fetch_params$search_token <- search_token
              search_confirm_modal()
            } else {
              showNotification(paste0(
                "Downloading ids of ",
                format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE),
                "  matching records..."
              ), id = "fetch_msg", duration = NULL, type = "message")
              fetch_params$fetch_by <- "processid"
              fetch_params$fetch_ids <- bold.full.search.step2(search_token)$processid
            }
          } else {
            fetch_by <- input$fetch_by
            fetch_ids <- split_query(input$fetch_id_list, list = TRUE)
            fetch_params$query[[fetch_by]] <- fetch_ids
            fetch_params$fetch_by <- fetch_by
            fetch_params$fetch_ids <- unlist(fetch_ids)
          }
        },
        warning = function(w) {
          showNotification("No results found using provided search terms.", id = "fetch_msg", type = "warning")
        },
        error = function(e) {
          showNotification(paste0("Error processing record identifiers: ", e$message), id = "fetch_msg", type = "error")
        }
      )
    },
    ignoreInit = TRUE
  )

  # modal to call up upon finding over 100k records from search
  search_confirm_modal <- function() {
    req(fetch_params$search_token)
    search_token <- fetch_params$search_token
    modal_msg <- p(HTML(paste0("Search returned <strong>", format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE), "</strong> accessible records.")))
    if (search_token$num_of_records >= search_token$limit) {
      modal_msg <- div(modal_msg, p(class = "warn-text", HTML("<strong>Note:</strong> Maximum search limit of ", format(search_token$limit, big.mark = ",", scientific = FALSE), " was reached. Additional matches may exist beyond what can be fetched.")))
    }
    showModal(
      modalDialog(
        title = "Confirm fetch",
        modal_msg,
        footer = tagList(
          div("Proceed to fetch records?"),
          div(
            id = "modal_confirm",
            actionButton("search_confirm_btn", "Yes"),
            modalButton("No")
          )
        ),
        easyClose = TRUE
      )
    )
  }

  # upon confirming search, proceed to get fetch_ids (process IDs)
  observeEvent(input$search_confirm_btn, {
    removeModal()
    search_token <- fetch_params$search_token
    tryCatch(
      {
        showNotification(paste0(
          "Downloading ids of ",
          format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE),
          "  matching records..."
        ), id = "fetch_msg", duration = NULL, type = "message")
        fetch_params$fetch_by <- "processid"
        fetch_params$fetch_ids <- bold.full.search.step2(search_token)$processid
      },
      error = function(e) {
        showNotification(paste0("Error retrieving records from BOLD: ", e$message), id = "fetch_msg", type = "error")
      }
    )
  })

  # perform fetch
  observeEvent(fetch_params$fetch_ids, {
    req(fetch_params$fetch_ids, fetch_params$fetch_by)
    data <- as.data.table(bold.fetch.shiny(
      get_by = fetch_params$fetch_by,
      query = fetch_params$fetch_ids,
      BCDM_only = FALSE
    ))
    req(nrow(data) > 0)
    populate_data(data)
  })

  # populate data
  populate_data <- function(data) {
    data[, c("id_date_parsed", "project_code", "lat", "lon") := c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdata$id_field)), parse_lat_lon(coord))]
    convert_factor_char(data)
    if (fetch_params$fetch_by == "bin_uris") {
      shinyjs::hide("include_binmates")
      shinyjs::hide("view_binmates")
    } else {
      shinyjs::show("include_binmates")
      shinyjs::show("view_binmates")
    }
    outdata$markers <- gsub("ZZZ", "None", sort(unique(c(levels(data$marker_code), if (anyNA(data$marker_code)) "ZZZ"))))
    outdata$data <- data

    shinyjs::show("table_buttons")
    bslib::accordion_panel_close(id = "optpanels", values = "fetchdata")
    bslib::accordion_panel_open(id = "optpanels", values = c("customize", "summarize", "analyze"))
    if (input$fetch_binmates == TRUE) {
      binmate_modal()
    }
  }

  # logic for initiating search (or fetch) of additional BIN records ("BIN mates")
  # if the BIN mates switch is toggled on, a modal opens to report the number of additional BIN records
  observeEvent(input$include_binmates,
    {
      req(outdata$data)
      if ((isolate(input$include_binmates) == TRUE) && (isolate(fetch_params$binmates_checked) == FALSE)) {
        binmate_modal()
      } else if ((length(outdata$binmates) > 0) && (fetch_params$binmates_fetched == FALSE)) {
        fetch_binmates()
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # the modal can also be accessed without toggling the switch, in case the user simply wants to review the BIN mate list
  observeEvent(input$view_binmates, {
    req(outdata$data)
    binmate_modal()
  })

  # modal logic for BIN mates
  binmate_modal <- function() {
    req(outdata$data)
    binmate_pids <- outdata$binmates
    if (is.null(outdata$binmates)) {
      binmate_pids <- get_binmates(outdata$data[!is.na(bin_uri)])
      outdata$binmates <- binmate_pids
    }
    if (length(binmate_pids) == 0) {
      modal_msg <- div(HTML(paste0("No additional BIN members found.")))
      modal_footer <- tagList(
        div(
          id = "modal_confirm",
          actionButton("binmate_btn", "OK")
        )
      )
      binmate_list <- div("")
    } else if (fetch_params$binmates_checked == FALSE) {
      modal_msg <- div(HTML(paste0("Found <strong>", length(binmate_pids), "</strong> additional BIN members. Retrieve and add to table?")))
      modal_footer <- tagList(
        actionButton("copy_binmates", "Copy"),
        div(
          id = "modal_confirm",
          actionButton("binmate_btn", "Yes"),
          actionButton("binmate_no", "No")
        )
      )
      binmate_list <- verbatimTextOutput("binmate_pids")
    } else {
      modal_msg <- div(HTML(paste0("The following <strong>", length(binmate_pids), "</strong> additional BIN members were added to the retrieved data.")))
      modal_footer <- tagList(
        actionButton("copy_binmates", "Copy"),
        div(
          id = "modal_confirm",
          modalButton("OK")
        )
      )
      binmate_list <- verbatimTextOutput("binmate_pids")
    }
    showModal(
      modalDialog(
        title = "Additional BIN members",
        modal_msg,
        binmate_list,
        footer = modal_footer,
        easyClose = (fetch_params$binmates_checked == TRUE)
      )
    )
    fetch_params$binmates_checked <- TRUE
  }

  fetch_binmates <- function() {
    binmate_data <- as.data.table(bold.fetch.shiny(
      get_by = "processid",
      query = outdata$binmates,
      BCDM_only = FALSE
    ))
    add_binmates(binmate_data)
  }

  add_binmates <- function(binmate_data) {
    showNotification("Processing additional BIN member data...",
      id = "fetch_msg", duration = NULL, type = "message"
    )
    binmate_data <- binmate_data[, c("id_date_parsed", "project_code", "lat", "lon") :=
      c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdata$id_field)), parse_lat_lon(coord))]
    convert_factor_char(binmate_data)
    data <- unique(rbindlist(
      list(
        outdata$data,
        binmate_data
      ),
      fill = TRUE
    ))
    showNotification("Additional BIN members retrieved.",
      id = "fetch_msg", duration = 2, type = "message"
    )
    outdata$markers <- gsub("ZZZ", "None", sort(unique(c(levels(data$marker_code), if (anyNA(data$marker_code)) "ZZZ"))))
    outdata$data <- data
    fetch_params$binmates_fetched <- TRUE
  }

  # user can close the BIN mate modal without fetching BIN mates
  observeEvent(input$binmate_no, {
    removeModal()
    bslib::update_switch("include_binmates", label = "Include additional BIN members", value = FALSE, session)
  })

  # fetch BIN mates and add them to the data
  observeEvent(input$binmate_btn, {
    removeModal()
    if (length(outdata$binmates) > 0) {
      fetch_binmates()
      bslib::update_switch("include_binmates", label = "Include additional BIN members", value = TRUE, session)
    } else {
      bslib::update_switch("include_binmates", label = "Include additional BIN members", value = FALSE, session)
    }
  })

  ### Reactive computations & observers ----

  # keep summary analysis options updated according to available fields
  observeEvent(outdata$data, {
    req(outdata$data)
    updateSelectInput(session,
      "ana_opt",
      choices = list(
        "Taxonomy" = list(
          "Taxonomic summary" = "tax_summary"
        ),
        "Unique values - selected fields" = list(
          "Taxon" = "identification",
          "BIN" = "bin_uri",
          "Identification dates (parsed)" = "id_date_parsed",
          "Projects" = "projects",
          "Datasets" = "datasets",
          "Country" = "country.ocean"
        ),
        "Unique values - all fields" = as.list(
          names(outdata$data)[
            !names(outdata$data) %in% c("identification", "bin_uri", "country.ocean", "id_date_parsed", "project_code")
          ]
        )
      )
    )
  })

  # logic for generating marker-collapsed data table
  collapsed_data <- reactive({
    req(outdata$data)
    collapse_markers(outdata$data)
  })

  # reactive to store additional marker-specific fields/columns
  coll_mrkr_fields <- reactive({
    cd <- collapsed_data()
    req(cd)
    setdiff(names(cd), c(config$fieldsets$all, config$fieldsets$parsed))
  })

  # reactive object for user-selected fields/column filters
  computed_fields <- reactive({
    # if no column filters have been selected, select all fields (plus collapsed table fields if there are any)
    if (length(input$filt_opt) == 0) {
      fields <- c(config$fieldsets$bcdm, coll_mrkr_fields())
      # show_copy_fasta <- TRUE
      id_field <- "processid"
    } else {
      # expand out any custom user field sets
      field_opts <- c()
      for (o in input$filt_opt) {
        if (grepl("^userset", o)) {
          field_opts <- unique(c(field_opts, unlist(sapply(user_config$fieldsets$custom, function(x) if (x$id == o) x$fields))))
        } else {
          field_opts <- unique(c(field_opts, o))
        }
      }

      # determine which filter selections are field sets from config specification
      sets <- field_opts[(field_opts %in% names(config$fieldsets)) | grepl("|", field_opts, fixed = T)]

      fields <- c()
      # step through selected fields, handling them according to whether they are preset field sets, custom field sets, or individual fields
      for (o in field_opts) {
        if (o %in% sets) {
          opt <- unlist(strsplit(o, "|", fixed = TRUE))
          if (length(opt) > 1) {
            fields <- unique(c(fields, opt[-1]))
          } else {
            fields <- unique(c(fields, config$fieldsets[o][[1]]))
          }
        } else {
          fields <- unique(c(fields, o))
        }
      }

      id_idx <- which(c("processid", "sampleid") %in% fields)
      if (length(id_idx) == 0) {
        fields <- c("processid", fields)
        id_field <- "processid"
      } else {
        id_field <- c("processid", "sampleid")[min(id_idx)]
      }
    }
    list(fields = fields, id_field = id_field)
  })

  # consume computed filter fields and record them in the reactive value list
  observeEvent(computed_fields(), {
    result <- computed_fields()
    outdata$select_fields <- result$fields
    outdata$id_field <- result$id_field
  })

  # modal logic for saving custom field sets
  observeEvent(input$save_filter_set, {
    showModal(
      modalDialog(
        title = "Saved column sets",
        div(
          selectInput(
            "sets_to_delete",
            "Existing presets:",
            sapply(user_config$fieldsets$custom, function(s) setNames(s$id, s$name)),
            multiple = TRUE,
            selectize = FALSE, width = "100%"
          ),
          hidden(actionLink("del_filter_btn", "× Delete selected", class = "warn-text"))
        ),
        textInput(
          "save_filter_name",
          tagList(
            span("Save current column selection as:"),
            bslib::tooltip(
              icon("circle-question"),
              "Using the name of an existing preset will update/overwrite it."
            )
          ),
          width = "100%"
        ),
        footer = tagList(
          div(),
          div(
            id = "modal_confirm",
            actionButton("save_filter_btn", "Save"),
            modalButton("Cancel")
          )
        ),
        easyClose = TRUE
      )
    )
  })

  # field set save logic
  observeEvent(input$save_filter_btn, {
    req(isTruthy(input$save_filter_name), isTruthy(input$filt_opt))
    removeModal()
    filter_set <- modify_custom_fieldset(input$save_filter_name, input$filt_opt)
    updateSelectizeInput(session, "filt_opt",
      choices = filter_options(),
      selected = filter_set
    )
  })

  # reactively show/hide delete button
  observe({
    shinyjs::toggleElement(id = "del_filter_btn", condition = isTruthy(input$sets_to_delete))
  }) # , ignoreInit = TRUE, ignoreNULL = FALSE)

  # field set delete logic
  observeEvent(input$del_filter_btn, {
    for (s in input$sets_to_delete) {
      filter_set <- modify_custom_fieldset(s, operation = "delete")
    }
    updateSelectInput(
      inputId = "sets_to_delete",
      choices = sapply(user_config$fieldsets$custom, function(s) s$name),
      selected = NULL
    )
    updateSelectizeInput(session, "filt_opt",
      choices = filter_options(),
      selected = input$filt_opt[input$filt_opt %in% unname(unlist(input$filt_opt))]
    )
  })

  # keep marker filters updated based on available data
  observeEvent(outdata$markers, {
    updateSelectizeInput(session, "filt_seq", choices = outdata$markers, selected = NULL)
  })

  # keep BIN rep options updated based on available data
  observeEvent(outdata$data, {
    req(outdata$data)
    insts <- unique(as.character(outdata$data[["inst"]]))
    updateSelectizeInput(session, "bin_rep_inst_opt",
      choices = as.list(insts),
      selected = ifelse("Centre for Biodiversity Genomics" %in% insts,
        "Centre for Biodiversity Genomics",
        insts[1]
      )
    )
  })

  # function to compute a row index based on user-selected markers
  # this is needed for the flexibility to work either with the full table or collapsed-marker table
  seq_filter <- function(data, filt_seq) {
    filt_markers <- filt_seq[filt_seq != "None"]
    nuc_cols <- intersect(paste0(filt_markers, "_nuc"), names(data))
    keep_seq <- list()

    if ("marker_code" %in% names(data)) {
      if (length(filt_markers) > 0) {
        keep_seq[[1]] <- data[["marker_code"]] %in% filt_markers
      }
      if ("None" %in% filt_seq & ("marker_code" %in% names(data))) {
        keep_seq[[2]] <- empty(data[["marker_code"]])
      }
    }
    if (length(nuc_cols) > 0) {
      keep_seq[[3]] <- rowSums(data[, lapply(.SD, function(x) !empty(x)), .SDcols = nuc_cols]) > 0
    }

    if (length(keep_seq) > 0) {
      return(Reduce("|", keep_seq[!sapply(keep_seq, is.null)]))
    } else {
      return(TRUE)
    }
  }

  # reactive computation of NTS row indexer
  nts_idx <- reactive({
    req(outdata$data)
    if (input$collapse_mrkrs == TRUE) {
      dt <- collapsed_data()
      req(dt)
      dt[, grepl("NTS(?:_[A-Za-z]+)?_OF:", associated_specimens, perl = TRUE) |
        grepl("^BIOUG.+?\\.NTS[0-9]*?$", sampleid, perl = TRUE)]
    } else {
      dt <- outdata$data
      grepl("NTS(?:_[A-Za-z]+)?_OF:", dt$associated_specimens, perl = TRUE) |
        grepl("^BIOUG.+?\\.NTS[0-9]*?$", dt$sampleid, perl = TRUE)
    }
  })

  # reactive computation of filtered data table
  filtered_data <- reactive({
    req(outdata$data)
    data <- if (input$collapse_mrkrs == TRUE) {
      cd <- collapsed_data()
      req(cd)
      cd
    } else {
      outdata$data
    }

    if (!isTRUE(input$include_nts)) {
      data <- data[!nts_idx()]
    }

    if (!isTRUE(input$include_binmates) || length(outdata$binmates) == 0) {
      data <- data[!processid %in% outdata$binmates]
    }

    data[seq_filter(data, input$filt_seq)]
  })

  # compute summary counts and inject as a new tab
  observeEvent(input$ana_btn, {
    req(nrow(outdata$data) > 0)
    outdata$summary <- NULL
    # outdata$summary <- summarize_table(filtered_data(), input$ana_opt, outdata$id_field)
    withInfProgress(message = "Generating summary...", {
      outdata$summary <- summarize_table(filtered_data()[input$data_table_rows_all, ], input$ana_opt, outdata$id_field)
    })
    if (!tab_status$summary) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "summary"), position = "after", select = TRUE,
        bslib::nav_panel(
          id = "summary",
          value = "summary",
          "Summary",
          div(
            id = "sum_table_area",
            DT::dataTableOutput("summary_table") |> withSpinner(color = "#aaaaaa", type = 5, size = 0.6)
          )
        )
      )
      tab_status$summary <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "summary")
    }
  })

  # compute query coverage report
  observeEvent(input$query_hits_btn, {
    req(nrow(outdata$data) > 0)
    hits <- get_query_hits(data = outdata$data, query = fetch_params$query)
    outdata$hits <- hits$hits_dt
    if (!tab_status$qhits_tab) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "qhits_tab"), select = TRUE,
        bslib::nav_panel(
          id = "qhits_tab",
          value = "qhits_tab",
          "Hit report",
          hits$report
        )
      )
      tab_status$qhits_tab <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "qhits_tab")
    }
  })

  # display diversity profile info
  observeEvent(input$div_profile_info, {
    showModal(
      modalDialog(
        title = "Diversity profile details",
        includeHTML("www/div_details.html"),
        footer = tagList(
          modalButton("Close")
        ),
        size = "xl",
        easyClose = TRUE
      )
    )
  })
  
  # compute diversity profile
  observeEvent(input$div_compute_btn, {
    req(outdata$data)
    outdata$div_profile <- NULL
    data <- unique(filtered_data()[input$data_table_rows_all, ], by = outdata$id_field)
    site_col <- input$div_location_type
    # notify with error if beta diversity cannot be computed
    if ("beta" %in% input$div_profile) {
      if (length(unique(data[!empty(get(site_col))][[site_col]])) <= 1) {
        showNotification("Must have at least two sites for beta diversity analysis.", type = "error")
        validate(need(FALSE, ""))
      }
    }
    # build named list to be used for diversity analysis call
    div_opts <- list(
      bold_df = NULL,
      taxon_rank = input$div_rank,
      taxon_name = NULL,
      site_type = input$div_site_type,
      location_type = NULL,
      gridsize = NULL,
      presence_absence = input$div_presence,
      diversity_profile = "preston",
      beta_index = input$div_beta_type
    )
    input_opts <- div_opts[c(2,5,7,9)]
    # get BIN-based names for BINs without species names
    if (div_opts$taxon_rank == "bin_fallback") {
      unid_bins <- unique(data[, if (all(empty(species))) .SD, by = bin_uri]$bin_uri)
      if(length(unid_bins) > 0) {
        withInfProgress(message = "Assigning fallback BIN-based names...", {
          unid_bin_tax <- get_bin_consensus(data[bin_uri %in% unid_bins], min_ids = 1)
          data[, bin_fallback := fifelse(
            bin_uri %in% unid_bins,
            unid_bin_tax[.SD,
              on = .(bin_uri),
              paste(concordant_id, bin_uri, sep = "_sp_")
            ],
            as.character(species)
          )]
        })
      } else {
        data[, bin_fallback := as.character(species)]
      }
    }
    # replace any blank values with NA
    data.table::set(data, i = which(empty(data[[div_opts$taxon_rank]])),
                    j = div_opts$taxon_rank, value = NA_character_)
    # data for site-based analyses needs to be filtered to records with site filled in
    if (input$div_site_type == "locations") {
      input_opts$location_type <- isolate(input$div_location_type)
      div_opts[["location_type"]] <- input_opts$location_type
      data.table::set(data, i = which(empty(data[[div_opts$location_type]])),
                      j = div_opts$location_type, value = NA_character_)
      site_data <- data[!empty(get(div_opts$location_type)) & !empty(get(div_opts$taxon_rank))]
      site_label <- get_div_label(div_opts$location_type, capitalize = TRUE)
    } else {
      input_opts$location_type <- "grids"
      input_opts$gridsize <- input$div_grid_size * 1000
      div_opts[["gridsize"]] <- input_opts$gridsize
      site_data <- data[!empty(coord) & !empty(get(div_opts$taxon_rank))]
      site_label <- "Grid cell"
    }
    div_opts[["bold_df"]] <- site_data
    # initialize the profile using BOLDconnectR::bold.analyze.diversity()
    # the minimal analysis option is the Preston abundance distribution, which also returns a community matrix
    # the Preston abundance distribution analysis is from vegan::prestondistr()
    withInfProgress(message = "Building community matrix...", {
      div_profile <- do.call(bold.analyze.diversity, div_opts)
      div_profile$input_opts <- input_opts
      div_profile$comm_table <- data.table(Site = rownames(div_profile$comm.matrix), div_profile$comm.matrix)
      names(div_profile$comm_table)[1] <- site_label
      # if Preston plot is not wanted, remove it now
      if (!"preston" %in% input$div_profile) {
        div_profile[["preston.plot"]] <- NULL
      }
      if (input$div_site_type == "locations") {
        div_profile$site_stats <- data[, .(N = .N, S = uniqueN(get(div_opts$taxon_rank), na.rm = TRUE)), by = eval(input_opts$location_type)]
        data.table::setnames(div_profile$site_stats,
                             old = input_opts$location_type,
                             new = site_label)
      } else {
        div_profile$site_stats <- data.table(
            site = rownames(div_profile$comm.matrix),
            N = rowSums(div_profile$comm.matrix),
            S = rowSums(div_profile$comm.matrix > 0)
          )
        data.table::setnames(div_profile$site_stats,
                             old = "site",
                             new = site_label)
          
        grids_wgs84 <- sf::st_transform(div_profile$grids.data, crs = 4326)

        leaflet(grids_wgs84) %>%
          addTiles() %>%  # OpenStreetMap base layer
          addPolygons(
            fillColor   = "#3A7D44",
            fillOpacity = 0.3,
            color       = "#1C2A1E",
            weight      = 1,
            label       = ~cell.id,
            highlightOptions = highlightOptions(
              fillOpacity = 0.6,
              weight      = 2,
              bringToFront = TRUE
            )
          )
      }
    })
    # save basic statistics for later use
    div_profile$site_counts <- list(N = rowSums(div_profile$comm.matrix),
                                    S = rowSums(div_profile$comm.matrix > 0))
    div_profile$counts <- list(sites = uniqueN(data[[site_col]], na.rm = TRUE),
                               obs = nrow(data),
                               taxa = uniqueN(data[[div_opts$taxon_rank]]), na.rm = TRUE)
    # rarefaction & extrapolation using iNEXT 
    if ("rarefy" %in% input$div_profile) {
      withInfProgress(message = "Computing rarefaction...", {
        if(input_opts$presence_absence) {
          # for presence-absence data, perform incidence-based rarefaction for entire dataset
          comm_inc <- iNEXT::as.incfreq(div_profile$comm.matrix)
          inext_res <- iNEXT::iNEXT(
            x = list(Overall = comm_inc),
            datatype = "incidence_freq",
            endpoint = sum(comm_inc) * 2,
            nboot = 100
          )
        } else {
          # otherwise, perform individual-based rarefaction for entire dataset
          total_counts <- table(data[[input$div_rank]])
          total_counts <- setNames(as.numeric(total_counts), names(total_counts))
          inext_res <- iNEXT::iNEXT(
            x = list(Overall = total_counts),
            datatype = "abundance",
            endpoint = sum(total_counts) * 2,
            nboot = 100
          )
        }
        div_profile$asy_est <- if("Diversity" %in% names(inext_res$AsyEst)) {
          inext_res$AsyEst[inext_res$AsyEst$Diversity == "Species richness", ]
        } else {
          asy_est <- inext_res$AsyEst["Species Richness", ]
          names(asy_est)[c(4,5)] <- c("LCL", "UCL")
          asy_est
        }
      })
      # note that this input should be FALSE when input$div_presence is TRUE
      if (input$div_rare_by_site) {
        # individual-based rarefaction for individual sites
        # discard sites with fewer than 10 observations OR fewer than 2 species
        keep <- div_profile$site_counts$N >= 10 & div_profile$site_counts$S >= 2
        comm_filtered <- div_profile$comm.matrix[keep, ]
        n_dropped <- sum(!keep)
        n_kept <- length(keep) - n_dropped
        msg_text <- if (n_kept == 0) {
          "None of the sites have sufficient records (min. 10) or species (min. 2) for site-level rarefaction."
        } else {
          paste(n_dropped, "site(s) have insufficient records (min. 10) or species (min. 2) for site-level rarefaction.")
        }
        showNotification(msg_text, type = "warning", duration = 10)
        if (n_kept > 0) {
          # treat each site as an assemblage (a named vector of species counts)
          assemblages <- lapply(seq_len(nrow(comm_filtered)), function(i) {
            row <- comm_filtered[i, ]
            row[row > 0]
          })
          names(assemblages) <- rownames(comm_filtered)
          opi <- pbapply::pboptions(type = "shiny", title = paste("Computing rarefactions for", nrow(comm_filtered), "sites"), label = "")
          inext_list <- pbapply::pblapply(names(assemblages), function(nm) {
            counts <- assemblages[[nm]]
            res <- iNEXT::iNEXT(
              x = list(counts),
              datatype = "abundance",
              endpoint = sum(counts) * 2,
              nboot = 100
            )
            res$iNextEst$size_based$Assemblage <- nm
            res$AsyEst$Assemblage <- nm
            res
          })
          pbapply::pboptions(opi)
          names(inext_list) <- names(assemblages)
          inext_res <- c(
            list(Overall = inext_res),
            inext_list
          )
          asy_est <- rbindlist(lapply(inext_res, function(x) x$AsyEst[x$AsyEst$Diversity == "Species richness", ]))[, !c("Diversity", "Observed", "s.e.")]
          data.table::setnames(asy_est, new = c(site_label, "S_est", "95_CI_lower", "95_CI_upper"))
          div_profile$site_stats <- asy_est[div_profile$site_stats, on = site_label]
        } else {
          inext_res <- list(Overall = inext_res)
        }
      }
      div_profile$inext <- inext_res
    }
    # beta diversity is computed with BAT::beta() via BOLDconnectR
    if ("beta" %in% input$div_profile) {
      withInfProgress(message = "Computing beta diversity...", {
        beta_div_results <- BOLDconnectR:::beta_div_profile(
          div_profile$comm.matrix,
          input_opts$beta_index,
          input_opts$presence_absence
        )
        div_profile$total.beta <- beta_div_results$total.beta
        div_profile$replace <- beta_div_results$replace
        div_profile$richnessd <- beta_div_results$richnessd
        beta_mat <- as.matrix(beta_div_results$total.beta)
        diag(beta_mat) <- NA
        beta_means <- data.table(
          site = rownames(beta_mat),
          mean = rowMeans(beta_mat, na.rm = TRUE)
        )
        data.table::setnames(beta_means, new = c(site_label, "mean_beta"))
        div_profile$site_stats <- beta_means[div_profile$site_stats, on = site_label]
        # NMDS ordination plot using vegan::monoMDS (only if >= 3 sites)
        if(attr(div_profile$total.beta, "Size") < 3) {
          showNotification("Skipping beta diversity plot; need at least 3 sites to run ordination.", type = "warning", duration = 10)  
        } else {
          div_profile$nmds.beta <- vegan::monoMDS(beta_mat, k = 2, maxit = 500)
        }
      })
    }
    # Shannon index is computed with vegan::diversity() via BOLDconnectR
    if ("shannon" %in% input$div_profile) {
      withInfProgress(message = "Computing Shannon indices...", {
        keep <- div_profile$site_counts$S >= 2
        n_dropped <- sum(!keep)
        n_kept <- length(keep) - n_dropped
        msg_text <- if (n_kept == 0) {
          "None of the sites have sufficient species to compute Shannon index (min. 2)."
        } else {
          paste(n_dropped, "site(s) have insufficient species to compute Shannon index (min. 2).")
        }
        showNotification(msg_text, type = "warning", duration = 10)
        if (n_kept > 0) {
          div_profile$shannon_div <- round(BOLDconnectR:::shannon_div_profile(df = div_profile$comm.matrix), 2)
          h_tab <- data.table(site = rownames(div_profile$shannon_div), div_profile$shannon_div)
          data.table::setnames(h_tab, new = c(site_label, "H'"), skip_absent = TRUE)
          div_profile$site_stats <- h_tab[div_profile$site_stats, on = site_label]
        }
      })
    }
    # complete the site_stats table and save the div_profile object
    data.table::setorder(div_profile$site_stats, na.last = TRUE)
    cols <- c(names(div_profile$site_stats)[1], "N", "S", "S_est", "95_CI_lower", "95_CI_upper", "H'", "mean_beta")
    div_profile$site_stats <- div_profile$site_stats[, .SD, .SDcols = intersect(cols, names(div_profile$site_stats))]
    outdata$div_profile <- div_profile
    if (!tab_status$div_profile) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "div_profile"), position = "after", select = TRUE,
        bslib::nav_panel(
          value = "div_profile",
          "Diversity profile",
          uiOutput("div_stats"),
          uiOutput("div_plots")
        )
      )
      tab_status$div_profile <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "div_profile")
    }
  })
  
  # show a modal for saving diversity analysis results
  observeEvent(input$div_dl_btn, {
    req(outdata$div_profile)
    div <- outdata$div_profile
    showModal(
      modalDialog(
        title = "Export diversity profile results",
        div(class = "div_dl",
            HTML('<span style="font-weight: 500; font-size:1.1rem;">Full results as ZIP archive:</span>'), 
            downloadButton("all_zip", "ZIP"),
            h3("Results by site:"),
            "Summary table", downloadButton("sum_table", "CSV"),
            if("comm_table" %in% names(div)) {
              tagList(
                "Community matrix", downloadButton("comm_mat", "CSV"),
              )
            },
            if("shannon_div" %in% names(div)) {
              tagList(
                "Shannon indices", downloadButton("shannon", "CSV")
              )
            },
            if("total.beta" %in% names(div)) {
              tagList(
                "Pairwise beta distance matrix", downloadButton("beta_dists", "CSV")
              )
            },
            if("preston.res" %in% names(div)) {
              tagList(
                h3("Preston abundance distribution:"),
                "Analysis results (R object)", downloadButton("preston_rds", "RDS"),
                "Frequencies", downloadButton("preston_csv", "CSV")
              )
            },
            if("inext" %in% names(div)) {
              tagList(
                h3(paste0(ifelse(div$input_opts$presence_absence, "Incidence", "Individual"), "-based rarefaction & extrapolation:")),
                "iNEXT analysis results (R object)", downloadButton("inext_rds", "RDS"),
                "Size-based sampling curves", downloadButton("inext_size", "CSV"),
                "Coverage-based sampling curves", downloadButton("inext_cov", "CSV"),
                "Asymptotic richness estimates", downloadButton("inext_asy", "CSV")
              )
            },
            if("nmds.beta" %in% names(div)) {
              tagList(
                h3("Beta diversity ordination:"),
                "vegan::monoMDS() analysis results (R object)", downloadButton("nmds_rds", "RDS"),
                "NMDS scores", downloadButton("nmds_scores", "CSV")
              )
            }
        ),
        footer = tagList(
          modalButton("Close")
        ),
        easyClose = TRUE
      )
    )
  })

  # compute BIN consensus and inject as a new tab
  observeEvent(input$bin_consensus_btn, {
    req(nrow(outdata$data) > 0)
    outdata$bin_consensus <- NULL
    data <- filtered_data()[input$data_table_rows_all, ][!empty(bin_uri)]
    withInfProgress(message = "Computing consensus BIN taxonomy...", {
      bin_consensus <- get_bin_consensus(
        df = data,
        threshold = as.double(unlist(unname(input$bc_threshold))),
        min_ids = unlist(unname(input$bc_minids)),
        enforce_scientific = input$bc_enforcesci,
        discord_format = "text"
      )
      col_order <- append(names(bin_consensus), c("min_rank", "max_rank"), after = 2)
      include <- ifelse(input$bc_enforcesci,
        which(!grepl(re_int, data[, identification], perl = TRUE)),
        seq_along(data[, identification])
      )
      bin_consensus <- merge(bin_consensus,
        data[, .(
          min_rank = as.factor(ranks[min(match(data[include, identification_rank], ranks))]),
          max_rank = as.factor(ranks[max(match(data[include, identification_rank], ranks))])
        ), by = "bin_uri"],
        by = "bin_uri",
        all.x = TRUE
      )
      setcolorder(bin_consensus, col_order)
    })

    outdata$bin_consensus <- bin_consensus

    if (!tab_status$consensus_tab) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "consensus_tab"), position = "after", select = TRUE,
        bslib::nav_panel(
          id = "consensus_tab",
          value = "consensus_tab",
          "BIN consensus",
          div(
            id = "consensus_table_area",
            DT::dataTableOutput("bincons_table") |> withSpinner(color = "#aaaaaa", type = 5, size = 0.6)
          )
        )
      )
      tab_status$consensus_tab <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "consensus_tab")
    }
  })

  observeEvent(outdata$bin_consensus, {
    req(outdata$bin_consensus)
    delay(1000, {
      if (input$bc_portal_stats == TRUE && is.null(outdata$bin_portal_stats)) {
        outdata$bin_portal_stats <- get_portal_bin_stats(as.character(isolate(outdata$bin_consensus$bin_uri)), shiny = TRUE)
        outdata$bin_consensus <- merge(isolate(outdata$bin_consensus),
          isolate(outdata$bin_portal_stats),
          by = "bin_uri",
          all.x = TRUE
        )
        bslib::nav_select(id = "tabs", selected = "consensus_tab")
      }
    })
  })

  # the portal API can be somewhat inconsistent; this will insert a warning message about any BINs for which the query timed out
  insert_portal_warning <- function(portal_stats) {
    removeUI("#bc_portal_warning", immediate = TRUE)
    failed <- nrow(portal_stats[empty(member_count)])
    if (failed > 0) {
      insertUI(
        selector = "div.form-group:has(#bc_portal_stats):not(:has(div.form-group:has(#bc_portal_stats)))",
        where = "beforeEnd",
        ui = div(
          id = "bc_portal_warning",
          class = "control-label warn-text",
          HTML(paste0("Portal stats timed out for ", failed, " BINs. ")),
          actionLink("bc_portal_retry", "Click to retry.")
        )
      )
    }
  }


  # check portal stats upon calculation and insert UI with a link to retry the query for failed BINs
  observeEvent(outdata$bin_portal_stats, {
    portal_stats <- outdata$bin_portal_stats
    req(portal_stats)
    if ("member_count" %in% names(portal_stats)) {
      insert_portal_warning(portal_stats)
    }
  })

  # logic for retrying portal BIN query
  observeEvent(input$bc_portal_retry, {
    init_stats <- outdata$bin_portal_stats
    req(init_stats)
    retry_stats <- get_portal_bin_stats(as.character(init_stats[empty(member_count), bin_uri]), shiny = TRUE)
    init_stats[retry_stats, (names(init_stats)) := mget(paste0("i.", names(init_stats))), on = "bin_uri"]
    cons <- isolate(outdata$bin_consensus)
    outdata$bin_consensus <- merge(cons[, .SD, .SDcols = c("bin_uri", names(cons)[!names(cons) %in% names(init_stats)])],
      init_stats,
      by = "bin_uri",
      all.x = TRUE
    )
    bslib::nav_select(id = "tabs", selected = "consensus_tab")
    outdata$bin_portal_stats <- init_stats
    insert_portal_warning(init_stats)
  })

  # select BIN reps and inject as a new tab
  observeEvent(input$bin_rep_btn, {
    req(nrow(outdata$data) > 0)
    outdata$bin_reps <- NULL
    withInfProgress(message = "Selecting BIN representatives...", {
      criteria <- if (!input$bin_rep_default) {
        list(
          vouchered = TRUE,
          seq_length = switch(input$bin_rep_seq_opt,
                              "custom" = input$bin_rep_seq_len,
                              "658" = 658,
                              input$bin_rep_seq_opt),
          id_method = input$bin_rep_id_opt,
          inst = input$bin_rep_inst_opt,
          coll_date = input$bin_rep_date_opt,
          seq_date = input$bin_rep_up_opt
        )[match(
          input$bin_rep_criteria,
          c("bin_rep_vouchered", "bin_rep_seq", "bin_rep_id", "bin_rep_inst", "bin_rep_date", "bin_rep_up")
        )]
      } else {
        list(
          vouchered = TRUE,
          seq_length = "COI_auto",
          id_method = c(
            "Morphology", "Morphology and sequence based",
            "Image based", "Image and sequence based"
          ),
          inst = "Centre for Biodiversity Genomics",
          coll_date = "latest",
          seq_date = "latest"
        )
      }

      tryCatch(
        {
          outdata$bin_reps <- get_bin_reps(
            convert_factor_char(outdata$data[processid %in% filtered_data()[input$data_table_rows_all, processid]],
              to_factor = FALSE, copy = TRUE
            ),
            Nreps = input$bin_rep_num,
            by.taxon = input$bin_rep_tax,
            non.redundant.taxa = input$bin_rep_non_redundant,
            enforce.scientific = input$bin_rep_scientific,
            criteria = criteria
          )$record_id
        },
        error = function(e) {
          showNotification(paste0("Error selecting BIN representatives:", e$message), type = "error")
        }
      )
    })
    if (!tab_status$bin_reps) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "bin_reps"), position = "after", select = TRUE,
        bslib::nav_panel(
          id = "bin_reps",
          value = "bin_reps",
          "BIN reps",
          div(
            id = "rep_table_area",
            DT::dataTableOutput("binrep_table") |> withSpinner(color = "#aaaaaa", type = 5, size = 0.6)
          )
        )
      )
      tab_status$bin_reps <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "bin_reps")
    }
  })

  # insert map tab when button is pressed (or jump to existing tab)
  observeEvent(input$map_btn, {
    req(nrow(outdata$data) > 0)
    if (!tab_status$map_tab) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "map_tab"), position = "after", select = TRUE,
        bslib::nav_panel(
          id = "map_tab",
          value = "map_tab",
          "Occurrence map",
          div(
            id = "map_area",
            leafletOutput("record_map")
          )
        )
      )
      shinyjs::delay(500, {
        tab_status$map_tab <- TRUE
        bslib::nav_select(id = "tabs", selected = "map_tab")
      })
    } else {
      bslib::nav_select(id = "tabs", selected = "map_tab")
    }
  })

  # plot/update records on the map (update function is further below)
  observeEvent(list(input$map_btn, tab_status$map_tab, input$tabs, input$data_table_rows_all, filtered_data()), {
    req(tab_status$map_tab, input$tabs == "map_tab")
    update_map()
  })

  # function used to update the rendered markers on the occurrence point map
  update_map <- function() {
    req(input$data_table_rows_all)
    # data <- filtered_data()[input$data_table_rows_all, ]
    data <- filtered_data()[input$data_table_rows_all, ]
    data <- unique(data[!empty(coord)], by = "processid")
    req(nrow(data) > 0)
    pal <- colorFactor(
      palette = map_colours,
      domain = data$identification
    )
    leafletProxy("record_map") |>
      clearMarkers() |>
      clearMarkerClusters() |>
      fitBounds(
        lng1 = min(data$lon),
        lat1 = min(data$lat),
        lng2 = max(data$lon),
        lat2 = max(data$lat)
      ) |>
      addCircleMarkers(
        lat = data$lat,
        lng = data$lon,
        color = "#444",
        fillColor = pal(data$identification),
        weight = 2,
        radius = 9,
        opacity = 0.3,
        fillOpacity = 0.7,
        popup = paste0(
          '<span class="tag">Sample ID:</span><span class="value"><a href="https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSpecimen?selectedrecordid=', data$processid, '" target="_blank">', data$sampleid, "</a></span>",
          '<span class="tag">Process ID:</span><span class="value">', data$processid, "</span>",
          '<span class="tag">Identification:</span><span class="value">', data$identification, "</span>",
          '<span class="tag">Country:</span><span class="value">', data$country.ocean, "</span>",
          '<span class="tag">Province/State:</span><span class="value">', data$province.state, "</span>",
          '<span class="tag">Region:</span><span class="value">', data$region, "</span>",
          '<span class="tag">Sector:</span><span class="value">', data$sector, "</span>",
          '<span class="tag">Site:</span><span class="value">', data$site, "</span>"
        ),
        clusterOptions = markerClusterOptions()
      )
  }

  # reactively subset the filtered data table based on selected fields
  out_table <- reactive({
    data <- filtered_data()
    data[, .SD, .SDcols = intersect(outdata$select_fields, names(data))]
  })

  # BIN reps are also updated reactively, using the same filters and field selections as the full data table
  rep_table <- reactive({
    req(outdata$data, outdata$bin_reps)
    outdata$data[record_id %in% outdata$bin_reps,
      .SD,
      .SDcols = intersect(outdata$select_fields, names(outdata$data))
    ]
  })

  ### Output rendering ----

  # build a reactive JSON lookup map and send it to the window
  # (enables hyperlinking of sample IDs and process IDs even when dependent columns are excluded)
  observeEvent(outdata$data, {
    req(outdata$data)
    json <- jsonlite::toJSON(outdata$data[, .(sampleid, processid, specimenid)], auto_unbox = TRUE)
    session$sendCustomMessage("initLookupMap", json)
  })

  # define options once, then add them to each table individually
  DT_extensions <- c("FixedColumns")
  DT_options <- list(
    headerCallback = header_js,
    columnDefs = list(
      list(
        targets = "_all",
        render = column_js
      )
    ),
    scrollCollapse = TRUE,
    lengthMenu = list(c(100, 500, 1000, 2000), c("100", "500", "1000", "2000")),
    # fixedHeader = TRUE,
    fixedColumns = list(leftColumns = 1),
    stateSave = TRUE,
    searchDelay = 500,
    initComplete = DT::JS("function(settings, json) {
                                            var dt = this.api();
                                            $(document).off('shown.bs.tab.dt').on('shown.bs.tab.dt', function() {
                                              dt.columns.adjust();
                                            });
                                          }")
  )

  # generate binmate list for display
  output$binmate_pids <- renderPrint(writeLines(outdata$binmates))

  # main data table
  output$data_table <- DT::renderDataTable(
    DT::datatable(
      {
        droplevels(out_table())
      },
      filter = "top",
      rownames = FALSE,
      selection = "none",
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options
    ),
    server = TRUE
  )

  # summary analysis table
  output$summary_table <- DT::renderDataTable(
    DT::datatable(
      {
        droplevels(outdata$summary)
      },
      rownames = FALSE,
      selection = "none",
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options
    ),
    server = TRUE
  )

  # diversity analysis summary stats
  output$div_stats <- renderUI({
    res <- outdata$div_profile
    asy_est <- if (!is.null(res$inext)) round(res$asy_est$Estimator, 1) else "-"
    shannon_h <- if (!is.null(res$shannon_div)) round(mean(as.numeric(unlist(res$shannon_div)), na.rm = TRUE), 2) else "—"
    total_b <- if (!is.null(res$total.beta)) round(mean(res$total.beta, na.rm = TRUE), 3) else "—"
    chip <- function(val, lbl) {
      div(
        class = "stat-chip",
        span(class = "chip-val", format(val, big.mark = ",")),
        span(class = "chip-lbl", lbl)
      )
    }
    div(
      class = "summary-strip",
      chip(res$counts$sites, get_div_label(outdata$div_profile$input_opts$location_type, plural = TRUE)),
      chip(res$counts$obs, "observations"),
      chip(res$counts$taxa, get_div_label(outdata$div_profile$input_opts$taxon_rank, plural = TRUE)),
      if (!is.null(res$inext)) bslib::tooltip(
        chip(asy_est, HTML("S<sub>est</sub>")),
        paste0("95% CI: ", round(res$asy_est$LCL, 1), '\u2013', round(res$asy_est$UCL, 1))),
      if (!is.null(res$shannon_div)) chip(shannon_h, "mean H′"),
      if (!is.null(res$total.beta)) chip(total_b, "mean β")
    )
  })

  # diversity analysis plots and tables
  output$div_plots <- renderUI({
    req(outdata$div_profile)
    res <- outdata$div_profile
    bslib::navset_card_pill(
      id = "div_nav",
      if (!is.null(res$inext)) {
        bslib::nav_panel("Rarefaction curve", value = "rarefy_panel", plotlyOutput("plot_rare"))
      },
      if (!is.null(res$preston.plot)) {
        bslib::nav_panel("Preston plot", value = "preston_panel", plotlyOutput("plot_preston"))
      },
      if (isTRUE(attr(res$total.beta, "Size") >= 3)) {
        bslib::nav_panel("Beta diversity plot",
          value = "beta_panel",
          plotlyOutput("plot_beta")
        )
      },
      if (res$input_opts$location_type == "grids") {
        bslib::nav_panel("Grid cells",
                         value = "grid_map",
                         leafletOutput("grid_map")
        )
      },
      if (!is.null(res$comm_table)) {
        bslib::nav_panel("Community matrix",
                         value = "comm_table",
                         DT::dataTableOutput("comm_matrix") |>
                           withSpinner(color = "#aaaaaa", type = 5, size = 0.6)
        )
      },
      if (!is.null(res$site_stats)) {
        bslib::nav_panel("Summary",
                         value = "site_stats",
                         DT::dataTableOutput("site_summary") |>
                           withSpinner(color="#aaaaaa", type=5, size=0.6)
        )
      },
      bslib::nav_spacer(),
      bslib::nav_item(
        actionButton("div_dl_btn", icon("download"))
      )
    )
  })

  # diversity analysis summary table
  output$site_summary <- DT::renderDataTable(
    DT::datatable(
      {
        req(outdata$div_profile)
        dt <- copy(outdata$div_profile$site_stats)
        # names are rendered with special characters here only (they do not save to CSV with proper encoding)
        data.table::setnames(dt, old = c("H'", "mean_beta"), new = c("H′", "mean_β"), skip_absent = TRUE)
        dt
      },
      filter = 'none',
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js,
      options = c(DT_options, list(paging = FALSE, searching = FALSE))) %>%
      DT::formatRound(columns = intersect(c("S_est", "95_CI_lower", "95_CI_upper"), names(dt)), digits = 1) %>%
      DT::formatRound(columns = intersect("H′", names(dt)), digits = 2) %>%
      DT::formatRound(columns = intersect("mean_β", names(dt)), digits = 3),
    server = TRUE
  )
  
  # community matrix table
  output$comm_matrix <- DT::renderDataTable(
    DT::datatable(
      {
        req(outdata$div_profile)
        outdata$div_profile$comm_table
      },
      filter = "none",
      rownames = FALSE,
      selection = "none",
      extensions = DT_extensions,
      callback = callback_js,
      options = c(DT_options, list(paging = FALSE, searching = FALSE))
    ),
    server = TRUE
  )
  
  # grid cell map
  output$grid_map <- renderLeaflet({
    req(outdata$div_profile$grids.data)
    res <- outdata$div_profile
    grids_wgs84 <- sf::st_transform(res$grids.data, crs = 4326)
    merge(grids_wgs84, res$site_stats[, .(`Grid cell`, N, S)])
    grids_wgs84 <- merge(grids_wgs84, res$site_stats[, .(cell.id = `Grid cell`, N, S)])
    grids_wgs84$label <- lapply(paste0('<span style="font-weight: 600;">', grids_wgs84$cell.id, '</span></br>',
                                       'N = ', grids_wgs84$N, '; S = ', grids_wgs84$S), HTML)
    leaflet(grids_wgs84) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = "#3A7D44",
        fillOpacity = 0.3,
        color       = "#1C2A1E",
        weight      = 1,
        label = ~label,
        highlightOptions = highlightOptions(
          fillOpacity = 0.6,
          weight      = 2,
          bringToFront = TRUE
        )
      )
  })

  # rarefaction curves
  output$plot_rare <- renderPlotly({
    res <- outdata$div_profile
    indiv <- !res$input_opts$presence_absence
    inext_res <- res$inext
    req(inext_res)
    df_inext <- if (!isolate(input$div_rare_by_site)) {
      inext_res$iNextEst$size_based
    } else {
      rbindlist(lapply(inext_res, function(x) x$iNextEst$size_based))
    }
    if(!indiv) names(df_inext)[names(df_inext) == "t"] <- "m"
    names(df_inext)[names(df_inext) == "Assemblage"] <- "site"
    # sort so that the overall results always come first
    df_inext$site <- factor(df_inext$site,
      levels = c(
        "Overall",
        sort(setdiff(
          unique(df_inext$site),
          "Overall"
        ))
      )
    )
    df_inext$tooltip <- paste0(
      if(indiv) paste0("<b>", df_inext$site, "</b><br>"),
      ifelse(indiv, "Individuals: ", "Incidences: "), round(df_inext$m, 1),
      "<br>Est. richness: ", round(df_inext$qD, 1),
      "<br>95% CI: ", round(df_inext$qD.LCL, 1),
      '\u2013', round(df_inext$qD.UCL, 1)
    )
    y_label <- paste0(get_div_label(outdata$div_profile$input_opts$taxon_rank, capitalize = TRUE), " richness")
    rare_plot <- ggplotly(
      ggplot() +
        # rarefaction line
        geom_line(
          data = df_inext[df_inext$Method != "Extrapolation", ],
          aes(x = m, y = qD, color = site),
          linewidth = 1, show.legend = TRUE
        ) +
        # extrapolated line
        geom_line(
          data = df_inext[df_inext$Method != "Rarefaction", ],
          aes(x = m, y = qD, color = site),
          linewidth = 1, linetype = "dashed"
        ) +
        # observed point
        geom_point(
          data = df_inext[df_inext$Method == "Observed", ],
          aes(x = m, y = qD, color = site),
          size = 2
        ) +
        # invisible points to anchor tooltips
        geom_point(
          data = df_inext,
          aes(x = m, y = qD, text = tooltip, color = site),
          size = 1.5, alpha = 0
        ) +
        # CI ribbon
        geom_ribbon(
          data = df_inext,
          aes(x = m, ymin = qD.LCL, ymax = qD.UCL, group = site, fill = site, color = site),
          alpha = 0.2, linewidth = 0, show.legend = FALSE
        ) +
        labs(x = paste0("Number of ", ifelse(indiv, "individuals", "incidences")),
             y = y_label, colour = "Site", fill = "Site") +
        theme_minimal() +
        guides(
          colour = guide_legend(override.aes = list(fill = NA, linetype = 1)),
          fill   = "none"
        ) +
        theme(legend.position = ifelse(uniqueN(df_inext$site) > 1, "right", "none")),
      tooltip = "text"
    ) |>
      layout(
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE)
      )
    # render the plot initially with only the overall rarefaction visible
    # all others can be toggled in the plotly legend
    for (i in seq_along(rare_plot$x$data)) {
      if (rare_plot$x$data[[i]]$name != "Overall") {
        rare_plot$x$data[[i]]$visible <- "legendonly"
      }
    }
    rare_plot
  })

  # Preston plot (basically just re-skinned output from BOLDconnectR)
  output$plot_preston <- renderPlotly({
    pr <- outdata$div_profile$preston.plot
    req(pr)
    y_label <- paste0("Number of ", get_div_label(outdata$div_profile$input_opts$taxon_rank, plural = TRUE))
    pr$layers$geom_bar$aes_params$fill <- "#80AAC4"
    pr$layers$geom_bar$aes_params$width <- 0.9
    pr$layers$geom_bar$aes_params$linewidth <- 0
    pr$layers$geom_point$aes_params$fill <- "#CC4945"
    pr$layers$geom_line$aes_params$linewidth <- 0.5
    pr_plot <- ggplotly(
      pr +
        theme_minimal() +
        ggtitle("") +
        ylab(y_label) +
        xlab("Abundance class"),
      tooltip = "none"
    ) |>
      layout(
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE)
      )
    pr_plot$x$data[[1]]$hovertemplate <- paste0("Observed: %{y:~g}", "<extra></extra>")
    pr_plot$x$data[[2]]$hovertemplate <- paste0("Fitted: %{y:~g}", "<extra></extra>")
    pr_plot
  })

  # Beta diversity ordination
  output$plot_beta <- renderPlotly({
    res <- outdata$div_profile
    req(res$total.beta)
    beta_mat <- res$total.beta
    validate(need(attr(beta_mat, "Size") >= 3, "Need at least 3 sites to run ordination."))
    nmds_res <- res$nmds.beta
    # create annotation for stress value
    stress_val <- round(nmds_res$stress, 3)
    stress_col <- fcase(stress_val > 0.2, "red",
      stress_val > 0.1, "#E07605",
      default = "green"
    )
    stress_text <- fcase(stress_val > 0.1, paste('<span style="font-weight: 700;">Stress = ', stress_val, "</span>"),
      nmds_res$stress == 0, '<span style="font-weight: 500;">Stress = 0</span>',
      stress_val == 0, '<span style="font-weight: 500;">Stress < 0.001</span>',
      default = paste('<span style="font-weight: 500;">Stress =', stress_val, "</span>")
    )
    # assemble NMDS results into data frame
    df_nmds <- data.frame(
      site  = rownames(res$comm.matrix),
      NMDS1 = nmds_res$points[, 1],
      NMDS2 = nmds_res$points[, 2]
    )
    beta_full <- as.matrix(beta_mat)
    diag(beta_full) <- NA
    df_nmds$mean_beta <- rowMeans(beta_full, na.rm = TRUE)
    df_nmds$tooltip <- paste0(
      "<b>", df_nmds$site, "</b><br>",
      "NMDS1: ", round(df_nmds$NMDS1, 3), "<br>",
      "NMDS2: ", round(df_nmds$NMDS2, 3), "<br>",
      "Mean \u03b2: ", round(df_nmds$mean_beta, 3)
    )
    plot_colours <- colorRampPalette(map_colours)(nrow(df_nmds))
    # identify outliers and scale axes to focus main cluster if necessary
    is_outlier <- (df_nmds$NMDS1 %in% boxplot.stats(df_nmds$NMDS1)$out) | (df_nmds$NMDS2 %in% boxplot.stats(df_nmds$NMDS2)$out)
    plot_subplot <- (diff(range(df_nmds$NMDS1)) / diff(range(df_nmds$NMDS1[!is_outlier]))) > 5
    if (plot_subplot) {
      xlim_zoom <- extendrange(range(df_nmds$NMDS1[!is_outlier]), f = c(0.1, 0.3))
      ylim_zoom <- extendrange(range(df_nmds$NMDS2[!is_outlier]), f = c(0.1, 0.3))
    } else {
      xlim_zoom <- extendrange(range(df_nmds$NMDS1), f = c(0.1, 0.1))
      ylim_zoom <- extendrange(range(df_nmds$NMDS2), f = c(0.1, 0.1))
    }
    # main plot
    plot_beta <- ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, label = site, colour = mean_beta)) +
      geom_point(aes(text = tooltip), size = 3.5, alpha = 0.85) +
      scale_colour_gradientn(
        name = "Mean \u03b2",
        colors = plot_colours
      ) +
      coord_cartesian(xlim = xlim_zoom, ylim = ylim_zoom) +
      labs(x = "NMDS1", y = "NMDS2") +
      theme_minimal() +
      theme(legend.position = "right")

    beta_fig <- ggplotly(plot_beta, tooltip = "text")
    main_x_style <- beta_fig$x$layout$xaxis
    main_y_style <- beta_fig$x$layout$yaxis
    # if there are outliers, add an inset plot with zoomed-out view
    if (plot_subplot) {
      beta_fig <- beta_fig |>
        add_markers(
          data = df_nmds,
          x = df_nmds$NMDS1,
          y = df_nmds$NMDS2,
          text = df_nmds$tooltip,
          hoverinfo = "text",
          name = "",
          xaxis = "x2", yaxis = "y2",
          marker = list(
            color = df_nmds$mean_beta,
            colorscale = lapply(seq_along(plot_colours), function(i) {
              c(
                (i - 1) / (length(plot_colours) - 1),
                plot_colours[i]
              )
            }),
            cmin = min(df_nmds$mean_beta), cmax = max(df_nmds$mean_beta),
            showscale = FALSE
          ),
          showlegend = FALSE
        ) |>
        layout(
          xaxis = list(tickmode = "auto"),
          yaxis = list(tickmode = "auto"),
          xaxis2 = list(
            domain = c(0.7, 0.95),
            anchor = "y2",
            fixedrange = TRUE,
            title = NA,
            tickfont = main_x_style$tickfont,
            linecolor = main_x_style$linecolor,
            gridcolor = main_x_style$gridcolor
          ),
          yaxis2 = list(
            domain = c(0.7, 0.95),
            anchor = "x2",
            fixedrange = TRUE,
            title = NA,
            tickfont = main_y_style$tickfont,
            linecolor = main_y_style$linecolor,
            gridcolor = main_y_style$gridcolor
          ),
          annotations = list(
            # subplot label
            list(
              text = '<span style="font-weight: 500;">Full plot with outliers</span>',
              x = 0.825, y = 0.96,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "bottom",
              showarrow = FALSE, font = list(size = 15, color = "#404040")
            ),
            # stress label (placed below inset if it exists)
            list(
              text = stress_text,
              x = 0.825, y = 0.65,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "top",
              showarrow = FALSE, font = list(size = 14, color = stress_col)
            )
          )
        )
    } else {
      beta_fig <- beta_fig |>
        layout(
          # stress label in top-right corner in the absence of an inset
          annotations = list(
            text = stress_text,
            x = 0.95, y = 0.95, xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "top", showarrow = FALSE,
            font = list(size = 12.5, color = stress_col)
          )
        )
    }
    beta_fig
  })

  # BIN consensus table
  output$bincons_table <- DT::renderDataTable(
    DT::datatable(
      {
        droplevels(outdata$bin_consensus)
      },
      filter = "top",
      rownames = FALSE,
      selection = "none",
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options
    ),
    server = TRUE
  )

  # BIN reps table
  output$binrep_table <- DT::renderDataTable(
    DT::datatable(
      {
        droplevels(rep_table())
      },
      filter = "top",
      rownames = FALSE,
      selection = "none",
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options
    ),
    server = TRUE
  )

  # base map for occurrence data
  output$record_map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "Base") |>
      addProviderTiles("Stadia.StamenTerrainBackground",
        options = providerTileOptions(noWrap = TRUE),
        group = "Terrain"
      ) |>
      addProviderTiles("Esri.WorldImagery",
        options = providerTileOptions(noWrap = TRUE),
        group = "Satellite"
      ) |>
      addScaleBar(position = "topright") %>%
      addLayersControl(
        baseGroups = c(
          "Base",
          "Terrain",
          "Satellite"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  ### Tab UI handling ----

    # convenience list object to handle download/copy from current tab 
    tab_data <- list(
      data = 
        list(basename = reactive("BOLD_fetch_"),
             output = out_table,
             current_rows = reactive(input$data_table_rows_all),
             copy_btns = list(copy_table = FALSE, copy_fasta = TRUE, save_fasta = TRUE),
             dl_btns = TRUE
          ),
      qhits_tab = 
        list(
          basename = reactive("query_hit_report_"),
          output = reactive(outdata$hits),
          current_rows = reactive(outdata$hits[, .I]),
          copy_btns = list(copy_table = FALSE, copy_fasta = FALSE, save_fasta = FALSE),
          dl_btns = TRUE),
      summary = 
        list(
          basename = reactive("summary_"),             output = reactive(outdata$summary),
          current_rows = reactive(input$summary_table_rows_all),
          copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, save_fasta = FALSE),
          dl_btns = TRUE),
      div_profile =
        list(
          basename = reactive({
            if(is.null(input$div_nav)) {
              "diversity_profile_"
            } else {
              tabs <- list("comm_table" = "community_matrix_",
                           "site_stats" = "site_diversity_")
              tab <- input$div_nav
              tabs[[tab]]
            }
          }),
          output = reactive({ outdata$div_profile[[input$div_nav]] }),
          current_rows = reactive({ outdata$div_profile[[input$div_nav]][, .I] }),
          copy_btns = list(copy_table = FALSE, copy_fasta = FALSE, copy_reps = FALSE),
          dl_btns = FALSE
        ),
      consensus_tab =
        list(basename = reactive("bin_consensus_"),
             output = reactive(outdata$bin_consensus),
             current_rows = reactive(input$bincons_table_rows_all),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, save_fasta = FALSE),
             dl_btns = TRUE),
      bin_reps = 
        list(basename = reactive("BIN_reps_"),
             output = rep_table,
             current_rows = reactive(input$binrep_table_rows_all),
             copy_btns = list(copy_table = FALSE, copy_fasta = TRUE, save_fasta = TRUE),
             dl_btns = TRUE),
      map_tab = 
        list(basename = reactive(NULL),
             output = NULL,
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, save_fasta = FALSE),
             dl_btns = FALSE)
    )
    
    # update button visibility depending on current tab
  observe({
    btns <- tab_data[[input$tabs]]$copy_btns
    for (i in seq_along(btns)) {
      shinyjs::toggleElement(id = names(btns)[i], condition = (btns[[i]] == TRUE))
    }
    req(outdata$data)
    shinyjs::toggleElement(
      id = "table_buttons",
      condition = tab_data[[input$tabs]]$dl_btns == TRUE
    )
  })
  
  # update button visibility on diversity profile tab
  observe({
    req(outdata$data, input$tabs == "div_profile")
    shinyjs::toggleElement(
      id = "table_buttons",
      condition = input$div_nav %in% c("site_stats", "comm_table")
    )
  })
    
    ### Data export logic ----

    # obtain FASTA-formatted sequences for current data
    get_fasta <- function(copy = TRUE) {
      collapse_mrkrs <- if(input$collapse_mrkrs == TRUE) { 
        coll_fields <- coll_mrkr_fields()[grepl(paste0("^",paste(input$filt_seq, collapse="|")), coll_mrkr_fields())]
        coll_fields[grepl("_nuc$", coll_fields)]
      } else {
        NULL
      }
      data <- if(input$tabs == "data") {
        id_col <- intersect(c("processid","sampleid"), names(tab_data[[input$tabs]]$output()))[1]
        ids <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), get(id_col)]
        if(input$collapse_mrkrs == TRUE) {
          collapsed_data()[get(id_col) %in% ids, ]
        } else if(!is.null(input$filt_seq)) {
          outdata$data[(get(id_col) %in% ids) & (marker_code %in% input$filt_seq), ]
        } else {
          outdata$data[get(id_col) %in% ids, ]
        }
      } else {
        tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
      }
      clip_fasta(data, collapse_mrkrs = collapse_mrkrs, to_cb = copy)
    }

    # copy button logic
    observeEvent(input$copy_table, { cb(as.data.frame(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ])) })
    observeEvent(input$copy_reps, { cb(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ][["processid"]], header = FALSE) })
    observeEvent(input$copy_fasta, { get_fasta(copy = TRUE) })
    observeEvent(input$copy_binmates, { cb(outdata$binmates, header = FALSE) })
    
    # column copy logic
    observeEvent(input$col_copy_clicked, {
      col_index <- input$col_copy_clicked$col  # JS uses 0-based index
      vals <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ][[col_index + 1]]
      vals[vals == "NA"] <- ""
      cb(vals, header = FALSE)
    })
    
    # download (save) logic
  download_output <- function(format) {
    downloadHandler(
      filename = function() {
        paste0(tab_data[[input$tabs]]$basename(), as.character(format(Sys.Date(), "%Y%m%d")), ".", format)
      },
      content = function(file) {
        data <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
        switch(format,
          tsv = fwrite(data, file, sep = "\t", na = "", quote = FALSE, row.names = FALSE),
          csv = fwrite(data, file, na = "", row.names = FALSE),
          xlsx = write_xlsx(data, file, format_headers = FALSE),
          fasta = writeLines(get_fasta(copy = FALSE), file))
      }
    )
  }

  # open logic
  observeEvent(input$open_xlsx, {
    data <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
    filename <- paste0(tab_data[[input$tabs]]$basename(), as.character(format(Sys.Date(), "%Y%m%d")), "_")
    temp <- tempfile(pattern = filename, fileext = ".xlsx")
    write_xlsx(data, temp, format_headers = FALSE)
    browseURL(temp)
  })
    
    # download (save) functions
    output$save_tsv <- download_output("tsv")
    output$save_csv <- download_output("csv")
    output$save_xlsx <- download_output("xlsx")
    output$save_fasta <- download_output("fasta")

# function to generate download handlers for diversity analysis results
  dl_div_profile <- function(result) {
    req(reactive(outdata$div_profile))
    datestamp <- as.character(format(Sys.Date(), "%Y%m%d"))
    filenames <- list(sum_table = paste0("site_diversity_", datestamp, ".csv"),
                      comm_mat = paste0("community_matrix_", datestamp, ".csv"),
                      preston_rds = paste0("preston_results_", datestamp, ".rds"),
                      preston_csv = paste0("preston_frequencies_", datestamp, ".csv"),
                      inext_rds = paste0("inext_results_", datestamp, ".rds"),
                      inext_size = paste0("inext_size_based_", datestamp, ".csv"),
                      inext_cov = paste0("inext_coverage_based_", datestamp, ".csv"),
                      inext_asy = paste0("inext_AsyEst_", datestamp, ".csv"),
                      shannon = paste0("shannon_div_", datestamp, ".csv"),
                      beta_dists = paste0("beta_dists_", datestamp, ".csv"),
                      nmds_rds = paste0("nmds_results_", datestamp, ".rds"),
                      nmds_scores = paste0("nmds_scores_", datestamp, ".csv"))
    div <- reactive(outdata$div_profile)
    # function to return the output object
    prep_content <- function(prep_result) {
      switch(prep_result,
             sum_table = div()$site_stats,
             comm_mat = div()$comm_table,
             preston_rds = div()$preston.res,
             preston_csv = data.frame(observed = div()$preston.res$freq,
                                      fitted   = div()$preston.res$fitted),
             inext_rds = div()$inext,
             inext_size =, inext_cov = {
               type <- switch(prep_result, inext_size = "size_based", inext_cov = "coverage_based")
               df_inext <- if (class(div()$inext) == "iNEXT") {
                 div()$inext$iNextEst[[type]]
               } else {
                 rbindlist(lapply(div()$inext, function(x) x$iNextEst[[type]]))
               }
               df_inext$Assemblage <- factor(df_inext$Assemblage, levels = c("Overall", sort(setdiff(unique(df_inext$Assemblage), "Overall"))))
               df_inext
             },
             inext_asy = {
               if (class(div()$inext) == "iNEXT") {
                 div()$inext$AsyEst
               } else {
                 rbindlist(lapply(div()$inext, function(x) x$AsyEst))
               }
             },
             shannon = data.table(div()$shannon_div, keep.rownames = div()$input_opts$location_type), 
             beta_dists = data.table(as.matrix(div()$total.beta), keep.rownames = div()$input_opts$location_type),
             nmds_rds = div()$nmds.beta,
             nmds_scores = {
               df_nmds <- data.frame(
                 site  = rownames(div()$comm.matrix),
                 NMDS1 = div()$nmds.beta$points[, 1],
                 NMDS2 = div()$nmds.beta$points[, 2]
               )
               beta_full <- as.matrix(div()$total.beta)
               diag(beta_full) <- NA
               df_nmds$mean_beta <- rowMeans(beta_full, na.rm = TRUE)
               df_nmds
             }
      )
    }
    # function to write the output to a file
    save_content <- function(result, file) {
      if(tools::file_ext(filenames[[result]]) == "rds") {
        saveRDS(prep_content(result), file)
      } else {
        write.csv(prep_content(result), file, , na = "", row.names = FALSE)
      }
    }
    # function for zipping all outputs into a single archive
    zip_content <- function(filenames, file) {
      tmpdir <- tempdir()
      files  <- sapply(seq_along(filenames), function(i) {
        tmpfile <- file.path(tmpdir, filenames[i])
        save_content(names(filenames[i]), tmpfile)
        tmpfile
      })
      zip(file, files, flags = "-j")
    }
    # download handler as return value
    downloadHandler(
      filename = function() {
        if(result == "all_zip") {
          paste0("diversity_profile_", datestamp, ".zip")
        } else {
          filenames[[result]]
        }
      },
      content = function(file) {
        if(result == "all_zip") {
          zip_content(filenames, file)
        } else {
          save_content(result, file)
        }
      }
    )
  }
  
  # diversity results modal download buttons are mapped to output
  output$all_zip <- dl_div_profile("all_zip")
  output$sum_table <- dl_div_profile("sum_table")
  output$comm_mat <- dl_div_profile("comm_mat")
  output$preston_rds <- dl_div_profile("preston_rds")
  output$preston_csv <- dl_div_profile("preston_csv")
  output$inext_rds <- dl_div_profile("inext_rds")
  output$inext_size <- dl_div_profile("inext_size")
  output$inext_cov <- dl_div_profile("inext_cov")
  output$inext_asy <- dl_div_profile("inext_asy")
  output$shannon <- dl_div_profile("shannon")
  output$beta_dists <- dl_div_profile("beta_dists")
  output$nmds_rds <- dl_div_profile("nmds_rds")
  output$nmds_scores <- dl_div_profile("nmds_scores")

output$fasta_head_ex <- renderPrint({
      rec <- config$fasta_ex
      ex_txt <- paste0(">", paste0(sapply(input$fasta_head_def, function(x) rec[[x]]), collapse = "|"), "\n",
                       rec$nuc)
      cat(ex_txt)
    })
    
    observeEvent(input$settings, {
      header_opt <- isolate(user_config$settings$fasta_head)
      showModal(
        modalDialog(
          title = "Settings",
          passwordInput( 
            "api_key_def",
            div(h3("BOLD API key"),
                bslib::tooltip(
                  icon("circle-question"),
                  paste0("The API key is saved to your system's credential store (",keystore,")."))
            ),
            value = tryCatch({
              key_get("BOLD.apikey")
            }, error = function(e){
              ""
            })
          ),
          inputGroup(title = "FASTA output",
                     groupId = "fasta_def",
                     selectizeInput("fasta_head_def",
                                    "Header labels:",
                                    choices = c(config$bcdmnames[match(header_opt, config$bcdmnames)], setdiff(config$bcdmnames, header_opt)),
                                    multiple = TRUE,
                                    options = list(plugins = list("drag_drop")),
                                    selected = if(is.null(header_opt)) {
                                      c("processid", "marker_code")
                                    } else {
                                      header_opt
                                    }
                     ),
                     HTML('<span style="margin-bottom: .125rem;">Example:</span>'),
                     verbatimTextOutput("fasta_head_ex")
          ),
          footer = tagList(
            div(),
            div(actionButton("save_settings_btn", "Save"),
                modalButton("Close"))),
          easyClose = TRUE
        )
      )
    })
    
    # settings save logic
    observeEvent(input$save_settings_btn, {
      removeModal()
      key <- trimws(gsub('"', "", isolate(input$api_key_def)))
      validate_apikey(key)
      header_opt <- isolate(input$fasta_head_def)
      user_config$settings$fasta_head <<- header_opt
      save_user_config()
      updateSelectizeInput(inputId = "fasta_head_def",
                           selected = header_opt)
    })

  }