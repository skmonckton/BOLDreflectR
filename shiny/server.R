server <- function(input, output, session) {
  ### Initialization ----

  # sessions from anything other than the app's own window are closed (see session_authorized() in global.R)
  if (!session_authorized(session$request)) {
    session$close()
    return(invisible(NULL))
  }

  session$onSessionEnded(function() {
    # clear temporary files on quit (otherwise they stick around)
    unlink(c(list.files(tempdir(), full.names = TRUE), list.files(open_files_path, full.names = TRUE)), recursive = TRUE)
    stopApp()
  })

  observeEvent(TRUE,
    {
      key <- ""
      try(key <- key_get("BOLD.apikey"), silent = TRUE)
      validate_apikey(key)
    },
    ignoreInit = FALSE,
    once = TRUE
  )

  bslib::accordion_panel_close("more_opts", "Additional fields")

  # BOLDconnectR takes several seconds to load, so its dependencies and imports are loaded step-wise in the background
  session$onFlushed(function() {
    queue <- "BOLDconnectR"
    tried <- character()
    step <- function() {
      while (length(queue) > 0 && isNamespaceLoaded(queue[1])) queue <<- queue[-1]
      if (length(queue) == 0) return()
      # packages that failed to load aren't queued again
      needs <- setdiff(Filter(Negate(isNamespaceLoaded), package_imports(queue[1])), tried)
      if (length(needs) > 0) {
        queue <<- c(needs, queue)
      } else {
        tried <<- c(tried, queue[1])
        try(suppressMessages(loadNamespace(queue[1])), silent = TRUE)
        queue <<- queue[-1]
      }
      later::later(step)
    }
    later::later(step, 2)
  }, once = TRUE)

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
  fetch_params <- reactiveValues(
    n_node_markers = 1
  )

  init_fetch_params <- list(
    query = list(),
    fetch_ids = NULL,
    fetch_by = NULL,
    search_token = NULL,
    binmates_checked = FALSE,
    binmates_fetched = FALSE,
    node_dtpkg = NULL,
    node_result = NULL
  )

  # initialize object to store fetched records and associated data
  outdata <- reactiveValues()

  init_outdata <- list(
    data = NULL,
    binmates = NULL,
    markers = NULL,
    filt_seq = NULL,
    filt_seq_idx = NULL,
    summary = NULL,
    bin_reps = NULL,
    div_profile = NULL,
    bin_consensus = NULL,
    bin_portal_stats = NULL,
    alignment = NULL,
    tree = NULL,
    query_info = NULL
  )

  # keep track of output tabs
  tab_status <- reactiveValues(
    overview = FALSE,
    data = FALSE,
    summary = FALSE,
    bin_reps = FALSE,
    consensus_tab = FALSE,
    aln_tab = FALSE,
    tree_tab = FALSE,
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

  test_mode <- reactiveVal()
  test_active <- reactiveVal(FALSE)
  session$onFlushed(function() {
    test_mode(isTRUE(user_config$settings$test_mode))
  }, once = TRUE)
  # if active, approved test functions are sourced into the session (failure reported, not fatal)
  session_env <- environment()
  load_test_funcs <- function(token = NULL) {
    path <- tstcode(token)
    if (!is.null(path)) {
      tryCatch(source(path, local = session_env),
               error = function(e) showNotification(paste0("Error in the test functions: ", conditionMessage(e)), type = "error"))
    }
  }
  observeEvent(input$tston, load_test_funcs())
  observeEvent(test_mode(), if (isTRUE(test_mode())) load_test_funcs(), once = TRUE)
  observeEvent(input$testkey_confirm, {
    removeModal()
    token <- trimws(gsub('"', "", input$testkey))
    req(nzchar(token))
    try(key_set_with_value("tctools-GH-PAT", password = token), silent = TRUE)
    load_test_funcs(token)
  })

  # conditional input logic to constrain marker search options (min/max requires a marker to be selected)
  observe({
    shinyjs::toggleState(id = "seq_min", condition = ((!is.null(input$seq_marker)) & (input$seq_marker != "")))
    shinyjs::toggleState(id = "seq_max", condition = ((!is.null(input$seq_marker)) & (input$seq_marker != "")))
    if ((is.null(input$seq_marker)) | (input$seq_marker == "")) {
      updateNumericInput(inputId = "seq_min", value = character())
      updateNumericInput(inputId = "seq_max", value = character())
    }
  })

  # keep data package options updated
  observe({
    updateSelectInput(
      inputId = "datapackage_id",
      choices = stats::setNames(user_datapkgs()$path, user_datapkgs()$id)
    )
  })


  # logic for multiple markers when searching local data
  observeEvent(input$node_add_marker, {
    i <- fetch_params$n_node_markers
    n <- i + 1
    fetch_params$n_node_markers <- n
    insertUI(
      selector = '.tab-pane[data-value="source-local"] div.marker-select div.single-marker:nth-last-child(1 of .single-marker)',
      where = "afterEnd",
      ui = div(
        class = "form-group shiny-input-container single-marker",
        selectInput(
          paste0("node_seq_marker_", n),
          label = "",
          selected = "",
          marker_options
        ),
        numericInput(
          paste0("node_seq_min_", n),
          label = "",
          value = NULL,
          min = 5, max = 2000, step = 1
        ),
        numericInput(
          paste0("node_seq_max_", n),
          label = "",
          value = NULL,
          min = 5, max = 2000, step = 1
        )
      )
    )
  })

  # remove marker from search
  observeEvent(input$node_del_marker, {
    i <- fetch_params$n_node_markers
    req(i > 1)
    n <- i - 1
    fetch_params$n_node_markers <- n
    shinyjs::runjs(paste0("Shiny.setInputValue(\"node_seq_marker_", i, "\", null);"))
    removeUI(
      selector = '.tab-pane[data-value="source-local"] div.marker-select div.single-marker:nth-last-child(1 of .single-marker)',
      immediate = TRUE
    )
  })

  # update select inputs to permit only one input per marker
  observeEvent(input$node_markers, {
    selected <- input$node_markers
    for (i in 1:isolate(fetch_params$n_node_markers)) {
      marker_i <- input[[paste0("node_seq_marker_", i)]]
      update_opts <- lapply(marker_options, function(m) m[!m %in% c(setdiff(selected, marker_i))])
      updateSelectInput(session,
        paste0("node_seq_marker_", i),
        choices = update_opts,
        selected = ifelse(is.null(marker_i), "", marker_i)
      )
    }
  })

  # disable option to find additional BIN records when fetching by BIN
  # (in this case, fetching BIN mates will always return zero records)
  observe({
    if ((input$fetch_by == "bin_uris") & (input$query_type == "fetch_opts")) {
      updateCheckboxInput(inputId = "fetch_binmates", value = FALSE)
    }
    shinyjs::toggleState(id = "fetch_binmates", condition = ((input$fetch_by != "bin_uris") | (input$query_type != "fetch_opts")))
  })

  # only show NTS toggle when data contains NTS records
  observe({
    nts_idx <- nts_idx()
    req(length(nts_idx) > 0)
    if (sum(nts_idx) > 0) {
      shinyjs::show("include_nts")
    } else {
      shinyjs::hide("include_nts")
    }
  })

  # reactively adjust available diversity analysis options
  observeEvent(c(input$div_presence, input$div_site_type),
    {
      allow <- list(
        "Rarefaction & extrapolation" = "rarefy",
        "Shannon diversity index" = "shannon",
        "Preston plot" = "preston",
        "Beta diversity index" = "beta"
      )

      if (input$div_presence) allow <- allow[allow %in% c("rarefy", "beta")]
      if (input$div_site_type %in% c("none", "taxa")) allow <- allow[allow %in% c("rarefy", "shannon")]

      updateSelectizeInput(
        inputId = "div_profile",
        choices = allow,
        selected = input$div_profile[input$div_profile %in% allow]
      )
    },
    ignoreInit = TRUE
  )
  observe({
    shinyjs::toggleElement(id = "div_grid_size", condition = (input$div_site_type == "grids"))
    shinyjs::toggleElement(id = "div_location_type", condition = (input$div_site_type == "locations"))
    shinyjs::toggleElement(id = "div_higher_tax", condition = (input$div_site_type == "taxa"))
    shinyjs::toggleElement(id = "div_beta_type", condition = ("beta" %in% input$div_profile))
    shinyjs::toggleElement(id = "div_rare_by_site", condition = ("rarefy" %in% input$div_profile) & (!input$div_presence) & (input$div_site_type != "none"))
  })
  observe({
    req(filtered_data())
    data <- shown_data()
    req(nrow(data) > 0)
    fallback_count <- uniqueN(data[!(empty(bin_uri) & empty(species)), .(bin_uri, species)])
    toggleElement(id = "div-analysis-opts", condition = (fallback_count >= 2) & (nrow(data) >= 10))
    toggleElement(id = "div-analysis-msg", condition = (fallback_count < 2) | (nrow(data) < 10))
    req((fallback_count >= 2) & (nrow(data) >= 10))
    site_types <- list(
      "Sites" = "site",
      "Sectors" = "sector",
      "Regions" = "region",
      "Provinces/States" = "province.state",
      "Countries/Oceans" = "country.ocean",
      "Ecoregions" = "ecoregion",
      # "Biomes" = "biome",
      "Realms" = "realm"
    )
    higher_ranks <- list(
      "Orders" = "order",
      "Families" = "family",
      "Subfamilies" = "subfamily",
      "Tribes" = "tribe",
      "Genera" = "genus"
    )
    taxon_ranks <- list(
      "Species if known, BIN if not" = "bin_fallback",
      "BIN" = "bin_uri",
      "Species" = "species"
    )
    assemblages <- c(site_types, higher_ranks)
    asssem_counts <- sapply(assemblages, function(x) uniqueN(data[[x]][!empty(data[[x]])]))
    allow_assemblages <- setNames(
      unname(assemblages)[asssem_counts >= 2],
      sapply(unlist(unname(c(assemblages[asssem_counts >= 2]))), function(x) {
        val_count <- uniqueN(data[[x]][!empty(data[[x]])])
        paste0(
          names(assemblages)[which(assemblages == x)],
          " (n=", val_count, ")"
        )
      })
    )
    allow_tax <- names(which(unlist(data[, lapply(.SD, function(x) uniqueN(x[!empty(x)]) >= 2),
      .SDcols = unlist(unname(taxon_ranks[taxon_ranks != "bin_fallback"]))
    ])))
    if (all(c("bin_uri", "species") %in% allow_tax) | fallback_count >= 2) {
      allow_tax <- c(allow_tax, "bin_fallback")
    }
    updateSelectInput(session, "div_location_type",
      choices = allow_assemblages[allow_assemblages %in% site_types],
      selected = unlist(allow_assemblages[allow_assemblages %in% site_types])[1]
    )
    updateSelectInput(session, "div_higher_tax",
      choices = allow_assemblages[allow_assemblages %in% higher_ranks],
      selected = unlist(allow_assemblages[allow_assemblages %in% higher_ranks])[1]
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
    # (the displayed columns follow input$filt_opt via computed_fields())
    updateSelectizeInput(session, "filt_opt",
      choices = filter_options(include_verbatim),
      selected = try(user_config$settings$col_set, silent = TRUE)
    )
    # default marker filter: applied once the markers in the data are known (see observer on outdata$markers)
    markers <- isolate(outdata$markers)
    if (is.null(markers)) {
      apply_default_markers <<- TRUE
    } else {
      updateSelectizeInput(session, "filt_seq",
        choices = markers,
        selected = intersect(user_config$settings$filt_seq, markers)
      )
    }
  }

  # flag to apply the default marker filter when newly fetched data is populated
  apply_default_markers <- TRUE

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
    bslib::update_switch("include_nts", value = isTRUE(user_config$settings$include_nts))
    bslib::update_switch("collapse_mrkrs", value = isTRUE(user_config$settings$collapse_mrkrs))
    updateCheckboxInput(inputId = "bc_portal_stats", value = FALSE)
  }

  # execute reset function when button is pressed
  observeEvent(input$reset_btn, {
    reset_filter(is.null(fetch_params$search_token$node_result))
  })

  # reactively assemble node search query
  node_query <- reactive({
    n_node_markers <- fetch_params$n_node_markers
    node_query <- list(
      ids = split_query(input$node_id_list),
      bins = split_query(input$node_bin_list),
      dataset.projects = split_query(input$node_recset_list),
      taxonomy = split_query(input$node_tax),
      geography = split_query(input$node_geo, sep = "\n"),
      identified.by = split_query(input$node_ider_list),
      biogeo.cat = split_query(input$node_biogeo_list, sep = "\n"),
      institutes = split_query(input$node_inst_list, sep = "\n"),
      seq.source = split_query(input$node_seqinst_list, sep = "\n"),
      bounding.box = c(input$node_lon_range, input$node_lat_range),
      marker = sapply(1:n_node_markers, function(m) input[[paste0("node_seq_marker_", m)]]),
      basecount = setNames(
        lapply(1:n_node_markers, function(m) {
          vals <- c(input[[paste0("node_seq_min_", m)]], input[[paste0("node_seq_max_", m)]])
          if (!all(is.na(vals))) {
            minmax <- c(5, 2000)
            minmax[!is.na(vals)] <- vals[!is.na(vals)]
            minmax
          } else {
            NULL
          }
        }),
        sapply(1:n_node_markers, function(m) input[[paste0("node_seq_marker_", m)]])
      )
    )
    if (length(node_query$bounding.box) != 4 || isTRUE(all(node_query$bounding.box == c(-180, 180, -90, 90)))) {
      node_query$bounding.box <- NULL
    }
    node_query$marker <- if (all(empty(node_query$marker))) {
      NULL
    } else {
      node_query$marker[!empty(node_query$marker)]
    }
    node_query$basecount <- node_query$basecount[!sapply(node_query$basecount, is.null)]
    for (q in names(node_query)) {
      if (is.null(unlist(node_query[[q]]))) node_query[[q]] <- NULL
    }
    node_query
  })

  ### Data fetching ----

  # set API key and generate fetch_ids
  observeEvent(input$fetch_btn | input$fetch_ctrl_enter,
    {
      # if sidebar key input is filled, set key and save it if 'Remember' is ticked; otherwise use the session's key
      if (isTruthy(input$api_key)) {
        validate_apikey(trimws(gsub('"', "", input$api_key)), remember = isTRUE(input$remember_key))
      } else if (!nzchar(Sys.getenv("api_key"))) {
        validate_apikey("")
      }
      if (!(((isTruthy(input$fetch_id_list) || isTruthy(input$search_tax) || isTruthy(input$search_geo) ||
        isTruthy(input$seq_marker)) && isTruthy(Sys.getenv("api_key"))) ||
        (isTruthy(length(node_query()) > 0) && isTruthy(input$datapackage_id)))) {
        showNotification("Nothing to search for: enter identifiers or search terms first.", id = "fetch_msg", type = "warning")
        return()
      }
      shinyjs::hide("main_panel")
      # immediate feedback in case of any delay
      showNotification(if (input$query_type == "source-local") "Searching data package for matching records..." else "Contacting BOLD...",
                       id = "fetch_msg", duration = NULL, type = "message")
      shinyjs::hide("table_buttons")
      reset_ui()
      reset_filter(input$query_type != "source-local")
      tab_status$data <- TRUE
      shinyjs::show("main_panel")
      tryCatch(
        {
          if (input$query_type == "source-local") {
            node_params <- node_query()
            fetch_params$query <- node_params
            node_params[["input.parquet"]] <- node_path(input$datapackage_id)
            if (!is.null(node_params$dataset.projects)) node_params$dataset.projects <- whole_code_patterns(node_params$dataset.projects)
            showNotification("Searching data package for matching records...", id = "fetch_msg", duration = NULL, type = "message")
            num_records <- 0
            withCallingHandlers(
              {
                result <- do.call(BOLDNODE::bold_parquet_search, node_params)
              },
              message = function(m) {
                notif_txt <- clean_ansi(conditionMessage(m))
                if (grepl("^The search has", notif_txt)) {
                  num_records <<- as.numeric(regmatches(notif_txt, gregexpr("(?<=has )[0-9,]+(?= records)", notif_txt, perl = TRUE))[[1]][1])
                }
                if (!grepl("shared_home", notif_txt)) {
                  showNotification(notif_txt, id = "fetch_msg", duration = NULL, type = "message")
                }
              }
            )
            fetch_params$node_dtpkg <- input$datapackage_id
            fetch_params$search_token <- list(
              num_of_accessible = num_records,
              num_of_records = num_records,
              limit = Inf,
              node_result = result
            )
            if (num_records == 0) {
              showNotification("No records found matching search terms.", id = "fetch_msg", type = "warning")
            } else if (num_records > user_config$settings$max_records) {
              too_many_records(num_records)
            } else if (num_records >= 100000) {
              search_confirm_modal()
            } else {
              collect_node_data()
            }
          } else if (input$query_type == "search_opts") {
            showNotification("Searching for matching records...", id = "fetch_msg", duration = NULL, type = "message")
            search_query <- list(
              taxonomy = split_query(input$search_tax, list = TRUE),
              geography = split_query(input$search_geo, list = TRUE, sep = "\n"),
              marker = input$seq_marker[input$seq_marker != ""],
              marker_min_length = input$seq_min[!is.na(input$seq_min)],
              marker_max_length = input$seq_max[!is.na(input$seq_max)],
              collection_start_date = input$search_dates[1][!is.na(input$search_dates[1])],
              collection_end_date = input$search_dates[2][!is.na(input$search_dates[2])],
              institutes = split_query(input$search_inst, list = TRUE, sep = "\n")
            )
            fetch_params$query <- search_query[sapply(search_query, length) > 0]
            search_token <- do.call(bold.full.search.step1, search_query[sapply(search_query, length) > 0])
            if (is.null(search_token)) {
              showNotification("No records found matching search terms.", id = "fetch_msg", type = "warning")
            } else if (search_token$num_of_accessible > user_config$settings$max_records) {
              too_many_records(search_token$num_of_accessible)
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
        # a warning stops the search: show it (it was reported as "no results", which hid real problems)
        warning = function(w) {
          showNotification(conditionMessage(w), id = "fetch_msg", type = "warning")
        },
        error = function(e) {
          showNotification(paste0("Error processing record identifiers: ", e$message), id = "fetch_msg", type = "error")
        }
      )
      # record query parameters (calls describe_query() from global.R) once data arrives
      # empty or declined searches do not relabel earlier data
      pending_query_info <<- describe_query(input$query_type, fetch_params$query,
                                            datapackage = tools::file_path_sans_ext(basename(input$datapackage_id)))
      if (!is.null(outdata$data)) outdata$query_info <- pending_query_info
    },
    ignoreInit = TRUE
  )

  # searches over the maximum in Settings are not fetched (very large results can exhaust memory)
  too_many_records <- function(n) {
    showNotification(sprintf("The search matched %s records, more than the maximum of %s per search (Settings > Default options). Narrow the search, or raise the maximum.",
                             format(n, big.mark = ",", scientific = FALSE), format(user_config$settings$max_records, big.mark = ",", scientific = FALSE)),
                     id = "fetch_msg", type = "error", duration = 15)
  }

  # the data table is shown only while there's data, so an empty, failed or declined search can't leave earlier
  # results on screen after they've been cleared
  observe(shinyjs::toggleElement("table_area", condition = !is.null(outdata$data)))
  pending_query_info <- NULL

  # modal to call up upon finding over 100k records from search
  search_confirm_modal <- function() {
    req(fetch_params$search_token)
    search_token <- fetch_params$search_token
    modal_msg <- div(
      p(HTML(paste0("Search returned <strong>", format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE), "</strong> accessible records."))),
      p("Large fetches can take several minutes and use a lot of memory (about 2 GB for 150,000 records).")
    )
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
            actionButton("search_cancel_btn", "No")
          )
        ),
        easyClose = FALSE
      )
    )
  }

  # declining a large fetch clears its progress message (table already hidden as there's no data)
  observeEvent(input$search_cancel_btn, {
    removeModal()
    removeNotification(id = "fetch_msg")
  })

  # upon confirming search, proceed to get fetch_ids (process IDs)
  observeEvent(input$search_confirm_btn, {
    removeModal()
    search_token <- fetch_params$search_token
    if (input$query_type == "source-local") {
      collect_node_data()
    } else {
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
    }
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

  # collect data from local data package
  collect_node_data <- function() {
    req(fetch_params$search_token)
    showNotification(
      paste0(
        "Collecting ", format(isolate(fetch_params$search_token$num_of_accessible), big.mark = ","),
        " records from data package..."
      ),
      id = "fetch_msg", duration = NULL, type = "message"
    )
    data <- as.data.table(BOLDNODE::bold_search_collect(fetch_params$search_token$node_result))
    fetch_params$fetch_by <- "processid"
    showNotification("Preparing table...", id = "fetch_msg", duration = NULL, type = "message")
    populate_data(data)
    showNotification("Data assembled.", id = "fetch_msg", duration = 2, type = "message")
  }

  # populate data
  populate_data <- function(data) {
    # each record has at most one row per marker
    data <- unique(data, by = intersect(c("processid", "marker_code"), names(data)))
    # show records in the order of the query terms
    data <- order_by_query(data, fetch_params$query)
    data[, c("id_date_parsed", "project_code", "lat", "lon") := c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", "processid")), parse_lat_lon(coord))]
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
    outdata$query_info <- pending_query_info
    if (!tab_status$overview) {
      bslib::nav_insert(
        "tabs",
        target = "data", position = "before", select = FALSE,
        bslib::nav_panel(id = "overview", value = "overview", "Overview", uiOutput("overview"))
      )
      tab_status$overview <- TRUE
    }

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
      if (!binmate_source_ok()) return(invisible())
      binmate_pids <- tryCatch(get_binmates(outdata$data[!empty(bin_uri)], fetch_params$node_dtpkg), error = function(e) {
        showNotification(paste0("Could not look for additional BIN members: ", conditionMessage(e)), type = "error")
        NULL
      })
      req(!is.null(binmate_pids))
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

  # source data package of records must still exist to look up additional BIN members
  binmate_source_ok <- function() {
    dtpkg <- fetch_params$node_dtpkg
    if (is.null(dtpkg) || file.exists(dtpkg)) return(TRUE)
    showNotification("Couldn't add BIN members: the data package these records came from is no longer available.",
                     type = "warning")
    FALSE
  }

  fetch_binmates <- function(dtpkg_parquet = fetch_params$node_dtpkg) {
    if (!binmate_source_ok()) return(invisible())
    binmate_data <- if (is.null(dtpkg_parquet)) {
      as.data.table(bold.fetch.shiny(
        get_by = "processid",
        query = outdata$binmates,
        BCDM_only = FALSE
      ))
    } else {
      withInfProgress("Collecting additional BIN members from data package...", {
        tryCatch(
          as.data.table(
            BOLDNODE::bold_search_collect(
              BOLDNODE::bold_parquet_search(node_path(dtpkg_parquet), ids = outdata$binmates)
            )
          ),
          error = function(e) NULL
        )
      })
    }
    add_binmates(binmate_data)
  }

  add_binmates <- function(binmate_data) {
    if (is.null(binmate_data) || nrow(binmate_data) == 0) {
      showNotification("No additional BIN member records could be retrieved.", id = "fetch_msg", type = "warning")
      return(invisible())
    }
    withInfProgress("Processing additional BIN member data...", {
      # never add records that are already present (comparing whole rows is not enough, since values can
      # differ between API calls, e.g. depending on the API key's access to private data)
      binmate_data <- unique(binmate_data[!processid %in% outdata$data$processid], by = c("processid", "marker_code"))
      req(nrow(binmate_data) > 0)
      binmate_data <- binmate_data[, c("id_date_parsed", "project_code", "lat", "lon") :=
        c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", "processid")), parse_lat_lon(coord))]
      convert_factor_char(binmate_data)
      # list BIN members in the same BIN order as the original results
      binmate_data <- binmate_data[order(match(as.character(bin_uri), unique(as.character(outdata$data$bin_uri))))]

      data <- rbindlist(
        list(
          outdata$data,
          binmate_data
        ),
        fill = TRUE
      )
    })
    showNotification("Additional BIN members retrieved.",
      id = "fetch_msg", type = "message"
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
    updateSelectInput(session, "ana_opt", choices = summarize_by_choices(names(outdata$data)))
  })

  user_datapkgs <- reactiveVal(datapkgs)
  # announce files from packages folder that couldn't be read (shown when the data package manager opens)
  notify_skipped_datapackages <- function() {
    if (length(datapkgs_skipped) > 0) {
      showNotification(paste0("The following files in the data packages folder could not be read (may be damaged or incomplete): ",
                              paste(datapkgs_skipped, collapse = ", ")), type = "warning", duration = 8, id = "skipped_pkgs")
    }
  }

  # modal logic for data package options
  observeEvent(input$datapackage_btn, {
    delete_check <<- TRUE
    notify_skipped_datapackages()
    showModal(
      modalDialog(
        title = tagList(div("Saved data packages")),
        HTML('Data packages must be in Parquet format. BOLD data packages are available for download <a href="https://bench.boldsystems.org/index.php/datapackages/" target="_blank" rel="noopener noreferrer">here <i class="fa fa-external-link" aria-hidden="true"></i></a>.'),
        DT::dataTableOutput("datapkg_table"),
        actionLink("datapackage_folder",
          icon = HTML('<i role="presentation" aria-label="folder icon" class="far fa-folder-open"></i>'),
          label = "Show folder"
        ),
        div(
          class = "flex-div",
          div(hidden(div(id = "delete_confirm", class = "warn-text", "Are you sure? File(s) will be permanently deleted."))),
          div(
            actionButton("datapackage_delete",
              icon = HTML('<i role="presentation" aria-label="trash icon" class="fa fa-trash"></i>'),
              label = "Delete"
            ),
            hidden(actionButton(inputId = "datapackage_del_cancel", icon = icon("x"), label = "Cancel")),
            actionButton("datapackage_browse",
              icon = bsicons::bs_icon("database-fill-add"),
              # icon = HTML('<i role="presentation" aria-label="folder icon" class="far fa-folder"></i>'),
              label = "Add..."
            )
          )
        ),
        # shown only when no file dialog is available (outside the desktop app, on systems without one)
        hidden(div(
          id = "datapackage_path_entry",
          class = "flex-div",
          textInput("datapackage_path_manual", "No file dialog is available on this system. Full path to a Parquet data package:", width = "100%"),
          actionButton("datapackage_path_add", "Add")
        )),
        footer = tagList(div(), actionButton("datapackage_close", "Close")),
        easyClose = FALSE,
        size = "l"
      )
    )
  })
  observeEvent(input$datapackage_close, {
    # the list used to save and delete packages is refreshed too (packages may have been added via "Show folder")
    datapkgs <<- update_datapackages()
    user_datapkgs(datapkgs)
    removeModal()
  })
  # open user data package directory
  observeEvent(input$datapackage_folder, {
    pkg_dir <- file.path(user_data_path, "BOLD_data_packages")
    dir.create(pkg_dir, showWarnings = FALSE, recursive = TRUE)
    open_path(pkg_dir)
  })

  # report any problem opening a file or folder via electron
  observeEvent(input$open_path_error, {
    showNotification(paste0("Could not open file or folder: ", input$open_path_error), type = "error")
  })

  # report any problem copying to the clipboard via electron
  observeEvent(input$clipboard_error, {
    showNotification(paste0("Could not copy to clipboard: ", input$clipboard_error), id = "copy_msg", type = "error")
  })

  # choose a data package file: in the desktop app, electron's native dialog is used (works on all platforms and
  # opens in the foreground); otherwise the best native dialog for the system is used (RStudio, Windows, macOS,
  # zenity/kdialog on Linux), and if there is none, a path can be typed or pasted instead
  observeEvent(input$datapackage_browse, {
    if (in_electron) {
      session$sendCustomMessage("choose_file", list(
        inputId = "datapackage_file",
        title = "Select a data package",
        filters = list(list(name = "Parquet data packages", extensions = list("parquet")))
      ))
    } else {
      path <- choose_file_native("Select a data package", ext = "parquet")
      if (identical(path, NA_character_)) {
        shinyjs::show("datapackage_path_entry")
      } else {
        add_datapackage_file(path)
      }
    }
  })
  observeEvent(input$datapackage_file, {
    add_datapackage_file(input$datapackage_file)
  })
  observeEvent(input$datapackage_path_add, {
    path <- trimws(gsub('^"|"$', "", input$datapackage_path_manual))
    req(nzchar(path))
    if (!file.exists(path)) {
      showNotification("File not found. Please check the path.", id = "fileio", type = "error")
    } else {
      add_datapackage_file(path)
      updateTextInput(inputId = "datapackage_path_manual", value = "")
    }
  })

  # record selected file info and copy to user data directory
  add_datapackage_file <- function(path) {
    if (!is.null(path) && nzchar(path)) {
      if (tools::file_ext(path) != "parquet") {
        showNotification("Only data packages in Parquet format are currently supported.", type = "error")
      } else {
        showNotification(paste0("Copying ", basename(path), " to user data folder..."), duration = NULL, id = "fileio", type = "message")
        tryCatch(
          {
            datapkgs <- modify_datapackage(path, operation = "save")
            new_path <- datapkgs[id == clean_dtpkg_id(basename(path)), path]
            updateSelectInput(inputId = "datapackage_id", choices = stats::setNames(datapkgs$path, datapkgs$id), selected = new_path[1])
            user_datapkgs(datapkgs)
          },
          error = function(e) {
            showNotification(paste0("Error saving data package: ", e$message), duration = NULL, id = "fileio", type = "error")
          }
        )
      }
    }
  }

  delete_check <- TRUE

  # delete selected data package
  observeEvent(input$datapackage_delete, {
    req(input$datapkg_table_rows_selected)
    delete_ids <- user_datapkgs()[input$datapkg_table_rows_selected, id]
    if (delete_check) {
      shinyjs::show("delete_confirm")
      updateActionButton(inputId = "datapackage_delete", icon = icon("check"), label = "Delete")
      shinyjs::show("datapackage_del_cancel")
      delete_check <<- FALSE
    } else {
      shinyjs::hide("delete_confirm")
      tryCatch(
        {
          for (id in delete_ids) {
            datapkgs <- modify_datapackage(id, operation = "delete")
            updateSelectInput(inputId = "datapackage_id", choices = stats::setNames(datapkgs$path, datapkgs$id), selected = datapkgs$path[1])
          }
          user_datapkgs(datapkgs)
        },
        error = function(e) {
          showNotification(paste0("Error deleting data package: ", e$message), duration = NULL, id = "fileio", type = "error")
        }
      )
      shinyjs::hide("datapackage_del_cancel")
      updateActionButton(
        inputId = "datapackage_delete",
        icon = HTML('<i role="presentation" aria-label="trash icon" class="fa fa-trash"></i>'),
        label = "Delete"
      )
      delete_check <<- TRUE
    }
  })

  observeEvent(input$datapackage_del_cancel, {
    shinyjs::hide("delete_confirm")
    shinyjs::hide("datapackage_del_cancel")
    updateActionButton(
      inputId = "datapackage_delete",
      icon = HTML('<i role="presentation" aria-label="trash icon" class="fa fa-trash"></i>'),
      label = "Delete"
    )
    delete_check <<- TRUE
  })

  output$datapkg_table <- DT::renderDataTable(DT::datatable(
    {
      if (nrow(user_datapkgs()) > 0) {
        user_datapkgs()[, .(
          `ID` = id,
          `Records` = format(rows, big.mark = ",", scientific = FALSE),
          `Size` = Vectorize(utils:::format.object_size)(as.numeric(size_kb), units = "auto", standard = "SI", digits = 2),
          `Date added` = as.IDate(created)
        )]
      } else {
        data.table(`ID` = character(), `Rows` = character(), `Size` = character(), `Date added` = character())
      }
    },
    rownames = FALSE,
    options = list(
      dom = "t"
    )
  ))

  # logic for saving data to parquet data package
  observeEvent(input$save_dtpkg, {
    save_datapkg_modal()
  })
  save_datapkg_modal <- function() {
    data <- outdata$data
    req(data)
    filt_keep <- data$processid %in% visible_pids()
    data_choices <- list(
      full = nrow(data),
      filtered = sum(filt_keep)
    )
    ask_choice <- nrow(data) != sum(filt_keep)
    replace_check(TRUE)
    showModal(
      modalDialog(
        title = "Save data package",
        div("Save current data in Parquet format for later offline access."),
        if (ask_choice) {
          radioButtons("save_dtpkg_source",
            HTML('<span style="font-weight:500;">Include:</span>'),
            choices = setNames(
              c("full", "filtered"),
              c(
                paste0("All data (", nrow(data), " records)"),
                paste0("Filtered data (", sum(filt_keep), " records)")
              )
            ),
            width = "100%"
          )
        } else {
          div(class = "form-group", HTML(paste0('<span style="font-weight:500;">Current data:</span> ', nrow(data), " records")))
        },
        textInput("save_dtpkg_name", HTML('<span style="font-weight:500;">ID/alias:</span>'), width = "100%"),
        footer = tagList(
          div(),
          div(
            actionButton("exp_dtpkg_btn", "Save"),
            modalButton("Close")
          )
        )
      )
    )
  }
  replace_check <- reactiveVal(TRUE)
  observeEvent(input$exp_dtpkg_btn, {
    req(input$save_dtpkg_name, user_datapkgs())
    # compare using the cleaned ID, since that is what the file will be saved as
    pkgid <- clean_dtpkg_id(input$save_dtpkg_name)
    if (!nzchar(pkgid)) {
      showNotification("Please provide a valid ID/alias for the data package.", id = "fileio", type = "error")
    } else if ((pkgid %in% user_datapkgs()$id) & replace_check()) {
      insertUI("#save_dtpkg_name",
        where = "afterEnd",
        ui = tagList(div(
          id = "exp_dtpkg_warn", class = "warn-text",
          HTML("A data package with this name already exists. </br>If you continue, it will be overwritten.")
        ))
      )
      updateActionButton(inputId = "exp_dtpkg_btn", label = "Continue")
      replace_check(FALSE)
    } else {
      tryCatch(
        {
          temp <- tempfile(fileext = ".parquet")
          data <- if (isTRUE(input$save_dtpkg_source == "filtered")) {
            outdata$data[processid %in% visible_pids()]
          } else {
            outdata$data
          }
          arrow::write_parquet(as_datapackage(data), temp, compression = "zstd", compression_level = 3)
          # an existing package with the same ID is only reached here once the user has confirmed overwriting it
          datapkgs <- modify_datapackage(temp, filename = pkgid, operation = "save", overwrite = TRUE)
          unlink(temp)
          user_datapkgs(datapkgs)
          replace_check(TRUE)
          removeModal()
        },
        error = function(e) {
          showNotification(paste0("Error saving data package:", e$message), type = "error")
        }
      )
    }
  })
  observeEvent(input$save_dtpkg_name, {
    req(input$save_dtpkg_name, replace_check() == FALSE)
    removeUI("#exp_dtpkg_warn")
    updateActionButton(inputId = "exp_dtpkg_btn", label = "Save")
    replace_check(TRUE)
  })

  # logic for generating marker-collapsed data table
  collapsed_data <- reactive({
    req(outdata$data)
    if (input$collapse_mrkrs == TRUE) {
      collapse_markers(outdata$data)
    } else {
      NULL
    }
  })

  # reactive to store additional marker-specific fields/columns
  # (empty when markers are not collapsed, so that field selection never depends on a cancelled reactive)
  coll_mrkr_fields <- reactive({
    if (is.null(outdata$data) || !isTRUE(input$collapse_mrkrs)) {
      return(character(0))
    }
    setdiff(names(collapsed_data()), c(field_labels, unlist(config$fieldsets$parsed)))
  })

  # reactive object for user-selected fields/column filters
  # (this is the single source of truth for the displayed columns, so default column sets apply as soon as data is fetched)
  computed_fields <- reactive({
    fieldset_version()
    # if no column filters have been selected, select all fields
    if (length(input$filt_opt) == 0) {
      fields <- unname(unlist(config$fieldsets$bcdm))
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
            fields <- unique(c(fields, unname(unlist(config$fieldsets[o]))))
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
    # in the marker-collapsed table, sequence fields are split into one column per marker (e.g. COI-5P_nuc);
    # show those columns in place of any selected sequence fields
    cmf <- coll_mrkr_fields()
    if (length(cmf) > 0) {
      pivot_field <- sub("^.*?_(insdc_acs|nuc_basecount|nuc|sequence_run_site|sequence_upload_date)$", "\\1", cmf)
      fields <- unique(unlist(lapply(fields, function(f) c(f, cmf[pivot_field == f]))))
    }
    list(fields = fields, id_field = id_field)
  })

  # modal logic for info panel
  observeEvent(input$app_info, {
    showModal(
      modalDialog(
        title = "",
        HTML(sprintf(paste0(
          '<div style="display:flex; flex-direction:column; justify-content:flex-start; align-items:center; text-align:center;">\n',
          '<div style="height:4rem; margin:auto; position:relative; pointer-events: none;" id="info_icon"><img src="reflectR-name.png" alt="BOLDreflectR" height="100%%"></div>\n',
          '<div style="font-size:1.2rem; font-weight:bold; margin-bottom:0.2rem;">v%s</div>\n',
          '<div style="font-size:0.85rem;">© 2026 Spencer K. Monckton – MIT License</div>\n',
          '<div style="font-size:0.95rem;">This software was developed at the Centre for Biodiversity Genomics, which is supported by funding from the Government of Canada through Genome Canada and Ontario Genomics (OGI-208), the Ontario Ministry of Colleges and Universities, the Natural Sciences and Engineering Research Council of Canada (NSERC), the New Frontiers in Research Fund (NFRFT-2020-00073), and the Canada Foundation for Innovation (MSI 42450).</div>\n',
          '<div style="margin-bottom:0.8rem; font-weight:500; width:90%%; position:relative;">If you use this software, please cite it as below:<button id="cite-copy-btn"><i class="far fa-copy"></i></button></div>\n',
          '<pre style="text-align:left; width:90%%; word-wrap:normal; text-wrap:auto; font-family: inherit;"><div class="citation">%s</div></pre>\n',
          '<div style="margin-bottom:0.8rem;">For data retrieval, please also cite <span style="font-weight:500;">BOLDconnectR</span> and/or <span style="font-weight:500;">BOLDNODE</span> as applicable:</div>\n',
          '<pre style="text-align:left; width:90%%; word-wrap:normal; text-wrap:auto; font-family: inherit;"><div class="citation" style="margin-bottom:0.8rem;">%s</div><div class="citation">%s</div></pre>\n',
          '<div style="margin-bottom:0.8rem;">Users are also strongly encouraged to cite any data from the BOLD database as follows:</div>\n',
          '<pre style="text-align:left; width:90%%; word-wrap:normal; text-wrap:auto; font-family: inherit;"><div class="citation">%s</div></pre>\n',
          "</div>"
        ), ver, citations[1], citations[2], citations[3], citations[4])),
        tstjs,
        tags$script(HTML(r"(
          document.getElementById('cite-copy-btn').onclick = function() {
            Shiny.setInputValue('cite_clicked', Math.random());
          };
        )")),
        footer = "",
        size = "l",
        easyClose = TRUE
      )
    )
  })

  # modal logic for settings panel
  observeEvent(input$settings, {
    show_settings_modal()
  })
  # (col_set: optional column selection to show instead of the saved default, e.g. after saving a new column set)
  show_settings_modal <- function(col_set = NULL) {
    header_opt <- isolate(user_config$settings$fasta_head)
    avail_opts <- field_choices(intersect(c(header_opt, setdiff(names(config$fasta_ex), c(header_opt, "nuc"))),
                                          unlist(config$fieldsets$bcdm)))
    showModal(
      modalDialog(
        title = "Settings",
        tagList(
          tagAppendChildren(
            # the key itself is never sent to the page so dots stand in to indicate that a password is saved
            if (nzchar(Sys.getenv("api_key"))) {
              htmltools::tagQuery(passwordInput("api_key_def", h3("BOLD API key"), placeholder = strrep("\u25cf", 36)))$
                find("input")$addClass("key-set")$allTags()
            } else {
              passwordInput("api_key_def", h3("BOLD API key"))
            },
            checkboxInput("remember_key_def",
                          div("Remember key",
                              bslib::tooltip(
                                # (clicks on the icon don't tick the checkbox)
                                tags$span(icon("circle-question"), onclick = "event.preventDefault()"),
                                paste0("When 'Remember' is ticked, the API key is saved to your system's credential store (", keystore,
                                       "). Otherwise, it is used until the app is closed, and any saved key is removed.")
                              )),
                          value = !nzchar(Sys.getenv("api_key")) || tryCatch(nzchar(key_get("BOLD.apikey")), error = function(e) FALSE)),
            if (nzchar(Sys.getenv("api_key"))) actionLink("remove_key_def", "Remove stored key")
          ),
          inputGroup(
            title = "FASTA output",
            groupId = "fasta_def",
            selectizeInput("fasta_head_def",
              "Header labels:",
              choices = avail_opts,
              multiple = TRUE,
              options = list(plugins = list("drag_drop", "remove_button")),
              selected = header_opt
            ),
            HTML('<span style="margin-bottom:0.125rem;">Example:</span>'),
            verbatimTextOutput("fasta_head_ex")
          ),
          inputGroup(
            title = "Default options",
            groupId = "customize_defaults",
            selectizeInput("filt_opt_def",
              tagList(
                div("Columns to show:"),
                actionLink(
                  "save_filter_def",
                  icon("bookmark")
                )
              ),
              choices = filter_options(),
              selected = if (is.null(col_set)) try(user_config$settings$col_set, silent = TRUE) else col_set,
              multiple = TRUE,
              options = list(plugins = list("drag_drop", "remove_button"))
            ),
            selectizeInput("filt_seq_def",
              "Filter by marker:",
              choices = list(
                "COI-5P" = "COI-5P",
                "COI-3P" = "COI-3P",
                "28S" = "28S",
                "28S-D2-D3" = "28S-D2-D3",
                "18S" = "18S",
                "18S-5P" = "18S-5P"
              ),
              selected = user_config$settings$filt_seq,
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            ),
            bslib::input_switch(
              "collapse_mrkrs_def",
              "Collapse multiple markers into single rows",
              value = isTRUE(user_config$settings$collapse_mrkrs)
            ),
            bslib::input_switch(
              "include_nts_def",
              div(
                "Show NTS records in fetched data",
                bslib::tooltip(
                  icon("circle-question"),
                  "Shows or hides non-target sequence records within existing results (i.e., NTS records matching initial query terms)."
                )
              ),
              value = isTRUE(user_config$settings$include_nts)
            ),
            numericInput("max_records_def",
              div(
                "Maximum records per search",
                bslib::tooltip(
                  icon("circle-question"),
                  "Searches exceeding this number of matching records – whether from BOLD or a local data package – are not fetched. Very large results can take a long time and several GB of memory (about 2 GB for 150,000 records). Note that BOLD imposes a maximum of 100k results on searches."
                )
              ),
              value = user_config$settings$max_records, min = 1000, step = 50000
            )
          )
          ,
          inputGroup(
            title = "Alignments & trees",
            groupId = "aln_tree_defaults",
            numericInput("max_seqs_def",
              div(
                "Maximum sequences",
                bslib::tooltip(
                  icon("circle-question"),
                  "Largest number of sequences that will be aligned or used to build a tree. This is a safeguard to prevent unintentionally large analyses. Set as appropriate."
                )
              ),
              value = user_config$settings$max_seqs, min = 3, step = 100
            ),
            radioButtons("tree_incomparable_def",
              div(
                "In case of missing distances:",
                bslib::tooltip(
                  icon("circle-question"),
                  "Concerns pairs of sequences with no overlap or that are too divergent to compute distances. Dropping removes the fewest sequences needed. NJ* (ape::njs) keeps them but is much slower (about 90 s for 1,000 sequences); if too many distances are missing, NJ* drops the sequences anyway."
                )
              ),
              choices = c("Drop tips from the tree (fast)" = "drop", "Keep them and use NJ* (slower)" = "njs"),
              selected = user_config$settings$tree_incomparable
            ),
            tree_label_input("tree_labels_def", "Tree tip labels:"),
            numericInput("pdf_tips_def",
              div(
                "Tree PDF tips per page",
                bslib::tooltip(
                  icon("circle-question"),
                  "Between 20 and 100. Labels are 10 pt up to about 60 tips per page, then smaller, down to 6 pt at 100."
                )
              ),
              value = user_config$settings$pdf_tips, min = 20, max = 100, step = 5
            )
          ),
          inputGroup(
            title = "Diversity profiles",
            groupId = "div_defaults",
            numericInput("rare_nboot_def",
              div(
                "Rarefaction bootstrap replicates",
                bslib::tooltip(
                  icon("circle-question"),
                  "Number of bootstrap replicates for confidence intervals on rarefaction curves (0 to 500). More replicates give steadier bands but take longer; 0 skips bootstrapping."
                )
              ),
              value = user_config$settings$rare_nboot, min = 0, max = 500, step = 10
            )
          )
        ),
        footer = tagList(
          div(),
          div(
            actionButton("save_settings_btn", "Save"),
            modalButton("Close")
          )
        ),
        easyClose = TRUE
      )
    )
  }

  # remove the key from this session and from the credential store
  observeEvent(input$remove_key_def, {
    removeModal()
    validate_apikey("", remember = FALSE)
    showNotification("The API key was removed.", type = "message")
  })

  # settings save logic
  observeEvent(input$save_settings_btn, {
    removeModal()
    key <- trimws(gsub('"', "", isolate(input$api_key_def)))
    # an empty box keeps the current key and 'Remember' still applies to it
    if (!nzchar(key)) key <- Sys.getenv("api_key")
    if (nzchar(key)) validate_apikey(key, remember = isTRUE(input$remember_key_def))
    # whole-number settings outside these ranges are left unchanged (with a notice)
    valid_count <- function(x, min) isTruthy(x) && is.numeric(x) && x >= min && x <= .Machine$integer.max
    ignored <- c(if (!valid_count(input$max_seqs_def, 3)) "maximum sequences (at least 3)",
                 if (!valid_count(input$max_records_def, 1000)) "maximum records (at least 1,000)",
                 if (!valid_count(input$rare_nboot_def, 0) || input$rare_nboot_def > 500) "rarefaction bootstrap replicates (0 to 500)")
    if (length(ignored) > 0) {
      showNotification(paste0("Not changed, invalid value: ", paste(ignored, collapse = "; "), "."), type = "warning")
    }
    save_settings <- list(
      fasta_head = isolate(input$fasta_head_def),
      col_set = isolate(input$filt_opt_def),
      filt_seq = isolate(input$filt_seq_def),
      collapse_mrkrs = isolate(input$collapse_mrkrs_def),
      include_nts = isolate(input$include_nts_def),
      max_seqs = if (valid_count(input$max_seqs_def, 3)) as.integer(input$max_seqs_def) else user_config$settings$max_seqs,
      max_records = if (valid_count(input$max_records_def, 1000)) as.integer(input$max_records_def) else user_config$settings$max_records,
      tree_labels = isolate(input$tree_labels_def),
      tree_incomparable = isolate(input$tree_incomparable_def),
      pdf_tips = if (isTruthy(input$pdf_tips_def)) as.integer(min(100, max(20, input$pdf_tips_def))) else user_config$settings$pdf_tips,
      rare_nboot = if (valid_count(input$rare_nboot_def, 0) && input$rare_nboot_def <= 500) as.integer(input$rare_nboot_def) else user_config$settings$rare_nboot
    )
    save_user_config(save_settings)
    updateSelectizeInput(inputId = "fasta_head_def", selected = save_settings$fasta_head)
    updateSelectizeInput(inputId = "filt_opt_def", selected = save_settings$col_set)
    updateSelectizeInput(inputId = "filt_seq_def", selected = save_settings$filt_seq)
    bslib::update_switch(id = "collapse_mrkrs_def", value = save_settings$collapse_mrkrs)
    bslib::update_switch(id = "include_nts_def", value = save_settings$include_nts)
    # new defaults only replace the current selections before any data has been retrieved
    if (is.null(outdata$data)) {
      updateSelectizeInput(session, "tree_labels", selected = save_settings$tree_labels)
      updateSelectizeInput(inputId = "filt_opt", selected = save_settings$col_set)
      updateSelectizeInput(inputId = "filt_seq", selected = save_settings$filt_seq)
      bslib::update_switch(id = "collapse_mrkrs", value = save_settings$collapse_mrkrs)
      bslib::update_switch(id = "include_nts", value = save_settings$include_nts)
    }
  })

  # modal logic for saving custom field sets
  # (records whether the modal was opened from the sidebar or from the settings panel, so the matching selection is saved)
  col_set_source <- reactiveVal("filt_opt")
  # incremented whenever custom field sets change, so that computed_fields() picks up edits to a selected set
  fieldset_version <- reactiveVal(0)
  observeEvent(input$save_filter_set, {
    save_col_set("filt_opt")
  })
  observeEvent(input$save_filter_def, {
    save_col_set("filt_opt_def")
  })
  save_col_set <- function(source = "filt_opt") {
    col_set_source(source)
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
            if (source == "filt_opt_def") actionButton("save_filter_cancel", "Cancel") else modalButton("Cancel")
          )
        ),
        easyClose = TRUE
      )
    )
  }

  # field set save logic
  observeEvent(input$save_filter_btn, {
    from_settings <- col_set_source() == "filt_opt_def"
    set_fields <- if (from_settings) input$filt_opt_def else input$filt_opt
    req(isTruthy(input$save_filter_name), isTruthy(set_fields))
    removeModal()
    filter_set <- modify_custom_fieldset(input$save_filter_name, set_fields)
    fieldset_version(fieldset_version() + 1)
    if (from_settings) {
      # refresh the sidebar options without changing its selection, then return to the settings panel
      updateSelectizeInput(session, "filt_opt",
        choices = filter_options(is.null(fetch_params$search_token$node_result)),
        selected = input$filt_opt
      )
      show_settings_modal(col_set = filter_set)
    } else {
      updateSelectizeInput(session, "filt_opt",
        choices = filter_options(is.null(fetch_params$search_token$node_result)),
        selected = filter_set
      )
    }
  })

  # cancelling from the settings panel returns to it
  observeEvent(input$save_filter_cancel, {
    show_settings_modal()
  })

  # reactively show/hide delete button
  observe({
    shinyjs::toggleElement(id = "del_filter_btn", condition = isTruthy(input$sets_to_delete))
  })

  # field set delete logic
  observeEvent(input$del_filter_btn, {
    deleted <- input$sets_to_delete
    for (s in deleted) {
      filter_set <- modify_custom_fieldset(s, operation = "delete")
    }
    # deleted sets are also removed from the saved default column selection
    if (any(deleted %in% user_config$settings$col_set)) {
      save_user_config(list(col_set = setdiff(user_config$settings$col_set, deleted)))
    }
    fieldset_version(fieldset_version() + 1)
    # choices must remain id values (named by set name) so that further deletions work
    updateSelectInput(
      inputId = "sets_to_delete",
      choices = unlist(lapply(user_config$fieldsets$custom, function(s) setNames(s$id, s$name))),
      selected = character(0)
    )
    updateSelectizeInput(session, "filt_opt",
      choices = filter_options(is.null(fetch_params$search_token$node_result)),
      selected = setdiff(input$filt_opt, deleted)
    )
  })

  # keep marker filters updated based on available data
  # (newly fetched data gets the default marker filter from settings; otherwise the current selection is kept where possible)
  observeEvent(outdata$markers, {
    markers <- outdata$markers
    selected <- if (apply_default_markers) {
      intersect(user_config$settings$filt_seq, markers)
    } else {
      intersect(input$filt_seq, markers)
    }
    if (!is.null(markers)) apply_default_markers <<- FALSE
    updateSelectizeInput(session, "filt_seq", choices = markers, selected = selected)
    # markers available for alignment (the current choice is kept if possible; otherwise COI-5P if present)
    aln_markers <- setdiff(markers, "None")
    for (id in c("aln_marker", "tree_marker")) {
      current <- isolate(input[[id]])
      updateSelectInput(session, id,
        choices = aln_markers,
        selected = if (isTRUE(current %in% aln_markers)) current else if ("COI-5P" %in% aln_markers) "COI-5P" else aln_markers[1]
      )
    }
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
    dt <- if (input$collapse_mrkrs == TRUE) collapsed_data() else outdata$data
    req(dt)
    dt[, grepl("NTS(?:_[A-Za-z]+)?_OF:", associated_specimens, perl = TRUE) |
      grepl("^(?:BIOUG|CBG|ZMUO).+?\\.NTS[0-9]*?$", sampleid, perl = TRUE)]
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

  # clear old table's search and rows as soon as new data arrives
  observeEvent(outdata$data, {
    session$sendCustomMessage("clear_input", "data_table_search")
    session$sendCustomMessage("clear_input", "data_table_rows_all")
  })

  # track the rows shown in the data table, with filters and search (or all filtered rows while table is still loading)
  shown_data <- reactive({
    if (is.null(input$data_table_search)) return(filtered_data())
    rows <- input$data_table_rows_all
    if (length(rows) == 0) filtered_data()[0] else filtered_data()[rows, ]
  })

  # process IDs currently shown in data table (after table filters, marker filter, NTS/BIN member toggles; none if the filters leave no rows)
  # (waits until the table has been drawn: its search input is NULL until then, as are its rows when none match)
  visible_pids <- reactive({
    req(outdata$data, !is.null(input$data_table_search))
    rows <- input$data_table_rows_all
    if (length(rows) == 0) character(0) else unique(filtered_data()[rows, processid])
  })

  # compute summary counts and inject as a new tab
  observeEvent(input$ana_btn, {
    req(nrow(outdata$data) > 0)
    outdata$summary <- NULL
    if (nrow(shown_data()) == 0) {
      showNotification("There are no records in the table.", type = "warning")
      return()
    }
    if (!input$ana_opt %in% c("tax_summary", "projects", "datasets", names(shown_data()))) {
      showNotification("This field isn't in the current table.", type = "warning")
      return()
    }
    withInfProgress(message = "Generating summary...", {
      outdata$summary <- summarize_table(shown_data(), input$ana_opt, computed_fields()$id_field)
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

  # overview tab: counts from fetched data and hit report for query terms (hits exclude additional BIN members)
  binmates_added <- reactive({
    if (isTRUE(fetch_params$binmates_fetched)) outdata$data$processid %in% outdata$binmates else rep(FALSE, nrow(outdata$data))
  })
  term_hits <- reactive({
    req(outdata$data)
    query_term_hits(outdata$data[!binmates_added()], fetch_params$query)
  })

  output$overview <- renderUI({
    req(nrow(outdata$data) > 0)
    s <- overview_counts(outdata$data)
    hits <- term_hits()
    num <- function(x) format(x, big.mark = ",", scientific = FALSE)
    count <- function(label, value, class = NULL) {
      tagList(div(class = c("count-label", class), label), div(class = c("count-value", class), num(value)))
    }
    div(
      class = "overview",
      h3("Data source"),
      p(outdata$query_info$source),
      h3("Search results"),
      div(
        class = "overview-counts",
        count("Specimens:", s$specimens),
        count("Sequences:", s$sequences),
        lapply(seq_len(nrow(s$markers)), function(i) with(s$markers[i], tagList(
          div(class = "count-label marker", paste0(marker, ":")),
          div(class = "count-value marker", num(sequences)),
          div(class = "count-range", sprintf("(%s bp)", if (min_bp == max_bp) num(min_bp) else paste0(num(min_bp), "–", num(max_bp))))
        ))),
        count("Specimens with BINs:", s$specimens_with_bins),
        count("BINs:", s$bins),
        count("Species:", s$species),
        count("Species with BINs:", s$species_with_bins),
        count("Institutions:", s$institutions)
      ),
      if (any(binmates_added())) {
        p(class = "overview-note", sprintf("Includes %s records of additional BIN members.", num(sum(binmates_added()))))
      },
      if (nrow(hits) > 0) tagList(
        h3("Search terms"),
        lapply(unique(hits$search), function(srch) tagList(
          div(class = "hit_rep_heading", paste0(srch, ":")),
          DT::datatable(hits[search == srch, .(term, found, records)],
            rownames = FALSE, colnames = c("Term", "Found", "Records"),
            options = list(
              dom = "ft",
              autoWidth = FALSE,
              paging = FALSE,
              columnDefs = list(
                list(className = "dt-left", width = "20rem", targets = 0),
                list(className = "dt-center", width = "6rem", targets = c(1, 2))
              )
            )
          ) |>
            DT::formatStyle("found", color = DT::styleEqual(c("FOUND", "NOT FOUND"), c("#33691e", "#c62828")), fontWeight = "500")
        ))
      )
    )
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
    tryCatch(compute_div_profile(), error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      showNotification(paste0("The diversity profile could not be computed: ", conditionMessage(e)), type = "error", duration = NULL)
    })
  })
  compute_div_profile <- function() {
    req(outdata$data)
    outdata$div_profile <- NULL
    data <- unique(shown_data(), by = computed_fields()$id_field)
    if (nrow(data) == 0) {
      showNotification("There are no records in the table.", type = "warning")
      return()
    }
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
    input_opts <- div_opts[c(2, 5, 7, 9)]
    # get BIN-based names for BINs without species names
    if (div_opts$taxon_rank == "bin_fallback") {
      unid_bins <- unique(data[, if (all(empty(species))) .SD, by = bin_uri]$bin_uri)
      if (length(unid_bins) > 0) {
        withInfProgress(message = "Assigning fallback BIN-based names...", {
          unid_bin_tax <- BOLDNODE::get_bin_consensus(data[bin_uri %in% unid_bins], min.ids = 1)
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
    data.table::set(data,
      i = which(empty(data[[div_opts$taxon_rank]])),
      j = div_opts$taxon_rank, value = NA_character_
    )
    # if not counting BINs, replace missing BINs with a placeholder (otherwise bold.analyze.diversity drops these records)
    if (div_opts$taxon_rank != "bin_uri") {
      data.table::set(data,
        i = which(empty(data$bin_uri)),
        j = "bin_uri", value = "None"
      )
    }
    # data for site-based analyses needs to be filtered to records with site filled in
    if (div_opts$site_type %in% c("locations", "taxa")) {
      input_opts$assemblage_type <- if (div_opts$site_type == "locations") {
        setNames(isolate(input$div_location_type), div_opts$site_type)
      } else {
        setNames(isolate(input$div_higher_tax), div_opts$site_type)
      }
      div_opts$site_type <- gsub("taxa", "locations", div_opts$site_type)
      div_opts[["location_type"]] <- unname(input_opts$assemblage_type)
      data.table::set(data,
        i = which(empty(data[[div_opts$location_type]])),
        j = div_opts$location_type, value = NA_character_
      )
      div_opts[["bold_df"]] <- data[!empty(get(div_opts$location_type)) & !empty(get(div_opts$taxon_rank))]
      site_label <- get_div_label(div_opts$location_type, capitalize = TRUE)
    } else if (div_opts$site_type == "grids") {
      input_opts$assemblage_type <- c("grid_cells" = "grids")
      input_opts$gridsize <- input$div_grid_size * 1000
      div_opts[["gridsize"]] <- input_opts$gridsize
      div_opts[["bold_df"]] <- data[!empty(coord) & !empty(get(div_opts$taxon_rank))]
      site_label <- "Grid cell"
    } else {
      input_opts$assemblage_type <- c("none" = "none")
    }

    if (input_opts$assemblage_type != "none") {
      # initialize the profile using BOLDconnectR::bold.analyze.diversity()
      # the minimal analysis option is the Preston abundance distribution, which also returns a community matrix
      # the Preston abundance distribution analysis is from vegan::prestondistr()
      withInfProgress(message = "Building community matrix...", {
        div_profile <- do.call(BOLDconnectR::bold.analyze.diversity, div_opts)
        div_profile$input_opts <- input_opts
        div_profile$comm_table <- data.table(Site = rownames(div_profile$comm.matrix), div_profile$comm.matrix)
        names(div_profile$comm_table)[1] <- site_label
        # if Preston plot is not wanted, remove it now
        if (!"preston" %in% input$div_profile) {
          div_profile[["preston.plot"]] <- NULL
        }
        if (div_opts$site_type == "locations") {
          div_profile$site_stats <- data[, .(N = .N, S = uniqueN(get(div_opts$taxon_rank), na.rm = TRUE)), by = eval(unname(input_opts$assemblage_type))]
          data.table::setnames(div_profile$site_stats,
            old = unname(input_opts$assemblage_type),
            new = site_label
          )
        } else if (div_opts$site_type == "grids") {
          div_profile$site_stats <- data.table(
            site = rownames(div_profile$comm.matrix),
            N = rowSums(div_profile$comm.matrix),
            S = rowSums(div_profile$comm.matrix > 0)
          )
          data.table::setnames(div_profile$site_stats,
            old = "site",
            new = site_label
          )
          # (the grid cell map itself is drawn by output$grid_map)
        }
      })
    } else {
      div_profile <- list(input_opts = input_opts)
    }
    # save basic statistics for later use
    div_profile$counts <- list()
    if (input_opts$assemblage_type != "none") {
      div_profile$site_counts <- list(
        N = rowSums(div_profile$comm.matrix),
        S = rowSums(div_profile$comm.matrix > 0)
      )
      div_profile$counts$sites <- uniqueN(data[[site_col]], na.rm = TRUE)
    }
    div_profile$counts$obs <- nrow(data)
    div_profile$counts$taxa <- uniqueN(data[[div_opts$taxon_rank]], na.rm = TRUE)
    # rarefaction & extrapolation using iNEXT
    if ("rarefy" %in% input$div_profile) {
      # bootstrap replicates from Settings (0 means no bands, otherwise iNEXT needs > 1)
      nboot <- user_config$settings$rare_nboot
      inext_se <- nboot > 0
      nboot <- max(2L, nboot)
      withInfProgress(message = "Computing rarefaction...", {
        if (input_opts$presence_absence) {
          # for presence-absence data, perform incidence-based rarefaction for entire dataset
          comm_inc <- iNEXT::as.incfreq(div_profile$comm.matrix)
          inext_res <- iNEXT::iNEXT(
            x = list(Overall = comm_inc),
            datatype = "incidence_freq",
            endpoint = sum(comm_inc) * 2,
            se = inext_se,
            nboot = nboot
          )
        } else {
          # otherwise, perform individual-based rarefaction for entire dataset
          total_counts <- table(data[[div_opts$taxon_rank]])
          total_counts <- setNames(as.numeric(total_counts), names(total_counts))
          inext_res <- iNEXT::iNEXT(
            x = list(Overall = total_counts),
            datatype = "abundance",
            endpoint = sum(total_counts) * 2,
            se = inext_se,
            nboot = nboot
          )
        }
        div_profile$asy_est <- if ("Diversity" %in% names(inext_res$AsyEst)) {
          inext_res$AsyEst[inext_res$AsyEst$Diversity == "Species richness", ]
        } else {
          asy_est <- inext_res$AsyEst["Species Richness", ]
          names(asy_est)[c(4, 5)] <- c("LCL", "UCL")
          asy_est
        }
      })
      # note that this input should be FALSE when input$div_presence is TRUE
      if (input$div_rare_by_site && input$div_site_type != "none" && !input_opts$presence_absence) {
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
              se = inext_se,
              nboot = nboot
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
          data.table::setnames(asy_est, new = c(site_label, "S_est", "LCL", "UCL"))
          div_profile$site_stats <- asy_est[div_profile$site_stats, on = site_label]
        } else {
          inext_res <- list(Overall = inext_res)
        }
      }
      div_profile$inext <- inext_res
    }

    # beta diversity is computed with BAT::beta() via BOLDconnectR
    # time and memory grow with the square of the number of sites, so it is skipped above a limit (div_max_sites in config.yaml)
    n_sites <- if (is.null(div_profile$comm.matrix)) 0 else nrow(div_profile$comm.matrix)
    if ("beta" %in% input$div_profile && n_sites > user_config$settings$div_max_sites) {
      showNotification(sprintf("Beta diversity skipped: %s sites, more than the maximum of %s. Use broader site categories (e.g. countries or grid squares) or filter the table.",
                               format(n_sites, big.mark = ","), format(user_config$settings$div_max_sites, big.mark = ",")),
                       type = "warning", duration = 20)
    } else if ("beta" %in% input$div_profile) {
      withInfProgress(message = sprintf("Computing beta diversity for %s sites...", format(n_sites, big.mark = ",")), {
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
        if (attr(div_profile$total.beta, "Size") < 3) {
          showNotification("Skipping beta diversity plot; need at least 3 sites to run ordination.", type = "warning", duration = 10)
        } else {
          div_profile$nmds.beta <- vegan::monoMDS(beta_mat, k = 2, maxit = 500)
        }
      })
    }
    # Shannon index is computed with vegan::diversity() via BOLDconnectR
    if ("shannon" %in% input$div_profile) {
      withInfProgress(message = paste0(
        "Computing Shannon ",
        ifelse(input_opts$assemblage_type == "none", "index", "indices"),
        "..."
      ), {
        n_sites <- 0
        if (input_opts$assemblage_type != "none") {
          keep <- div_profile$site_counts$S >= 2
          n_dropped <- sum(!keep)
          n_sites <- length(keep) - n_dropped
          msg_text <- if (n_sites == 0) {
            "None of the sites have sufficient species to compute Shannon index (min. 2)."
          } else {
            paste(n_dropped, "site(s) have insufficient species to compute Shannon index (min. 2).")
          }
          showNotification(msg_text, type = "warning", duration = 10)
          if (n_sites > 0) {
            div_profile$shannon_div <- round(BOLDconnectR:::shannon_div_profile(df = div_profile$comm.matrix), 2)
            h_tab <- data.table(site = rownames(div_profile$shannon_div), div_profile$shannon_div)
            data.table::setnames(h_tab, new = c(site_label, "H'"), skip_absent = TRUE)
            div_profile$site_stats <- h_tab[div_profile$site_stats, on = site_label]
          }
        }
        if (n_sites == 0) {
          total_counts <- table(data[[div_opts$taxon_rank]])
          total_counts <- setNames(as.numeric(total_counts), names(total_counts))
          div_profile$shannon_div <- round(BOLDconnectR:::shannon_div_profile(df = total_counts), 2)
        }
      })
    }
    # complete the site_stats table and save the div_profile object
    if (input_opts$assemblage_type != "none") {
      data.table::setorder(div_profile$site_stats, na.last = TRUE)
      cols <- c(names(div_profile$site_stats)[1], "N", "S", "S_est", "LCL", "UCL", "H'", "mean_beta")
      div_profile$site_stats <- div_profile$site_stats[, .SD, .SDcols = intersect(cols, names(div_profile$site_stats))]
      if (names(input_opts$assemblage_type) == "taxa") {
        div_profile$comm.matrix <- NULL
        div_profile$comm_table <- NULL
      }
    }
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
  }

  # show a modal for saving diversity analysis results
  observeEvent(input$div_dl_btn, {
    req(outdata$div_profile)
    div <- outdata$div_profile
    showModal(
      modalDialog(
        title = "Export diversity profile results",
        div(
          class = "div_dl",
          HTML('<span style="font-weight: 500; font-size:1.1rem;">Full results as ZIP archive:</span>'),
          downloadButton("all_zip", "ZIP"),
          h3("Results by site:"),
          "Summary table", downloadButton("sum_table", "CSV"),
          if ("comm_table" %in% names(div)) {
            tagList(
              "Community matrix", downloadButton("comm_mat", "CSV"),
            )
          },
          if ("shannon_div" %in% names(div)) {
            tagList(
              "Shannon indices", downloadButton("shannon", "CSV")
            )
          },
          if ("total.beta" %in% names(div)) {
            tagList(
              "Pairwise beta distance matrix", downloadButton("beta_dists", "CSV")
            )
          },
          if ("preston.res" %in% names(div)) {
            tagList(
              h3("Preston abundance distribution:"),
              "Analysis results (R object)", downloadButton("preston_rds", "RDS"),
              "Frequencies", downloadButton("preston_csv", "CSV")
            )
          },
          if ("inext" %in% names(div)) {
            tagList(
              h3(paste0(ifelse(div$input_opts$presence_absence, "Incidence", "Individual"), "-based rarefaction & extrapolation:")),
              "iNEXT analysis results (R object)", downloadButton("inext_rds", "RDS"),
              "Size-based sampling curves", downloadButton("inext_size", "CSV"),
              "Coverage-based sampling curves", downloadButton("inext_cov", "CSV"),
              "Asymptotic richness estimates", downloadButton("inext_asy", "CSV")
            )
          },
          if ("nmds.beta" %in% names(div)) {
            tagList(
              h3("Beta diversity ordination:"),
              "NMDS analysis results (R object)", downloadButton("nmds_rds", "RDS"),
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
    # outdata$bin_consensus <- NULL
    data <- shown_data()[!empty(bin_uri)]
    # BOLDNODE's BIN consensus fails on an empty table
    if (nrow(data) == 0) {
      showNotification("There are no records with a BIN in the table.", type = "warning")
      return()
    }
    bin_consensus <- NULL
    withInfProgress(message = "Computing consensus BIN taxonomy...", {
      tryCatch(
        {
          bin_consensus <- BOLDNODE::get_bin_consensus(data,
            threshold = as.double(unlist(unname(input$bc_threshold))),
            min.ids = unlist(unname(input$bc_minids)),
            enforce.scientific = input$bc_enforcesci,
            discord.format = "text"
          )
          col_order <- append(names(bin_consensus), c("min_rank", "max_rank"), after = 2)
          # highest and lowest identification rank within each BIN (positions in 'ranks' run from phylum to subspecies)
          rank_pos <- data[, .(
            bin_uri,
            pos = match(as.character(identification_rank), ranks),
            keep = if (input$bc_enforcesci) !grepl(re_int, identification, perl = TRUE) else TRUE
          )][keep == TRUE & !is.na(pos)]
          bin_ranks <- rank_pos[, .(
            min_rank = factor(ranks[min(pos)], levels = ranks),
            max_rank = factor(ranks[max(pos)], levels = ranks)
          ), by = "bin_uri"]
          bin_consensus <- merge(bin_consensus,
            bin_ranks,
            by = "bin_uri",
            all.x = TRUE
          )
          setcolorder(bin_consensus, col_order)
        },
        error = function(e) {
          bin_consensus <<- NULL
          showNotification(paste0("Error computing the BIN consensus: ", conditionMessage(e)), type = "error")
        }
      )
    })
    req(bin_consensus)

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
      cons <- isolate(outdata$bin_consensus)
      # portal stats are only merged once per consensus table (this observer fires again after the merge below);
      # avg_dist is used to detect this because it only comes from the portal
      if (isTRUE(input$bc_portal_stats) && !"avg_dist" %in% names(cons)) {
        # reuse stats already retrieved for these BINs, and only query the portal for the rest
        portal_stats <- isolate(outdata$bin_portal_stats)
        missing_bins <- setdiff(as.character(cons$bin_uri), as.character(portal_stats$bin_uri))
        if (length(missing_bins) > 0) {
          new_stats <- get_portal_bin_stats(missing_bins, shiny = TRUE)
          if (is.data.frame(new_stats)) {
            portal_stats <- unique(rbindlist(list(portal_stats, new_stats), fill = TRUE), by = "bin_uri")
          }
        }
        if (is.data.frame(portal_stats) && "bin_uri" %in% names(portal_stats)) {
          outdata$bin_portal_stats <- portal_stats
          outdata$bin_consensus <- merge(cons,
            portal_stats,
            by = "bin_uri",
            all.x = TRUE
          )
          bslib::nav_select(id = "tabs", selected = "consensus_tab")
        } else {
          showNotification("BOLD Portal stats could not be retrieved.", type = "warning")
        }
      }
    })
  })

  # the portal API can be somewhat inconsistent; this will insert a warning message about any BINs for which the query timed out
  insert_portal_warning <- function(portal_stats) {
    removeUI("#bc_portal_warning", immediate = TRUE)
    failed <- nrow(portal_stats[empty(bold_member_count)])
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
    if ("bold_member_count" %in% names(portal_stats)) {
      insert_portal_warning(portal_stats)
    }
  })

  # logic for retrying portal BIN query
  observeEvent(input$bc_portal_retry, {
    init_stats <- outdata$bin_portal_stats
    req(init_stats)
    retry_stats <- get_portal_bin_stats(as.character(init_stats[empty(bold_member_count), bin_uri]), shiny = TRUE)
    if (!is.data.frame(retry_stats) || nrow(retry_stats) == 0) {
      showNotification("BOLD Portal stats could not be retrieved.", type = "warning")
      return()
    }
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
            input$bin_rep_seq_opt
          ),
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
          outdata$bin_reps <- BOLDNODE::get_bin_reps(
            convert_factor_char(outdata$data[processid %in% visible_pids()],
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
          showNotification(paste0("Error selecting BIN representatives: ", e$message), type = "error")
        }
      )
    })
    # don't open (or jump to) the tab if no representatives could be selected
    req(outdata$bin_reps)
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

  ### Sequence alignment & neighbour-joining tree ----

  # incremented for each new alignment and tree: matches viewer edits with corresponding alignment; tree viewer reloads only for a new tree
  aln_version <- 0
  tree_version <- 0

  # sequences to align, i.e. filtered records that have a sequence for the selected marker (prefixed "aln" or "tree" - see alignment_marker_input() in global.R)
  # or with bin_reps = TRUE, BIN rep records instead
  # NULL if too few or too many
  get_alignment_input <- function(prefix = "aln", bin_reps = FALSE) {
    marker <- input[[paste0(prefix, "_marker")]]
    req(isTruthy(marker))
    pids <- visible_pids()
    header <- user_config$settings$fasta_head
    records <- if (bin_reps) outdata$data[processid %in% bin_rep_pids(marker)] else outdata$data[processid %in% pids]
    inp <- alignment_input(records, marker, header)
    n <- length(inp$seqs)
    if (n < 2) {
      showNotification(paste0("At least two ", marker, " sequences are needed (found ", n,
                              if (bin_reps) " from BIN representatives)." else " in filtered records)."), type = "error")
      return(NULL)
    }
    if (n > user_config$settings$max_seqs) {
      showNotification(sprintf("There are %s %s sequences in the filtered data, which may take a long time to align (maximum is set to %s). Either filter the table further, or raise the maximum in Settings to bypass this safeguard.",
                               format(n, big.mark = ","), marker, format(user_config$settings$max_seqs, big.mark = ",")), type = "error", duration = 10)
      return(NULL)
    }
    # check if records for alignment are a subset of the fetched data (table filters, marker filter, NTS/BIN member toggles)
    c(inp, marker = marker, filtered = length(pids) < uniqueN(outdata$data$processid), bin_reps = bin_reps)
  }

  # process IDs of BIN reps that have a sequence for the marker, as filtered in the BIN reps tab
  bin_rep_pids <- function(marker) {
    reps <- outdata$data[record_id %in% outdata$bin_reps]
    if (!is.null(input$binrep_table_search)) reps <- reps[if (is.null(input$binrep_table_rows_all)) 0 else input$binrep_table_rows_all]
    reps[as.character(marker_code) == marker & !empty(nuc), unique(processid)]
  }

  # subset of visible_pids() that have a sequence for the selected marker (before gap-only and ambiguous-only sequences are dropped; though in practice those should not occur)
  marker_pids <- function(marker) {
    pids <- visible_pids()  # (waits for data)
    outdata$data[processid %in% pids & as.character(marker_code) == marker & !empty(nuc), unique(processid)]
  }

  # align sequences from get_alignment_input() using the chosen method
  run_alignment <- function(inp, prefix = "aln") {
    method <- input[[paste0(prefix, "_method")]]
    gcode <- input[[paste0(prefix, "_gcode")]]
    if (method == "none") {
      lengths <- range(nchar(inp$raw))
      if (lengths[1] != lengths[2]) {
        showNotification(sprintf("NOTE: Input sequences have different lengths (%s to %s); gaps have been added to the ends.",
                                 lengths[1], lengths[2]), type = "warning", duration = 10)
      }
    }
    aligned <- withInfProgress(sprintf("%s %s %s sequences...", if (method == "none") "Preparing" else "Aligning",
                                       format(length(inp$seqs), big.mark = ","), inp$marker), {
      tryCatch(align_sequences(if (method == "none") inp$raw else inp$seqs, inp$labels, method = method, gcode = gcode),
               error = function(e) {
                 showNotification(paste0("Alignment failed: ", conditionMessage(e)), type = "error")
                 NULL
               })
    })
    req(aligned)
    aln_version <<- aln_version + 1
    list(marker = inp$marker, ids = inp$ids, labels = inp$labels, seqs = unname(aligned), version = aln_version,
         method = alignment_method_text(method, gcode), filtered = inp$filtered, bin_reps = isTRUE(inp$bin_reps),
         source_pids = marker_pids(inp$marker))
  }

  # update suggested translation table for AA alignment using suggest_genetic_code() when selected data calls for it
  suggested_gcode <- list(aln = NULL, tree = NULL)
  for (prefix in c("aln", "tree")) local({
    prefix <- prefix
    observe({
      marker <- input[[paste0(prefix, "_marker")]]
      req(isTruthy(marker))
      pids <- visible_pids()  # (waits for data)
      taxa_cols <- intersect(c("kingdom", "phylum", "class"), names(outdata$data))
      d <- outdata$data[processid %in% pids & as.character(marker_code) == marker, ..taxa_cols]
      code <- suggest_genetic_code(marker, d)
      if (!identical(code, suggested_gcode[[prefix]])) {
        suggested_gcode[[prefix]] <<- code
        updateSelectInput(session, paste0(prefix, "_gcode"), selected = code)
      }
    })
  })

  # insert/update the alignment tab
  show_alignment <- function(aln) {
    outdata$alignment <- aln
    if (!tab_status$aln_tab) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "aln_tab"), position = "after", select = TRUE,
        bslib::nav_panel(value = "aln_tab", "Alignment", div(id = "aln_viewer"))
      )
      tab_status$aln_tab <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "aln_tab")
    }
    session$sendCustomMessage("aln_load", list(id = "aln_viewer", names = aln$labels, seqs = aln$seqs,
                                               marker = aln$marker, version = aln$version))
  }

  observeEvent(input$aln_btn, {
    inp <- get_alignment_input()
    req(inp)
    show_alignment(run_alignment(inp))
  })

  # copy notifications from alignment viewer
  observeEvent(input$aln_notify, {
    showNotification(input$aln_notify, id = "copy_msg", type = "message")
  })

  # copy requests from the viewers
  observeEvent(input$viewer_copy, {
    text <- input$viewer_copy$text
    req(isTruthy(text))
    cb(strsplit(text, "\n", fixed = TRUE)[[1]], header = FALSE)
  })

  # build a neighbour-joining tree and show it in the tree tab
  # if an alignment has to be computed first, it is sent to the page before the tree is built (because Shiny only sends updates once an observer has finished)
  tree_request <- reactiveVal(NULL)
  observeEvent(input$tree_btn, {
    aln <- outdata$alignment
    bin_reps <- isTRUE(input$tree_bin_reps) && length(outdata$bin_reps) > 0
    use_aln <- isTRUE(input$tree_from_aln) && !is.null(aln)
    # use rows of BIN reps from current alignment (pairwise distances are the same)
    reps <- if (bin_reps && use_aln) bin_rep_pids(aln$marker)
    if (use_aln && bin_reps && length(reps) < 3) {
      showNotification(sprintf("At least three sequences are needed to build a tree (found %s).", length(reps)),
                       type = "error")
      req(FALSE)
    }
    if (use_aln && bin_reps && !all(reps %in% aln$ids)) {
      showNotification(sprintf("The existing alignment does not include all %s BIN representatives. They have been aligned on their own.",
                               format(length(reps), big.mark = ",")), type = "warning", duration = 10)
      use_aln <- FALSE
    }
    if (use_aln) {
      tree_request(list(aln = aln, from_aln = TRUE, reps = reps, requested = Sys.time()))
    } else {
      # tree alignment options (not shown in alignment tab)
      inp <- get_alignment_input("tree", bin_reps = bin_reps)
      req(inp)
      aln <- run_alignment(inp, "tree")
      session$onFlushed(function() tree_request(list(aln = aln, requested = Sys.time())), once = TRUE)
    }
  })

  # option to build a BIN rep tree shown when BIN reps available
  observeEvent(outdata$bin_reps, {
    has_reps <- length(outdata$bin_reps) > 0
    shinyjs::toggleElement("tree_bin_reps_wrap", condition = has_reps)
    if (!has_reps) updateCheckboxInput(session, "tree_bin_reps", value = FALSE)
  }, ignoreNULL = FALSE)

  # "Use current alignment" is only selectable when an alignment is available (and is checked when a new alignment is made)
  observeEvent(outdata$alignment, {
    has_aln <- !is.null(outdata$alignment)
    updateCheckboxInput(session, "tree_from_aln", value = has_aln)
    shinyjs::toggleState("tree_from_aln", condition = has_aln)
  }, ignoreNULL = FALSE)

  # while checked, tree marker is updated to match alignment and disabled
  # (low priority observer, so it runs after the marker list is updated for new data)
  observe({
    outdata$markers
    aln <- outdata$alignment
    use_aln <- isTRUE(input$tree_from_aln) && !is.null(aln)
    if (use_aln) updateSelectInput(session, "tree_marker", selected = aln$marker)
    shinyjs::toggleState("tree_marker", condition = !use_aln)
  }, priority = -1)

  # option is unchecked if the filtered records change relative to those used in the alignment (hopefully reduces user error)
  observeEvent(visible_pids(), {
    aln <- outdata$alignment
    req(aln, isTRUE(input$tree_from_aln))
    if (!setequal(marker_pids(aln$marker), aln$source_pids)) {
      updateCheckboxInput(session, "tree_from_aln", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(tree_request(), {
    aln <- tree_request()$aln
    seqs <- aln$seqs
    manually_edited <- FALSE
    # manual edits from the alignment viewer are incorporated if applicable
    edited <- input$aln_edited
    if (isTRUE(tree_request()$from_aln) && identical(as.numeric(edited$version), as.numeric(aln$version)) && length(edited$seqs) == length(seqs)) {
      seqs <- unlist(edited$seqs)
      manually_edited <- isTRUE(edited$edits > 0)
    }
    # BIN rep rows from the current alignment
    reps <- tree_request()$reps
    full_n <- length(seqs)
    if (!is.null(reps)) {
      keep <- aln$ids %in% reps
      seqs <- seqs[keep]
      aln$ids <- aln$ids[keep]
      aln$labels <- aln$labels[keep]
      aln$bin_reps <- TRUE
    }
    if (length(seqs) < 3) {
      showNotification("At least three sequences are needed to build a tree.", type = "error")
      req(FALSE)
    }
    tree <- withInfProgress(sprintf("Building neighbour-joining tree from %s sequences%s...", format(length(seqs), big.mark = ","),
                               if (isTRUE(input$tree_collapse)) sprintf(" (%s unique)", format(length(unique(seqs)), big.mark = ",")) else ""), {
      tryCatch(build_nj_tree(seqs, aln$labels, model = input$tree_model, midpoint = isTRUE(input$tree_midpoint),
                             collapse_identical = isTRUE(input$tree_collapse), incomparable = user_config$settings$tree_incomparable),
               error = function(e) {
                 showNotification(paste0("Tree building failed: ", conditionMessage(e)), type = "error")
                 NULL
               })
    })
    req(tree)
    # sequences dropped due to missing distances
    dropped <- attr(tree, "dropped")
    if (dropped > 0) {
      showNotification(if (attr(tree, "njs_failed")) {
        sprintf("%s sequence(s) were dropped: too many distances were missing for NJ* to build a tree.", format(dropped, big.mark = ","))
      } else {
        sprintf("%s sequence(s) were dropped: no overlap or too divergent from others in the alignment. This behaviour can be adjusted in Settings > Alignments & trees.",
                format(dropped, big.mark = ","))
      }, type = "warning", duration = 15)
    }
    if (ape::Ntip(tree) < 3) {
      showNotification("Fewer than three sequences are left; cannot build a tree.", type = "error")
      req(FALSE)
    }
    # record data used for tooltips, tip labels and tree-order table
    ids <- aln$ids[match(tree$tip.label, aln$labels)]
    meta <- outdata$data[processid %in% ids]
    meta <- unique(meta[order(as.character(marker_code) != aln$marker)], by = "processid")[match(ids, processid)]
    # details for the PDF cover page
    model_names <- c(K80 = "Kimura 2-parameter (K2P)", raw = "p-distance", TN93 = "Tamura-Nei (TN93)")
    details <- c(
      "Query" = if (is.null(outdata$query_info)) "-" else outdata$query_info$summary,
      "Additional filters applied" = if (isTRUE(aln$filtered)) "Yes" else "No",
      "Sequences analysed" = paste0(format(ape::Ntip(tree), big.mark = ","), if (isTRUE(aln$bin_reps)) " (BIN representatives)"),
      if (dropped > 0) c("Sequences dropped (not comparable)" = format(dropped, big.mark = ",")),
      "Data type" = "Nucleotide",
      "Marker" = aln$marker,
      "Alignment method" = paste0(aln$method, if (manually_edited) "; manually edited" else "",
                                  if (!is.null(reps)) sprintf("; taken from an alignment of %s sequences", format(full_n, big.mark = ","))),
      "Distance model" = model_names[[input$tree_model]],
      "Tree method" = paste0(if (attr(tree, "njs")) "Neighbour-joining with missing distances (NJ*, ape::njs; ape " else "Neighbour-joining (ape ",
                             packageVersion("ape"), ")",
                             if (isTRUE(input$tree_midpoint)) ", midpoint-rooted" else ", unrooted",
                             if (isTRUE(input$tree_collapse)) paste0("; identical sequences collapsed (", format(attr(tree, "unique"), big.mark = ","), " unique)") else "")
    )
    tree_version <<- tree_version + 1
    outdata$tree <- list(tree = tree, marker = aln$marker, details = details, meta = meta,
                         tip_number = tip_numbers(tree), version = tree_version)
    if (!tab_status$tree_tab) {
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "tree_tab"), position = "after", select = TRUE,
        bslib::nav_panel(
          value = "tree_tab",
          "Tree",
          div(id = "tree_viewer")
        )
      )
      tab_status$tree_tab <- TRUE
    } else {
      bslib::nav_select(id = "tabs", selected = "tree_tab")
    }
  })

  # tip labels and colours for current tree, shared by the viewer, PDF, Newick file, and tree-order table
  tree_view <- reactive({
    tr <- outdata$tree
    req(tr)
    fields <- if (length(input$tree_labels) == 0) config$settings$tree_labels else input$tree_labels
    colour <- input$tree_colour
    colours <- if (isTruthy(colour) && colour != "none") {
      tree_colours(tr$tree, tip_field_values(tr$meta, colour, tr$tip_number), tr$tip_number)
    }
    list(tree = tr, labels = tree_tip_labels(tr$meta, fields, tr$tip_number), info = tree_tip_info(tr$meta, fields, tr$tip_number),
         col = colours$node,
         values = colours$values, fields = fields, colour = if (is.null(colours)) "none" else colour)
  })

  # a new tree is loaded into the viewer; changes to tip labels or colours only update it (no rebuild, zoom & scroll position retained)
  tree_sent <- 0
  observe({
    v <- tree_view()
    tr <- v$tree
    if (tr$version != tree_sent) {
      tree_sent <<- tr$version
      session$sendCustomMessage("tree_load", list(id = "tree_viewer", tree = tree_layout(tr$tree, v$info, v$labels, v$col)))
    } else {
      session$sendCustomMessage("tree_update", list(id = "tree_viewer", label = v$labels, info = v$info,
                                                    col = if (is.null(v$col)) "" else v$col))
    }
  })

  # record data in tree order for the TSV/CSV/XLSX buttons on the tree tab, sorted by tip number
  # includes tip number (as tree_order), process ID, sample ID, BIN, taxonomy, identification details, then any other fields chosen as tip labels
  tree_table <- reactive({
    v <- tree_view()
    meta <- v$tree$meta
    base <- c("processid", "sampleid", "bin_uri", "kingdom", ranks, "identification", "identification_rank",
              "identified_by", "identification_method", "taxonomy_notes")
    cols <- intersect(unique(c(base, setdiff(v$fields, "tip_number"))), names(meta))
    out <- data.table(tree_order = v$tree$tip_number, meta[, ..cols])
    out[order(tree_order)]
  })

  # tip label options: fields with at least one value in the data
  observeEvent(outdata$data, {
    filled <- names(outdata$data)[vapply(outdata$data, function(x) any(!empty(x)), logical(1))]
    updateSelectizeInput(session, "tree_labels", choices = tree_label_fields(filled),
                         selected = intersect(isolate(input$tree_labels), c("tip_number", filled)))
  })

  # options to colourize tree follow the chosen label fields (except tip number)
  observeEvent(input$tree_labels, {
    fields <- setdiff(input$tree_labels, "tip_number")
    current <- isolate(input$tree_colour)
    updateSelectInput(session, "tree_colour",
                      choices = c("None" = "none", stats::setNames(fields, tree_label_names[fields])),
                      selected = if (isTRUE(current %in% fields)) current else "none")
  }, ignoreNULL = FALSE)

  # tree PDF is created here, then download is triggered through the hidden tree_pdf_dl link
  tree_pdf_file <- reactiveVal(NULL)
  observeEvent(input$tree_pdf, {
    v <- tree_view()
    tr <- v$tree
    file <- tempfile(fileext = ".pdf")
    label_text <- c(
      "Tip labels" = paste(tree_label_names[v$fields], collapse = " | "),
      "Tip colours" = if (v$colour == "none") "None" else
        sprintf("By %s (%s values%s)", tree_label_names[[v$colour]], format(length(v$values), big.mark = ","),
                if (length(v$values) > length(tree_palette)) sprintf("; colours reused after %d", length(tree_palette)) else "")
    )
    ok <- withProgress(message = "Creating tree PDF...", value = 0, {
      tryCatch({
        write_tree_pdf(tr$tree, file, title = paste(tr$marker, "neighbour-joining tree"),
                       cover = c("Date" = format(Sys.Date()), tr$details, label_text),
                       labels = v$labels, col = v$col, tips_per_page = user_config$settings$pdf_tips,
                       progress = function(page, pages) setProgress(page / pages, detail = sprintf("page %d of %d", page, pages)))
        TRUE
      }, error = function(e) {
        showNotification(paste0("The PDF could not be created: ", conditionMessage(e)), type = "error")
        FALSE
      })
    })
    req(ok)
    tree_pdf_file(file)
    shinyjs::runjs("document.getElementById('tree_pdf_dl').click();")
  })

  output$tree_pdf_dl <- downloadHandler(
    filename = function() paste0("NJ_tree_", outdata$tree$marker, "_", format(Sys.Date(), "%Y%m%d"), ".pdf"),
    content = function(file) {
      file.copy(tree_pdf_file(), file)
    }
  )
  # keep PDF download link active when hidden, so the above JS click works
  outputOptions(output, "tree_pdf_dl", suspendWhenHidden = FALSE)

  output$tree_newick <- downloadHandler(
    filename = function() paste0("NJ_tree_", outdata$tree$marker, "_", format(Sys.Date(), "%Y%m%d"), ".nwk"),
    content = function(file) {
      v <- tree_view()
      writeLines(enc2utf8(tree_newick(v$tree$tree, v$labels)), file, useBytes = TRUE)
    }
  )

  observeEvent(input$reset_tbl, {
    session$sendCustomMessage("reset-dt", "data_table")
  })

  # insert map tab when button is pressed (or jump to existing tab)
  observeEvent(input$map_btn, {
    req(nrow(outdata$data) > 0)
    if (!tab_status$map_tab) {
      # a new map widget will be created, so any previously plotted records need to be redrawn (starting in clustered state)
      map_pids <<- NULL
      shinyjs::runjs("Shiny.setInputValue('map_clustered', true);")
      bslib::nav_insert(
        "tabs",
        target = tab_monitor("ins_target", "map_tab"), position = "after", select = TRUE,
        bslib::nav_panel(
          id = "map_tab",
          value = "map_tab",
          "Occurrence map",
          div(
            id = "map_area",
            leaflet::leafletOutput("record_map")
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
  observeEvent(list(input$map_btn, tab_status$map_tab, input$tabs, input$data_table_rows_all, filtered_data(), input$map_clustered), {
    req(tab_status$map_tab, input$tabs == "map_tab")
    # the map may have been resized while its tab was hidden
    session$sendCustomMessage("resize_map", "#record_map")
    update_map()
  })

  # process IDs & clustering mode currently plotted on the occurrence map (used to skip redrawing when nothing has changed)
  map_pids <- NULL

  # function used to update the rendered markers on the occurrence point map
  update_map <- function() {
    data <- shown_data()
    data <- unique(data[!is.na(lat) & !is.na(lon)], by = "processid")
    # points are clustered unless switched off with the map's cluster/points button
    clustered <- !isFALSE(input$map_clustered)
    # switching back to the map tab does not require redrawing the same records
    if (identical(list(data$processid, clustered), map_pids)) {
      return(invisible())
    }
    map_pids <<- list(data$processid, clustered)
    if (nrow(data) == 0) {
      leaflet::leafletProxy("record_map") |>
        leaflet::clearMarkers() |>
        leaflet::clearMarkerClusters()
      showNotification("None of the filtered records have valid coordinates.", id = "map_msg", type = "warning")
      return(invisible())
    }
    # record values are escaped, since they are inserted into the popups as HTML
    esc <- function(x) htmltools::htmlEscape(as.character(x))
    pal <- leaflet::colorFactor(
      palette = map_colours,
      domain = data$identification
    )
    leaflet::leafletProxy("record_map") |>
      leaflet::clearMarkers() |>
      leaflet::clearMarkerClusters() |>
      leaflet::fitBounds(
        lng1 = min(data$lon),
        lat1 = min(data$lat),
        lng2 = max(data$lon),
        lat2 = max(data$lat)
      ) |>
      leaflet::addCircleMarkers(
        lat = data$lat,
        lng = data$lon,
        color = "#444",
        fillColor = pal(data$identification),
        weight = 2,
        radius = 9,
        opacity = 0.3,
        fillOpacity = 0.7,
        popup = do.call(paste0, c(
          list('<span class="tag">', field_label("sampleid"), ':</span><span class="value"><a href="https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSpecimen?selectedrecordid=', utils::URLencode(as.character(data$processid), reserved = TRUE), '" target="_blank">', esc(data$sampleid), "</a></span>"),
          lapply(c("processid", "identification", "country.ocean", "province.state", "region", "sector", "site"), function(f) {
            paste0('<span class="tag">', field_label(f), ':</span><span class="value">', esc(data[[f]]), "</span>")
          })
        )),
        clusterOptions = if (clustered) leaflet::markerClusterOptions() else NULL
      )
  }

  # reactively subset the filtered data table based on selected fields
  out_table <- reactive({
    data <- filtered_data()
    data[, .SD, .SDcols = intersect(computed_fields()$fields, names(data))]
  })

  # BIN reps are also updated reactively, using the same filters and field selections as the full data table
  rep_table <- reactive({
    req(outdata$data, outdata$bin_reps)
    outdata$data[record_id %in% outdata$bin_reps,
      .SD,
      .SDcols = intersect(computed_fields()$fields, names(outdata$data))
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
  DT_extensions <- c("FixedHeader", "FixedColumns")
  DT_column_js <- reactiveVal(column_js)
  DT_options <- reactive({
    list(
      headerCallback = header_js,
      columnDefs = list(
        list(
          targets = "_all",
          render = DT_column_js()
        )
      ),
      scrollCollapse = TRUE,
      lengthMenu = list(c(100, 500, 1000, 2000), c("100", "500", "1000", "2000")),
      # fixedHeader = TRUE,
      fixedColumns = list(leftColumns = 1),
      stateSave = TRUE,
      stateDuration = -1,
      searchDelay = 500,
      initComplete = DT::JS("function(settings, json) {
                                            var dt = this.api();
                                            $(document).off('shown.bs.tab.dt').on('shown.bs.tab.dt', function() {
                                              dt.columns.adjust();
                                            });
                                          }")
    )
  })

  # generate example FASTA sequence for display
  output$fasta_head_ex <- renderPrint({
    rec <- config$fasta_ex
    ex_txt <- paste0(
      ">", paste0(sapply(input$fasta_head_def, function(x) rec[[x]]), collapse = "|"), "\n",
      rec$nuc
    )
    cat(ex_txt)
  })

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
      options = DT_options()
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
      options = DT_options()
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
      if (!is.null(res$counts$sites)) chip(res$counts$sites, get_div_label(unname(outdata$div_profile$input_opts$assemblage_type), plural = TRUE)),
      chip(res$counts$obs, "observations"),
      chip(res$counts$taxa, get_div_label(outdata$div_profile$input_opts$taxon_rank, plural = TRUE)),
      if (!is.null(res$inext)) {
        bslib::tooltip(
          chip(asy_est, HTML("S<sub>est</sub>")),
          paste0("95% CI: ", round(res$asy_est$LCL, 1), "\u2013", round(res$asy_est$UCL, 1))
        )
      },
      if (!is.null(res$shannon_div)) chip(shannon_h, paste0(ifelse(length(as.numeric(unlist(res$shannon_div))) > 1, "mean", ""), "H′")),
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
        bslib::nav_panel("Rarefaction curve", value = "rarefy_panel", plotly::plotlyOutput("plot_rare"))
      },
      if (!is.null(res$preston.plot)) {
        bslib::nav_panel("Preston plot", value = "preston_panel", plotly::plotlyOutput("plot_preston"))
      },
      if (isTRUE(attr(res$total.beta, "Size") >= 3)) {
        bslib::nav_panel("Beta diversity plot",
          value = "beta_panel",
          plotly::plotlyOutput("plot_beta")
        )
      },
      if (res$input_opts$assemblage_type == "grids") {
        bslib::nav_panel("Grid cells",
          value = "grid_map",
          leaflet::leafletOutput("grid_map")
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
            withSpinner(color = "#aaaaaa", type = 5, size = 0.6)
        )
      },
      bslib::nav_spacer(),
      bslib::nav_item(
        actionButton("div_dl_btn", icon("download"))
      )
    )
  })


  # the grid map is created while its panel is hidden, so it needs to be resized once shown
  # (zoom stability is set via mapOptions() in output$grid_map; mapOptions() has no effect on a leafletProxy)
  observeEvent(input$div_nav, {
    if (input$div_nav == "grid_map") {
      session$sendCustomMessage("resize_map", "#grid_map")
    }
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
      filter = "none",
      rownames = FALSE,
      selection = "none",
      extensions = DT_extensions,
      callback = callback_js,
      options = c(DT_options(), list(paging = FALSE, searching = FALSE))
    ) |>
      DT::formatRound(columns = intersect(c("S_est", "LCL", "UCL"), names(dt)), digits = 1) |>
      DT::formatRound(columns = intersect("H′", names(dt)), digits = 2) |>
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
      options = c(isolate(DT_options()), list(paging = FALSE, searching = FALSE))
    ),
    server = TRUE
  )

  # grid cell map
  output$grid_map <- leaflet::renderLeaflet({
    req(outdata$div_profile$grids.data)
    res <- outdata$div_profile
    grids_wgs84 <- sf::st_transform(res$grids.data, crs = 4326)
    grids_wgs84 <- merge(grids_wgs84, res$site_stats[, .(cell.id = `Grid cell`, N, S)])
    grids_wgs84$label <- lapply(paste0(
      '<span style="font-weight: 600;">', grids_wgs84$cell.id, "</span></br>",
      "N = ", grids_wgs84$N, "; S = ", grids_wgs84$S
    ), HTML)
    leaflet::leaflet(grids_wgs84) |>
      leaflet::mapOptions(zoomToLimits = "first") |>
      leaflet::addTiles() |>
      leaflet::addPolygons(
        fillColor = "#3A7D44",
        fillOpacity = 0.3,
        color = "#1C2A1E",
        weight = 1,
        label = ~label,
        highlightOptions = leaflet::highlightOptions(
          fillOpacity = 0.6,
          weight = 2,
          bringToFront = TRUE
        )
      )
  })

  # rarefaction curves
  output$plot_rare <- plotly::renderPlotly({
    res <- outdata$div_profile
    indiv <- !res$input_opts$presence_absence
    inext_res <- res$inext
    req(inext_res)
    df_inext <- if (!isolate(input$div_rare_by_site)) {
      inext_res$iNextEst$size_based
    } else {
      rbindlist(lapply(inext_res, function(x) x$iNextEst$size_based))
    }
    if (!indiv) names(df_inext)[names(df_inext) == "t"] <- "m"
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
      if (indiv) paste0("<b>", htmltools::htmlEscape(df_inext$site), "</b><br>"),
      ifelse(indiv, "Individuals: ", "Incidences: "), round(df_inext$m, 1),
      "<br>Est. richness: ", round(df_inext$qD, 1),
      ifelse(is.na(df_inext$qD.LCL), "", paste0("<br>95% CI: ", round(df_inext$qD.LCL, 1), "\u2013", round(df_inext$qD.UCL, 1)))
    )
    y_label <- paste0(get_div_label(outdata$div_profile$input_opts$taxon_rank, capitalize = TRUE), " richness")
    rare_plot <- plotly::ggplotly(
      ggplot2::ggplot() +
        # rarefaction line
        ggplot2::geom_line(
          data = df_inext[df_inext$Method != "Extrapolation", ],
          ggplot2::aes(x = m, y = qD, color = site),
          linewidth = 1, show.legend = TRUE
        ) +
        # extrapolated line
        ggplot2::geom_line(
          data = df_inext[df_inext$Method != "Rarefaction", ],
          ggplot2::aes(x = m, y = qD, color = site),
          linewidth = 1, linetype = "dashed"
        ) +
        # observed point
        ggplot2::geom_point(
          data = df_inext[df_inext$Method == "Observed", ],
          ggplot2::aes(x = m, y = qD, color = site),
          size = 2
        ) +
        # invisible points to anchor tooltips
        ggplot2::geom_point(
          data = df_inext,
          ggplot2::aes(x = m, y = qD, text = tooltip, color = site),
          size = 1.5, alpha = 0
        ) +
        # CI ribbon
        ggplot2::geom_ribbon(
          data = df_inext,
          ggplot2::aes(x = m, ymin = qD.LCL, ymax = qD.UCL, group = site, fill = site, color = site),
          alpha = 0.2, linewidth = 0, show.legend = FALSE
        ) +
        ggplot2::labs(
          x = paste0("Number of ", ifelse(indiv, "individuals", "incidences")),
          y = y_label, colour = "Site", fill = "Site"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::guides(
          colour = ggplot2::guide_legend(override.aes = list(fill = NA, linetype = 1)),
          fill   = "none"
        ) +
        ggplot2::theme(legend.position = ifelse(uniqueN(df_inext$site) > 1, "right", "none")),
      tooltip = "text"
    ) |>
      plotly::layout(
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
  output$plot_preston <- plotly::renderPlotly({
    pr <- outdata$div_profile$preston.plot
    req(pr)
    y_label <- paste0("Number of ", get_div_label(outdata$div_profile$input_opts$taxon_rank, plural = TRUE))
    pr$layers$geom_bar$aes_params$fill <- "#80AAC4"
    pr$layers$geom_bar$aes_params$width <- 0.9
    pr$layers$geom_bar$aes_params$linewidth <- 0
    pr$layers$geom_point$aes_params$fill <- "#CC4945"
    pr$layers$geom_line$aes_params$linewidth <- 0.5
    pr_plot <- plotly::ggplotly(
      pr +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle("") +
        ggplot2::ylab(y_label) +
        ggplot2::xlab("Abundance class"),
      tooltip = "none"
    ) |>
      plotly::layout(
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE)
      )
    pr_plot$x$data[[1]]$hovertemplate <- paste0("Observed: %{y:~g}", "<extra></extra>")
    pr_plot$x$data[[2]]$hovertemplate <- paste0("Fitted: %{y:~g}", "<extra></extra>")
    pr_plot
  })

  # Beta diversity ordination
  output$plot_beta <- plotly::renderPlotly({
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
      "<b>", htmltools::htmlEscape(df_nmds$site), "</b><br>",
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
    plot_beta <- ggplot2::ggplot(df_nmds, ggplot2::aes(x = NMDS1, y = NMDS2, label = site, colour = mean_beta)) +
      ggplot2::geom_point(ggplot2::aes(text = tooltip), size = 3.5, alpha = 0.85) +
      ggplot2::scale_colour_gradientn(
        name = "Mean \u03b2",
        colors = plot_colours
      ) +
      ggplot2::coord_cartesian(xlim = xlim_zoom, ylim = ylim_zoom) +
      ggplot2::labs(x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right")

    beta_fig <- plotly::ggplotly(plot_beta, tooltip = "text")
    main_x_style <- beta_fig$x$layout$xaxis
    main_y_style <- beta_fig$x$layout$yaxis
    # if there are outliers, add an inset plot with zoomed-out view
    if (plot_subplot) {
      beta_fig <- beta_fig |>
        plotly::add_markers(
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
        plotly::layout(
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
        plotly::layout(
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
      options = DT_options()
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
      options = DT_options()
    ),
    server = TRUE
  )

  # base map for occurrence data
  output$record_map <- leaflet::renderLeaflet({
    # canvas rendering keeps the map responsive with large numbers of points
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      leaflet::addTiles(group = "Base") |>
      leaflet::addProviderTiles("Stadia.StamenTerrainBackground",
        options = leaflet::providerTileOptions(noWrap = TRUE),
        group = "Terrain"
      ) |>
      leaflet::addProviderTiles("Esri.WorldImagery",
        options = leaflet::providerTileOptions(noWrap = TRUE),
        group = "Satellite"
      ) |>
      leaflet::addScaleBar(position = "topright") |>
      leaflet::addLayersControl(
        baseGroups = c(
          "Base",
          "Terrain",
          "Satellite"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) |>
      # button to switch between clustered and individual points
      leaflet::addEasyButton(leaflet::easyButton(
        states = list(
          leaflet::easyButtonState(
            stateName = "clustered",
            icon = as.character(icon("circle-dot")),
            title = "Show individual points",
            onClick = DT::JS("function(btn, map) { btn.state('points'); Shiny.setInputValue('map_clustered', false); }")
          ),
          leaflet::easyButtonState(
            stateName = "points",
            icon = as.character(icon("circle-nodes")),
            title = "Group nearby points into clusters",
            onClick = DT::JS("function(btn, map) { btn.state('clustered'); Shiny.setInputValue('map_clustered', true); }")
          )
        ),
        position = "topleft"
      )) |>
      # point size follows the zoom level: 8 px at zoom 6, growing to 10 px when zoomed in, and shrinking by
      # a factor of 1.4 per level when zoomed out, down to 2.5 px (applied to points as they're added, and on zoom)
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          var REF_SIZE = 8, REF_ZOOM = 6, MIN_SIZE = 2.5, MAX_SIZE = 10;
          function radius() {
            var dz = map.getZoom() - REF_ZOOM;
            return dz >= 0 ? Math.min(MAX_SIZE, REF_SIZE * Math.pow(2, dz * 0.15)) : Math.max(MIN_SIZE, REF_SIZE * Math.pow(2, dz * 0.5));
          }
          function resize(layer) {
            if (layer instanceof L.CircleMarker && !(layer instanceof L.Circle)) layer.setRadius(radius());
          }
          map.on('layeradd', function(e) { resize(e.layer); });
          map.on('zoomend', function() { map.eachLayer(resize); });
        }
      ")
  })

  ### Tab UI handling ----

  # convenience list object to handle download/copy from current tab
  table_saves <- c("save_tsv", "save_csv", "save_xlsx", "open_xlsx")
  tab_data <- list(
    data =
      list(
        basename = reactive("BOLD_fetch_"),
        output = out_table,
        current_rows = reactive(input$data_table_rows_all),
        buttons = c("copy_fasta", "save_fasta", "save_dtpkg", table_saves)
      ),
    overview =
      list(
        basename = reactive("search_terms_"),
        output = term_hits,
        current_rows = reactive(seq_len(nrow(term_hits()))),
        buttons = c("copy_table", table_saves)
      ),
    summary =
      list(
        basename = reactive("summary_"), output = reactive(outdata$summary),
        current_rows = reactive(input$summary_table_rows_all),
        buttons = c("copy_table", table_saves)
      ),
    div_profile =
      list(
        basename = reactive({
          if (is.null(input$div_nav)) {
            "diversity_profile_"
          } else {
            tabs <- list(
              "comm_table" = "community_matrix_",
              "site_stats" = "site_diversity_"
            )
            tab <- input$div_nav
            tabs[[tab]]
          }
        }),
        output = reactive({
          outdata$div_profile[[input$div_nav]]
        }),
        current_rows = reactive({
          outdata$div_profile[[input$div_nav]][, .I]
        }),
        # buttons depend on the diversity profile's own tab
        buttons = list(nav = "div_nav", site_stats = table_saves, comm_table = table_saves)
      ),
    consensus_tab =
      list(
        basename = reactive("bin_consensus_"),
        output = reactive(outdata$bin_consensus),
        current_rows = reactive(input$bincons_table_rows_all),
        buttons = c("copy_table", table_saves)
      ),
    bin_reps =
      list(
        basename = reactive("BIN_reps_"),
        output = rep_table,
        current_rows = reactive(input$binrep_table_rows_all),
        buttons = c("copy_fasta", "save_fasta", table_saves)
      ),
    map_tab =
      list(
        basename = reactive(NULL),
        output = NULL,
        buttons = character(0)
      ),
    aln_tab =
      list(
        basename = reactive(NULL),
        output = NULL,
        buttons = c("aln_copy_sel", "aln_copy_all", "aln_save")
      ),
    tree_tab =
      list(
        basename = reactive(paste0("NJ_tree_order_", outdata$tree$marker, "_")),
        output = tree_table,
        current_rows = reactive(seq_len(nrow(tree_table()))),
        buttons = c("btn_label_data", table_saves, "btn_label_tree", "tree_pdf", "tree_newick")
      )
  )

  # the browser shows each tab's buttons (tab_data$buttons) in the bottom button bar and hides the rest (see ui.R)
  button_set <- unique(c("copy_fasta", "save_fasta", "copy_table", "aln_copy_sel", "aln_copy_all",
                          "save_dtpkg", table_saves, "aln_save", "tree_pdf", "tree_newick",
                          "btn_label_data", "btn_label_tree"))
  session$sendCustomMessage("tab_buttons", list(tabs = lapply(tab_data, `[[`, "buttons"), all = button_set))

  ### Data export logic ----

  # obtain FASTA-formatted sequences for current data
  get_fasta <- function(copy = TRUE) {
    collapse_mrkrs <- if (input$collapse_mrkrs == TRUE) {
      # the selected markers' columns, or all markers if none are selected
      coll_fields <- coll_mrkr_fields()
      if (length(input$filt_seq) > 0) {
        coll_fields <- coll_fields[vapply(coll_fields, function(f) any(startsWith(f, paste0(input$filt_seq, "_"))), logical(1))]
      }
      coll_fields[grepl("_nuc$", coll_fields)]
    } else {
      NULL
    }
    data <- if (input$tabs %in% c("data", "bin_reps")) {
      id_col <- intersect(c("processid", "sampleid"), names(tab_data[[input$tabs]]$output()))[1]
      ids <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), get(id_col)]
      if (input$collapse_mrkrs == TRUE) {
        collapsed_data()[get(id_col) %in% ids, ]
      } else if (!is.null(input$filt_seq)) {
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
  observeEvent(input$copy_table, {
    cb(as.data.frame(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]))
  })
  observeEvent(input$copy_fasta, {
    get_fasta(copy = TRUE)
  })
  observeEvent(input$copy_binmates, {
    cb(outdata$binmates, header = FALSE)
  })

  # column copy logic
  observeEvent(input$col_copy_clicked, {
    col_index <- input$col_copy_clicked$col # JS uses 0-based index
    vals <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ][[col_index + 1]]
    vals[vals == "NA"] <- ""
    cb(vals, header = FALSE)
  })
  
  # copy logic for app citation
  observeEvent(input$cite_clicked, {
    cb(gsub("<.*?>", "", citations), header = FALSE)
  })

  # download (save) logic
  download_output <- function(format) {
    downloadHandler(
      filename = function() {
        paste0(tab_data[[input$tabs]]$basename(), as.character(format(Sys.Date(), "%Y%m%d")), ".", format)
      },
      content = function(file) {
        data <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
        if (format != "fasta") data <- prep_export(data, format)
        switch(format,
          tsv = fwrite(data, file, sep = "\t", na = "", quote = FALSE, row.names = FALSE),
          csv = fwrite(data, file, na = "", row.names = FALSE),
          xlsx = writexl::write_xlsx(data, file, format_headers = FALSE),
          fasta = writeLines(fasta_to_save(), file)
        )
      }
    )
  }

  # open logic
  observeEvent(input$open_xlsx, {
    data <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
    filename <- paste0(clean_dtpkg_id(tab_data[[input$tabs]]$basename()), as.character(format(Sys.Date(), "%Y%m%d")), "_")
    temp <- tempfile(pattern = filename, tmpdir = open_files_path, fileext = ".xlsx")
    writexl::write_xlsx(prep_export(data, "xlsx"), temp, format_headers = FALSE)
    open_path(temp)
  })

  # download (save) functions
  output$save_tsv <- download_output("tsv")
  output$save_csv <- download_output("csv")
  output$save_xlsx <- download_output("xlsx")
  # FASTA: saved only when there are sequences (get_fasta() says so otherwise)
  fasta_to_save <- reactiveVal(NULL)
  observeEvent(input$save_fasta, {
    fasta <- get_fasta(copy = FALSE)
    req(length(fasta) > 0)
    fasta_to_save(fasta)
    session$sendCustomMessage("click_link", "save_fasta_dl")
  })
  output$save_fasta_dl <- download_output("fasta")
  # button row download links are shown/hidden by the browser (tab_data$buttons), which Shiny doesn't always notice
  # straight away, so they're kept active while hidden; otherwise, a click could come before a link's address is set
  for (id in c("save_tsv", "save_csv", "save_xlsx", "save_fasta_dl", "tree_newick")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  # function to generate download handlers for diversity analysis results
  dl_div_profile <- function(result) {
    req(reactive(outdata$div_profile))
    datestamp <- as.character(format(Sys.Date(), "%Y%m%d"))
    filenames <- list(
      sum_table = paste0("site_diversity_", datestamp, ".csv"),
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
      nmds_scores = paste0("nmds_scores_", datestamp, ".csv")
    )
    div <- reactive(outdata$div_profile)
    # function to return the output object
    prep_content <- function(prep_result) {
      switch(prep_result,
        sum_table = div()$site_stats,
        comm_mat = div()$comm_table,
        preston_rds = div()$preston.res,
        preston_csv = data.frame(
          observed = div()$preston.res$freq,
          fitted = div()$preston.res$fitted
        ),
        inext_rds = div()$inext,
        inext_size = ,
        inext_cov = {
          type <- switch(prep_result,
            inext_size = "size_based",
            inext_cov = "coverage_based"
          )
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
        shannon = data.table(div()$shannon_div, keep.rownames = unname(div()$input_opts$assemblage_type)),
        beta_dists = data.table(as.matrix(div()$total.beta), keep.rownames = unname(div()$input_opts$assemblage_type)),
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
      if (tools::file_ext(filenames[[result]]) == "rds") {
        saveRDS(prep_content(result), file)
      } else {
        content <- prep_content(result)
        if (is.data.frame(content)) content <- prep_export(content, "csv")
        write.csv(content, file, na = "", row.names = FALSE)
      }
    }
    # function for zipping all outputs into a single archive
    # (zip::zip is used rather than utils::zip, which needs an external 'zip' program that Windows doesn't provide)
    zip_content <- function(filenames, file) {
      # only include results that were actually computed
      requires <- c(
        sum_table = "site_stats", comm_mat = "comm_table", preston_rds = "preston.res", preston_csv = "preston.res",
        inext_rds = "inext", inext_size = "inext", inext_cov = "inext", inext_asy = "inext", shannon = "shannon_div",
        beta_dists = "total.beta", nmds_rds = "nmds.beta", nmds_scores = "nmds.beta"
      )
      filenames <- filenames[!sapply(div()[requires[names(filenames)]], is.null)]
      tmpdir <- tempfile("div_profile_")
      dir.create(tmpdir)
      files <- sapply(seq_along(filenames), function(i) {
        tmpfile <- file.path(tmpdir, filenames[[i]])
        save_content(names(filenames[i]), tmpfile)
        tmpfile
      })
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zip(file, files, mode = "cherry-pick")
      } else {
        utils::zip(file, files, flags = "-j")
      }
      unlink(tmpdir, recursive = TRUE)
    }
    # download handler as return value
    downloadHandler(
      filename = function() {
        if (result == "all_zip") {
          paste0("diversity_profile_", datestamp, ".zip")
        } else {
          filenames[[result]]
        }
      },
      content = function(file) {
        if (result == "all_zip") {
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
}
