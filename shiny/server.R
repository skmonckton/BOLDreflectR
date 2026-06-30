  server <- function(input, output, session) {
    
    session$onSessionEnded(function() {
      stopApp()
    })

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
    
    ### SETUP
    
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
      bin_portal_stats = NULL)
    
    # keep track of output tabs
    tab_status <- reactiveValues(
      data = FALSE,
      qhits_tab = FALSE,
      summary = FALSE,
      bin_reps = FALSE,
      div_profile = FALSE,
      consensus_tab = FALSE,
      map_tab = FALSE
    )
    
    # convenience function for interrogating current tab status
    tab_monitor <- function(get = c("status", "current", "absent", "ins_target"), ins_tab = NULL) {
      tabs <- reactiveValuesToList(tab_status)
      switch(get,
             status = tabs,
             current = names(tabs)[tabs == TRUE],
             absent = names(tabs)[tabs == FALSE],
             ins_target = names(tabs)[max(which(tabs[1:match(ins_tab, names(tabs))] == TRUE))])
    }
    
    ### REACTIVE UI
    
    source_test_code <- function() {
      tryCatch({
        Sys.setenv(GITHUB_PAT = key_get(service="tctools-GH-PAT"))
        tryCatch({
          tmpfile <- tempfile()
          on.exit(unlink(tmpfile))
          content(httr::GET(
            "https://raw.githubusercontent.com/Centre-for-Biodiversity-Genomics/CBG-taxonomy/refs/heads/main/Code/bfr_test_func.R",
            add_headers(Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
          ), "raw") |> writeBin(tmpfile)
          source(tmpfile, local = TRUE)
        }, error = function(e){
          showNotification(paste0("Error accessing test functions:", e$message), type = "error")
        })
      }, error = function(e){
        showModal(
          modalDialog(
            title = "Test user access",
            textInput("testkey", "Please input your access token:"),
            footer = tagList(
              div(),
              div(id="modal_confirm",
                  actionButton("testkey_confirm", "Confirm"),
                  modalButton("Cancel"))),
            easyClose = TRUE))
      })
    }
    
    test_mode <- reactiveVal()
    session$onFlushed(function() {
      test_mode(user_config$settings$test_mode)
    }, once = TRUE)
    observeEvent(input$cbg_btn, source_test_code()) 
    observeEvent(test_mode(), if(isTRUE(test_mode())) source_test_code(), once = TRUE)
    observeEvent(input$testkey_confirm, {
      Sys.setenv(GITHUB_PAT = trimws(gsub('"', "", input$testkey)))
      
      try({
        key_set_with_value("tctools-GH-PAT", password = Sys.getenv("GITHUB_PAT"))
        }, silent=TRUE)
      source_test_code()
    })
    
    # conditional input logic to constrain marker search options (min/max requires a marker to be selected)
    observeEvent(input$seq_marker, {
      if((!is.null(input$seq_marker)) & (input$seq_marker != "")) {
        shinyjs::enable("seq_min")
        shinyjs::enable("seq_max")
      } else {
        shinyjs::disable("seq_min")
        shinyjs::disable("seq_max")
        updateNumericInput(inputId = "seq_min", value = character())
        updateNumericInput(inputId = "seq_max", value = character())
      }
    })
    
    # disable option to find additional BIN records when fetching by BIN
    # (in this case, fetching BIN mates will always return zero records)
    observe({
      if((input$fetch_by == "bin_uris") & (input$query_params == "fetch_opts")) {
        updateCheckboxInput(inputId = "fetch_binmates", value = FALSE)
        disable("fetch_binmates")
      } else {
        enable("fetch_binmates")
      }
    })
    
    # automatically update min/max marker length to avoid impossible ranges
    observeEvent(input$last_blurred, {
      id <- input$last_blurred
      value <- input[[id]]
      
      is_min <- grepl("_min", id)
      is_max <- grepl("_max", id)
      sibling_id <- if (is_min) sub("_min", "_max", id) else sub("_max", "_min", id)
      sibling_value <- input[[sibling_id]]
      
      val_range <- fcase(grepl("seq", id), list(c(5, 2000)))
      
      if(isTRUE(value < val_range[[1]][1])) updateNumericInput(inputId = id, value = val_range[[1]][1])
      if(isTRUE(value > val_range[[1]][2])) updateNumericInput(inputId = id, value = val_range[[1]][2])
      if(isTRUE(is_max && value < sibling_value)) updateNumericInput(inputId = id, value = sibling_value)
      if(isTRUE(is_min && value > sibling_value)) updateNumericInput(inputId = id, value = sibling_value)
    }, ignoreInit = TRUE)
    
    # only show NTS toggle when data contains NTS records
    observe({
      nts_idx <- nts_idx()
      req(length(nts_idx) > 0)
      if(sum(nts_idx) > 0) {
        shinyjs::show('include_nts')
      } else {
        shinyjs::hide('include_nts')
      }
    })
    
    # reactively adjust available diversity analysis options
    observeEvent(input$div_site_type, {
      if(input$div_site_type == "locations") {
        shinyjs::show('div_location_type')
        shinyjs::hide('div_grid_size')
      } else if(input$div_site_type == "grids") {
        shinyjs::show('div_grid_size')
        shinyjs::hide('div_location_type')
      } else {
        shinyjs::hide('div_location_type')
        shinyjs::hide('div_grid_size')
      }
    })
    
    observe({
      req(filtered_data())
      data <- filtered_data()[input$data_table_rows_all, ]
      req(nrow(data) > 0)
      site_types <- list("Site" = "site",
                         "Sector" = "sector",
                         "Region" = "region",
                         "Province/State" = "province.state",
                         "Country/Ocean" = "country.ocean")
      taxon_ranks <- c("Species if known, BIN if not" = "bin_fallback",
                       "BIN" = "bin_uri",
                       rev(config$bcdmnames)[rev(config$bcdmnames) %in% ranks])
      allow_cols <- names(which(unlist(data[, lapply(.SD, function(x) uniqueN(!empty(x)) >= 2),
                                            .SDcols = unlist(unname(c(site_types, taxon_ranks[taxon_ranks != "bin_fallback"])))])))
      if(any(c("bin_uri","species") %in% allow_cols)) allow_cols <- c(allow_cols, "bin_fallback")
      
      allow_sites <- site_types[site_types %in% allow_cols]
      updateSelectizeInput(session, "div_location_type",
                           choices = allow_sites,
                           selected = allow_sites[[1]])
      
      allow_ranks <- taxon_ranks[taxon_ranks %in% allow_cols]
      updateSelectInput(session, "div_rank", choices = allow_ranks, selected = allow_ranks[1])
    })
    
    observeEvent(input$div_profile, {
      if("beta" %in% input$div_profile) {
        shinyjs::enable('div_beta_type')
      } else {
        shinyjs::disable('div_beta_type')
      }
    })
    
    ### SERVER HELPERS
    
    # function to reset filter options
    reset_filter <- function(include_verbatim = TRUE) {
      if (!isolate(input$tabs) %in% c("data", "bin_reps", "map_tab")) {bslib::nav_select(id="tabs", selected="data")}
      outdata$select_fields <- if (is.null(isolate(outdata$data))) {
        config$fieldsets$bcdm
      } else {
        c(config$fieldsets$bcdm, isolate(coll_mrkr_fields()))
      }
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options(include_verbatim),
                           selected = NULL)
      updateSelectizeInput(session, "filt_seq",
                           choices = outdata$markers,
                           selected = NULL)
    }
    
    # function to reset UI (and reactive values) to blank state
    reset_ui <- function() {
      for(i in seq_along(init_outdata)) {
        outdata[[names(init_outdata[i])]] <- unname(unlist(init_outdata[i]))
      }
      for(i in seq_along(init_fetch_params)) {
        fetch_params[[names(init_fetch_params[i])]] <- unname(unlist(init_fetch_params[i]))
      }
      bslib::nav_select(id="tabs", selected = "data")
      for(t in names(tab_status)[names(tab_status) != "data"]) bslib::nav_remove(id="tabs", target = t, session)
      for(i in seq_along(reactiveValuesToList(tab_status))) tab_status[[names(tab_status)[i]]] <- FALSE
      bslib::update_switch("include_binmates", value = FALSE)
      bslib::update_switch("include_nts", value = FALSE)
      bslib::update_switch("collapse_mrkrs", value = FALSE)
      updateCheckboxInput(inputId = "bc_portal_stats", value = FALSE)
    }
    
    # execute reset function when button is pressed
    observeEvent(input$reset_btn, {
      reset_filter()
    })
    
    ### DATA FETCHING
    
    # set API key and generate fetch_ids
    observeEvent(input$fetch_btn | input$fetch_ctrl_enter, {
      shinyjs::hide("main_panel")
      req(input$api_key)
      tryCatch({
        key <- trimws(gsub('"', "", input$api_key))
        if (Sys.getenv("api_key") != key) {
          bold.apikey(key)
          try(key_set_with_value("BOLD.apikey", password = key), silent=TRUE)
        }
      }, error = function(e) {
        showNotification(e$message, id="fetch_msg", type = "error")
      })
      req(((isTruthy(input$fetch_id_list) || isTruthy(input$search_tax) || isTruthy(input$search_geo) || 
            isTruthy(input$seq_marker)) && isTruthy(Sys.getenv("api_key"))))
      shinyjs::show('table_area')
      shinyjs::hide('table_buttons')
      reset_ui()
      tab_status$data <- TRUE
      shinyjs::show("main_panel")
      tryCatch({
        if(input$query_params == "search_opts") {
          showNotification("Searching for matching records...", id = "fetch_msg", duration=NULL, type = "message")
          search_query <- list(taxonomy = split_query(input$search_tax, list = TRUE),
                               geography = split_query(input$search_geo, list = TRUE),
                               marker = input$seq_marker[input$seq_marker != ""],
                               marker_min_length = input$seq_min[!is.na(input$seq_min)],
                               marker_max_length = input$seq_max[!is.na(input$seq_max)],
                               collection_start_date = input$search_dates[1][!is.na(input$search_dates[1])],
                               collection_end_date = input$search_dates[2][!is.na(input$search_dates[1])],
                               institutes = split_query(input$search_inst, list = TRUE))
          fetch_params$query <- search_query[sapply(search_query, length) > 0]
          search_token <- do.call(bold.full.search.step1, search_query[sapply(search_query, length) > 0])
          if(is.null(search_token)) {
            showNotification("No records found matching search terms.", id="fetch_msg", type = "warning")
          } else if(search_token$num_of_accessible >= 100000) {
            removeNotification(id = "fetch_msg")
            fetch_params$search_token <- search_token
            search_confirm_modal()
          } else {
            showNotification(paste0("Downloading ids of ",
                                    format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE),
                                    "  matching records..."), id = "fetch_msg", duration=NULL, type = "message")
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
      }, warning = function(w) {
        showNotification("No results found using provided search terms.", id="fetch_msg", type = "warning")
      }, error = function(e) {
        showNotification(paste0("Error processing record identifiers: ", e$message), id="fetch_msg", type = "error")
      })
    }, ignoreInit = TRUE)
    
    # modal to call up upon finding over 100k records from search
    search_confirm_modal <- function() {
      req(fetch_params$search_token)
      search_token <- fetch_params$search_token
      modal_msg <- p(HTML(paste0("Search returned <strong>", format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE), "</strong> accessible records.")))
      if(search_token$num_of_records >= search_token$limit) {
        modal_msg <- div(modal_msg, p(class="warn-text", HTML("<strong>Note:</strong> Maximum search limit of ", format(search_token$limit, big.mark = ",", scientific = FALSE), " was reached. Additional matches may exist beyond what can be fetched.")))
      }
      showModal(
        modalDialog(
          title = "Confirm fetch",
          modal_msg,
          footer = tagList(
            div("Proceed to fetch records?"),
            div(id="modal_confirm",
                actionButton("search_confirm_btn", "Yes"),
                modalButton("No"))),
          easyClose = TRUE)
      )
    }
    
    # upon confirming search, proceed to get fetch_ids (process IDs)
    observeEvent(input$search_confirm_btn, {
      removeModal()
      search_token <- fetch_params$search_token
      tryCatch({
        showNotification(paste0("Downloading ids of ",
                                format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE),
                                "  matching records..."), id = "fetch_msg", duration=NULL, type = "message")
        fetch_params$fetch_by <- "processid"
        fetch_params$fetch_ids <- bold.full.search.step2(search_token)$processid
      }, error = function(e) {
        showNotification(paste0("Error retrieving records from BOLD: ", e$message), id="fetch_msg", type = "error")
      }) 
    })
    
    # perform fetch
    observeEvent(fetch_params$fetch_ids, {
      req(fetch_params$fetch_ids, fetch_params$fetch_by)
      data <- as.data.table(bold.fetch.shiny(get_by = fetch_params$fetch_by,
                               query = fetch_params$fetch_ids,
                               BCDM_only = FALSE))
      req(nrow(data) > 0)
      populate_data(data)
    })
    
    # populate data
    populate_data <- function(data) {
      data[, c("id_date_parsed", "project_code", "lat", "lon") := c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdata$id_field)), parse_lat_lon(coord))]
      cols_to_factor <- intersect(config$fieldsets$factorfields, names(data))
      data[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]
      if(fetch_params$fetch_by == "bin_uris") {
        shinyjs::hide("include_binmates")
        shinyjs::hide("view_binmates")
      } else {
        shinyjs::show("include_binmates")
        shinyjs::show("view_binmates")
      }
      outdata$markers <- gsub("ZZZ", "None", sort(unique(c(levels(data$marker_code), if(anyNA(data$marker_code)) "ZZZ"))))
      outdata$data <- data
      
      shinyjs::show('table_buttons')
      bslib::accordion_panel_close(id="optpanels", values="fetchdata")
      bslib::accordion_panel_open(id="optpanels", values=c("customize","summarize","analyze"))
      if (input$fetch_binmates == TRUE) {
        binmate_modal()
      }
    }
    
    # logic for initiating search (or fetch) of additional BIN records ("BIN mates") 
    # if the BIN mates switch is toggled on, a modal opens to report the number of additional BIN records
    observeEvent(input$include_binmates, {
      req(outdata$data)
      if ((isolate(input$include_binmates) == TRUE) && (isolate(fetch_params$binmates_checked) == FALSE)) {
          binmate_modal()
      } else if((length(outdata$binmates) > 0) && (fetch_params$binmates_fetched == FALSE)) {
          fetch_binmates()
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
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
      if(length(binmate_pids) == 0) {
        modal_msg <- div(HTML(paste0("No additional BIN members found.")))
        modal_footer <- tagList(
          div(id="modal_confirm",
              actionButton("binmate_btn", "OK")))
        binmate_list <- div("")
      } else if (fetch_params$binmates_checked == FALSE) {
        modal_msg <- div(HTML(paste0("Found <strong>",length(binmate_pids),"</strong> additional BIN members. Retrieve and add to table?")))
        modal_footer <- tagList(
          actionButton("copy_binmates", "Copy"),
          div(id="modal_confirm",
              actionButton("binmate_btn", "Yes"),
              actionButton("binmate_no", "No"))
        )
        binmate_list <- verbatimTextOutput("binmate_pids")
      } else {
        modal_msg <- div(HTML(paste0("The following <strong>",length(binmate_pids),"</strong> additional BIN members were added to the retrieved data.")))
        modal_footer <- tagList(
          actionButton("copy_binmates", "Copy"),
          div(id="modal_confirm",
              modalButton("OK"))
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
      binmate_data <- as.data.table(bold.fetch.shiny(get_by = "processid",
                                                     query = outdata$binmates,
                                                     BCDM_only = FALSE))
      add_binmates(binmate_data)
    }
    
    add_binmates <- function(binmate_data) {
      showNotification("Processing additional BIN member data...",
                       id = "fetch_msg", duration=NULL, type = "message")
      binmate_data <- binmate_data[, c("id_date_parsed", "project_code", "lat", "lon") :=
                                     c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdata$id_field)), parse_lat_lon(coord))]
      cols_to_factor <- intersect(config$fieldsets$factorfields, names(binmate_data))
      binmate_data[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]
      data <- unique(rbindlist(list(outdata$data,
                                    binmate_data),
                               fill = TRUE))
      showNotification("Additional BIN members retrieved.",
                       id = "fetch_msg", duration=2, type = "message")
      outdata$markers <- gsub("ZZZ", "None", sort(unique(c(levels(data$marker_code), if(anyNA(data$marker_code)) "ZZZ"))))
      outdata$data <- data
      fetch_params$binmates_fetched <- TRUE
    }
    
    # user can close the BIN mate modal without fetching BIN mates
    observeEvent(input$binmate_no, {
      removeModal()
      bslib::update_switch("include_binmates",label="Include additional BIN members",value=FALSE,session)
    })
    
    # fetch BIN mates and add them to the data
    observeEvent(input$binmate_btn, {
      removeModal()
      if(length(outdata$binmates) > 0) {
        fetch_binmates()
        bslib::update_switch("include_binmates",label="Include additional BIN members",value=TRUE,session)
      } else {
        bslib::update_switch("include_binmates",label="Include additional BIN members",value=FALSE,session)
      }
    })
    
    ### REACTIVE COMPUTATIONS AND OBSERVERS
    
    # keep summary analysis options updated according to available fields
    observeEvent(outdata$data, {
      req(outdata$data)
      updateSelectInput(session, 
                        "ana_opt",
                        choices = list(
                          "Taxonomy" = list(
                            "Taxonomic summary" = "tax_summary"),
                          "Unique values - selected fields" = list(
                            "Taxon" = "identification",
                            "BIN" = "bin_uri",
                            "Identification dates (parsed)" = "id_date_parsed",
                            "Projects" = "projects",
                            "Datasets" = "datasets",
                            "Country" = "country.ocean"),
                          "Unique values - all fields" = as.list(
                            names(outdata$data)[
                              !names(outdata$data) %in% c("identification","bin_uri","country.ocean","id_date_parsed","project_code")])
                        ))
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
        for(o in input$filt_opt) {
          if (grepl("^userset", o)) {
            field_opts <- unique(c(field_opts, unlist(sapply(user_config$fieldsets$custom, function(x) if(x$id == o) x$fields))))
          } else {
            field_opts <- unique(c(field_opts, o))
          }
        }
        
        # determine which filter selections are field sets from config specification
        sets <- field_opts[(field_opts %in% names(config$fieldsets)) | grepl("|", field_opts, fixed=T)]
        
        fields <- c()
        # step through selected fields, handling them according to whether they are preset field sets, custom field sets, or individual fields
        for(o in field_opts) {
          if(o %in% sets) {
            opt <- unlist(strsplit(o, "|", fixed=TRUE))
            if(length(opt) > 1) {
              fields <- unique(c(fields, opt[-1]))
            } else {
              fields <- unique(c(fields, config$fieldsets[o][[1]]))
            }
          } else {
            fields <- unique(c(fields, o))
          }
        }
        
        id_idx <- which(c("processid", "sampleid") %in% fields)
        if(length(id_idx) == 0) {
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
          div(selectInput( 
            "sets_to_delete", 
            "Existing presets:", 
            sapply(user_config$fieldsets$custom, function(s) setNames(s$id, s$name)), 
            multiple = TRUE,
            selectize = FALSE, width = "100%"),
            hidden(actionLink("del_filter_btn", "× Delete selected", class = "warn-text"))),
          textInput(
            "save_filter_name", 
            tagList(span("Save current column selection as:"),
                bslib::tooltip(
                  icon("circle-question"),
                  "Using the name of an existing preset will update/overwrite it.")),
            width = "100%"),
          footer = tagList(
            div(),
            div(id="modal_confirm",
                actionButton("save_filter_btn", "Save"),
                modalButton("Cancel"))),
          easyClose = TRUE)
      )
    })
    
    # field set save logic
    observeEvent(input$save_filter_btn, {
      req(isTruthy(input$save_filter_name), isTruthy(input$filt_opt))
      removeModal()
      filter_set <- modify_custom_fieldset(input$save_filter_name, input$filt_opt)
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options(),
                           selected = filter_set)
    })
    
    # reactively show/hide delete button
    observeEvent(input$sets_to_delete, {
      if(isTruthy(input$sets_to_delete)) {
        shinyjs::show("del_filter_btn")
      } else {
        shinyjs::hide("del_filter_btn")
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # field set delete logic
    observeEvent(input$del_filter_btn, {
      for(s in input$sets_to_delete) {
        filter_set <- modify_custom_fieldset(s, operation = "delete")
      }
      updateSelectInput(
        inputId = "sets_to_delete", 
        choices = sapply(user_config$fieldsets$custom, function(s) s$name),
        selected = NULL
      )
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options(),
                           selected = input$filt_opt[input$filt_opt %in% unname(unlist(input$filt_opt))])
    })
    
    # keep marker filters updated based on available data
    observeEvent(outdata$markers, {
      updateSelectizeInput(session, "filt_seq", choices = outdata$markers, selected = NULL)
    })
    
    # function to compute a row index based on user-selected markers
    # this is needed for the flexibility to work either with the full table or collapsed-marker table  
    seq_filter <- function(data, filt_seq) {
      
      filt_markers <- filt_seq[filt_seq != "None"]
      nuc_cols <- intersect(paste0(filt_markers, "_nuc"), names(data))
      keep_seq <- list()
      
      if("marker_code" %in% names(data)) {
        if (length(filt_markers) > 0) {
          keep_seq[[1]] <- data[["marker_code"]] %in% filt_markers
        }
        if ("None" %in% filt_seq & ("marker_code" %in% names(data))) {
          keep_seq[[2]] <- empty(data[["marker_code"]])
        }
      }
      if(length(nuc_cols) > 0) {
        keep_seq[[3]] <- rowSums(data[, lapply(.SD, function(x) !empty(x)), .SDcols = nuc_cols]) > 0
      }
      
      if(length(keep_seq) > 0) {
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
      
      if(!isTRUE(input$include_nts)) {
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
      if(!tab_status$summary) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","summary"), position = "after", select = TRUE,
          bslib::nav_panel(id = "summary",
                    value = "summary",
                    "Summary",
                    div(id = 'sum_table_area',
                        DT::dataTableOutput("summary_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6)))
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
      if(!tab_status$qhits_tab) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","qhits_tab"), select = TRUE,
          bslib::nav_panel(
            id="qhits_tab",
            value="qhits_tab",
            "Hit report",
            hits$report)
        )
        tab_status$qhits_tab <- TRUE
      } else {
        bslib::nav_select(id = "tabs", selected = "qhits_tab")
      }
    })
    
    # compute diversity profile
    observeEvent(input$div_compute_btn, {
      req(outdata$data)
      
      data <- filtered_data()[input$data_table_rows_all, ]
      
      data <- filtered_data()

      if(any(c("beta", "all") %in% input$div_profile)) {
        site_type <- input$div_location_type
        if(length(unique(data[!empty(get(site_type))][[site_type]])) <= 1) {
          showNotification("Must have at least two sites for beta diversity analysis.", type = "error")
          validate(need(FALSE, ""))
        }
      }
      
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
      
      if(div_opts$taxon_rank == "bin_fallback") {
        withInfProgress(message = "Assigning fallback BIN-based names...", {
          unid_bins <- unique(data[, if(all(empty(species))) .SD, by = bin_uri]$bin_uri)
          unid_bin_tax <- get_bin_consensus(data[bin_uri %in% unid_bins], min_ids = 1)
          
          data[, bin_fallback := fifelse(bin_uri %in% unid_bins,
                                         unid_bin_tax[.SD, on = .(bin_uri), 
                                                      paste(concordant_id, bin_uri, sep="_sp_")],
                                         as.character(species))]
        })
      }
      
      if(input$div_site_type == "locations") {
        div_opts[["location_type"]] <- input$div_location_type
        data <- data[!empty(get(div_opts$location_type))]
      } else {
        div_opts[["gridsize"]] <- input$div_grid_size
        data <- data[!empty(coord)]
      }
      
      div_opts[["bold_df"]] <- data[!empty(get(div_opts$taxon_rank))]
      
      withInfProgress(message = "Computing diversity profile...", {
        div_profile <- do.call(bold.analyze.diversity, div_opts)
        if("beta" %in% input$div_profile) {
          beta_div_results <- BOLDconnectR:::beta_div_profile(div_profile$comm.matrix,
                                                              input$div_beta_type,
                                                              input$div_presence)
          div_profile$total.beta <- beta_div_results$total.beta
          div_profile$replace <- beta_div_results$replace
          div_profile$richnessd <- beta_div_results$richnessd
        }
        if("shannon" %in% input$div_profile) {
          div_profile$shannon_div <- round(BOLDconnectR:::shannon_div_profile(df = div_profile$comm.matrix), 2)
        }
        outdata$div_profile <- div_profile
      })
      
      if(!tab_status$div_profile) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","div_profile"), position = "after", select = TRUE,
          bslib::nav_panel(
            id="div_profile",
            value="div_profile",
            "Diversity profile",
            bslib::navset_card_pill( 
              # bslib::nav_panel("Richness metrics",
                        # DT::dataTableOutput("richness_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6)),
              
              bslib::nav_panel("Accumulation curve", 
                        plotlyOutput("plot_accum")), 
              bslib::nav_panel("Preston plot", 
                        plotlyOutput("plot_preston")),
              bslib::nav_panel("Community matrix",
                        DT::dataTableOutput("comm_matrix") |> withSpinner(color="#aaaaaa", type=5, size=0.6)),
              bslib::nav_panel("Beta diversity ordination plot", 
                        plotlyOutput("plot_beta")))
          )
        )
        tab_status$div_profile <- TRUE
      } else {
        bslib::nav_select(id = "tabs", selected = "div_profile")
      }
    })
    
    # compute BIN consensus and inject as a new tab
    observeEvent(input$bin_consensus_btn, {
      req(nrow(outdata$data) > 0)
      # outdata$bin_consensus <- NULL
      data <- filtered_data()[input$data_table_rows_all, ][!empty(bin_uri)]
      withInfProgress(message = "Computing consensus BIN taxonomy...", {
        bin_consensus <- get_bin_consensus(df = data,
                                           threshold = as.double(unlist(unname(input$bc_threshold))),
                                           min_ids = unlist(unname(input$bc_minids)),
                                           enforce_scientific = input$bc_enforcesci,
                                           discord_format = "text")
        col_order <- append(names(bin_consensus), c("min_rank", "max_rank"), after = 2)
        bin_consensus <- merge(bin_consensus,
                               data[, .(min_rank = as.factor(ranks[min(match(get("identification_rank"), ranks))]),
                                        max_rank = as.factor(ranks[max(match(get("identification_rank"), ranks))])), by = "bin_uri"],
                               by = "bin_uri",
                               all.x = TRUE)
        setcolorder(bin_consensus, col_order)
      })
      
      outdata$bin_consensus <- bin_consensus
      
      if(!tab_status$consensus_tab) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","consensus_tab"), position = "after", select = TRUE,
          bslib::nav_panel(
            id="consensus_tab",
            value="consensus_tab",
            "BIN consensus",
            div(id = 'consensus_table_area',
                DT::dataTableOutput("bincons_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6))
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
        if(input$bc_portal_stats == TRUE && is.null(outdata$bin_portal_stats)) {
          outdata$bin_portal_stats <- get_portal_bin_stats(as.character(isolate(outdata$bin_consensus$bin_uri)), shiny = TRUE)
          outdata$bin_consensus <- merge(isolate(outdata$bin_consensus),
                                         isolate(outdata$bin_portal_stats),
                                         by = "bin_uri",
                                         all.x = TRUE)
          bslib::nav_select(id="tabs", selected="consensus_tab")
        }
      })
    })
    
    # the portal API can be somewhat inconsistent; this will insert a warning message about any BINs for which the query timed out
    insert_portal_warning <- function(portal_stats) {
      removeUI("#bc_portal_warning", immediate = TRUE)
      failed <- nrow(portal_stats[empty(member_count)])
      if(failed > 0) {
        insertUI(
          selector = "div.form-group:has(#bc_portal_stats):not(:has(div.form-group:has(#bc_portal_stats)))",
          where = "beforeEnd",
          ui = div(id = "bc_portal_warning",
                   class="control-label warn-text",
                   HTML(paste0("Portal stats timed out for ", failed, " BINs. ")),
                   actionLink("bc_portal_retry", "Click to retry."))
        )
      }
    }
    
    
    # check portal stats upon calculation and insert UI with a link to retry the query for failed BINs
    observeEvent(outdata$bin_portal_stats, {
      portal_stats <- outdata$bin_portal_stats
      req(portal_stats)
      if("member_count" %in% names(portal_stats)) {
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
      outdata$bin_consensus <- merge(cons[, .SD, .SDcols = c("bin_uri",names(cons)[!names(cons) %in% names(init_stats)])],
                                     init_stats,
                                     by = "bin_uri",
                                     all.x = TRUE)
      bslib::nav_select(id="tabs", selected="consensus_tab")
      outdata$bin_portal_stats <- init_stats
      insert_portal_warning(init_stats)
    })
    
    # select BIN reps and inject as a new tab
    observeEvent(input$bin_rep_btn, {
      req(nrow(outdata$data) > 0)
      outdata$bin_reps <- NULL
      # outdata$bin_reps <- get_bin_reps(filtered_data()[!empty(bin_uri)])
      withInfProgress(message = "Selecting BIN representatives...", {
        outdata$bin_reps <- get_bin_reps(filtered_data()[input$data_table_rows_all, ][!empty(bin_uri)])
      })
      if(!tab_status$bin_reps) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","bin_reps"), position = "after", select = TRUE,
          bslib::nav_panel(
            id="bin_reps",
            value="bin_reps",
            "BIN reps",
            div(id = 'rep_table_area',
                DT::dataTableOutput("binrep_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6))
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
      if(!tab_status$map_tab) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","map_tab"), position = "after", select = TRUE,
          bslib::nav_panel(
            id="map_tab",
            value="map_tab",
            "Occurrence map",
            div(id = 'map_area',
                leafletOutput("record_map"))
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
        addCircleMarkers(lat = data$lat,
                         lng = data$lon,
                         color = "#444",
                         fillColor = pal(data$identification),
                         weight = 2,
                         radius = 9,
                         opacity = 0.3,
                         fillOpacity = 0.7,
                         popup = paste0(
                           '<span class="tag">Sample ID:</span><span class="value"><a href="https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSpecimen?selectedrecordid=', data$processid, '" target="_blank">', data$sampleid,'</a></span>',
                           '<span class="tag">Process ID:</span><span class="value">', data$processid, '</span>',
                           '<span class="tag">Identification:</span><span class="value">', data$identification,'</span>',
                           '<span class="tag">Country:</span><span class="value">', data$country.ocean, '</span>',
                           '<span class="tag">Province/State:</span><span class="value">', data$province.state, '</span>',
                           '<span class="tag">Region:</span><span class="value">', data$region, '</span>',
                           '<span class="tag">Sector:</span><span class="value">', data$sector, '</span>',
                           '<span class="tag">Site:</span><span class="value">', data$site, '</span>'),
                         clusterOptions = markerClusterOptions())
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
                 .SDcols = intersect(outdata$select_fields, names(outdata$data))]
    })
    
    ### OUTPUT RENDERING
  
    # build a reactive JSON lookup map and send it to the window
    # (enables hyperlinking of sample IDs and process IDs even when dependent columns are excluded)
    observeEvent(outdata$data, {
      req(outdata$data)
      json <- jsonlite::toJSON(outdata$data[, .(sampleid, processid, specimenid)], auto_unbox = TRUE)
      session$sendCustomMessage("initLookupMap", json)
    })
    
    # define options once, then add them to each table individually
    DT_extensions <- c("FixedColumns")
    DT_options <- list(headerCallback = header_js,
                       columnDefs = list(
                         list(
                           targets = "_all",
                           render = column_js)),
                       scrollCollapse = TRUE,
                       lengthMenu = list(c(100, 500, 1000, 2000), c('100', '500', '1000', '2000')),
                       # fixedHeader = TRUE,
                       fixedColumns = list(leftColumns = 1),
                       stateSave = TRUE,
                       searchDelay = 500,
                       initComplete = DT::JS("function(settings, json) {
                                            var dt = this.api();
                                            $(document).off('shown.bs.tab.dt').on('shown.bs.tab.dt', function() {
                                              dt.columns.adjust();
                                            });
                                          }"))
    
    # generate binmate list for display
    output$binmate_pids <- renderPrint(writeLines(outdata$binmates))
    
    # main data table
    output$data_table <- DT::renderDataTable(DT::datatable({
      droplevels(out_table())
      },
      filter = 'top',
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options),
      server = TRUE)
    
    # summary analysis table
    output$summary_table <- DT::renderDataTable(DT::datatable({
      droplevels(outdata$summary)
      },
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options),
      server = TRUE)
    
    # diversity analysis table
    # output$richness_table <- DT::renderDataTable(DT::datatable({
    #   outdata$div_profile$richness
    #   },
    #   filter = 'none', 
    #   rownames = FALSE,
    #   selection = 'none',
    #   extensions = DT_extensions,
    #   callback = callback_js, 
    #   options = DT_options),
    #   server = TRUE)
    
    # community matrix
    output$comm_matrix <- DT::renderDataTable(DT::datatable({
      mat <- outdata$div_profile$comm.matrix
      mat_df <- data.frame(Site = rownames(mat), mat)
      names(mat_df)[1] <- tools::toTitleCase(isolate(input$div_location_type))
      mat_df
      },
      filter = 'none', 
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js, 
      options = DT_options),
      server = TRUE)
    
    # species accumulation plot
    output$plot_accum <- renderPlotly({
      mat <- outdata$div_profile$comm.matrix
      validate(need(nrow(mat) > 1, "Need at least 2 sites for an accumulation curve."))
      
      sac <- vegan::specaccum(mat, method = "random")
      
      by <- max(1L, floor(length(sac$sites) / 50))
      idx <- c(seq(1, length(sac$sites), by = by), length(sac$sites))
      
      df_sac <- data.frame(sites = sac$sites[idx], richness = sac$richness[idx], sd = sac$sd[idx])
      
      df_sac$tooltip <- paste0(
        "Sites: ", df_sac$sites,
        "<br>Richness: ", round(df_sac$richness, 1),
        "<br>Range: ", round(df_sac$richness - df_sac$sd, 1),
        "–", round(df_sac$richness + df_sac$sd, 1)
      )
      
      y_label <- paste0(switch(isolate(input$div_rank),
                                bin_uri = "BIN",
                                bin_fallback = "Species",
                                tools::toTitleCase(isolate(input$div_rank))),
                         " richness")
      ggplotly(
        ggplot(df_sac, aes(x = sites, y = richness)) +
          geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), 
                      fill = "#F78E1E", alpha = 0.2) +
          geom_line(color = "#F78E1E", linewidth = 1) +
          geom_point(aes(text = tooltip), color = "#DB6822", size = 2) +
          labs(x = "Number of sites",
               y = y_label) +
          theme_minimal(), tooltip = "text") |>
        layout(
          xaxis = list(autorange = TRUE),
          yaxis = list(autorange = TRUE)
        )
    })
    
    # Preston plot
    output$plot_preston <- renderPlotly({
      pr <- outdata$div_profile$preston.plot
      validate(need(!is.null(pr), "Preston results not available."))
      
      y_label <- switch(isolate(input$div_rank),
                        bin_uri = "Number of BINs",
                        bin_fallback =, "Number of species",
                        subspecies =, species = paste0("Number of ", isolate(input$div_rank)),
                        genus = "Number of genera",
                        subfamily = "Number of subfamilies",
                        family = "Number of families",
                        class = "Number of classes",
                        tribe =, order = paste0("Number of ", isolate(input$div_rank), "s"))
      
      pr$layers$geom_bar$aes_params$fill <- "#80AAC4"
      pr$layers$geom_bar$aes_params$width <- 0.9
      pr$layers$geom_bar$aes_params$linewidth <- 0
      pr$layers$geom_point$aes_params$fill <- "#CC4945"
      pr$layers$geom_line$aes_params$linewidth <- 0.5
      
      pr_plot <- ggplotly(pr + 
                            theme_minimal() + 
                            ggtitle("") + 
                            ylab(y_label) + 
                            xlab("Abundance class"), 
                          tooltip = "none") |>
        layout(
          xaxis = list(autorange = TRUE),
          yaxis = list(autorange = TRUE)
        )
      
      pr_plot$x$data[[1]]$hovertemplate <- paste0("Observed: %{y:~g}", "<extra></extra>")
      pr_plot$x$data[[2]]$hovertemplate <- paste0("Fitted: %{y:~g}", "<extra></extra>")
      
      pr_plot
      
    })
    
    # beta diversity stats
    # output$beta_stats <- renderUI({
    #   res <- results()
    #   mat <- res$comm.matrix
    #   n_sites   <- nrow(mat)
    #   n_taxa    <- ncol(mat)
    #   shannon_h <- if (!is.null(res$shannon_div)) round(mean(as.numeric(unlist(res$shannon_div)), na.rm = TRUE), 2) else "—"
    #   total_b   <- if (!is.null(res$total.beta)) round(mean(res$total.beta,  na.rm = TRUE), 3) else "—"
    #   
    #   chip <- function(val, lbl) {
    #     div(class = "stat-chip",
    #         span(class = "chip-val", val),
    #         span(class = "chip-lbl", lbl)
    #     )
    #   }
    #   div(class = "summary-strip",
    #       chip(n_sites,   "sites"),
    #       chip(n_taxa,    input$taxon_rank),
    #       chip(shannon_h, "mean H′"),
    #       chip(total_b,   "mean β")
    #   )
    # })
    
    # beta diversity ordination
    output$plot_beta <- renderPlotly({
      res <- outdata$div_profile
      validate(need(!is.null(res$total.beta), "Beta diversity results not available."))
      
      beta_mat <- res$total.beta

      validate(need(attr(beta_mat, "Size") >= 3,
                    "Need at least 3 sites to run ordination."))
      
      ord_type <- "nmds"
      
      if(ord_type == "nmds") {
      
        set.seed(42)
        nmds_res <- vegan::monoMDS(beta_mat, k = 2, maxit = 500)
        
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
        
        # Identify outliers and scale axes
        is_outlier <- (df_nmds$NMDS1 %in% boxplot.stats(df_nmds$NMDS1)$out) | (df_nmds$NMDS2 %in% boxplot.stats(df_nmds$NMDS2)$out)
        xlim_zoom  <- extendrange(range(df_nmds$NMDS1[!is_outlier]), f = c(0.1, 0.3))
        ylim_zoom  <- extendrange(range(df_nmds$NMDS2[!is_outlier]), f = c(0.1, 0.3))

        # Main plot — zoomed into dense cluster
        plot_beta <- ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, label = site, colour = mean_beta)) +
          geom_point(aes(text = tooltip), size = 3.5, alpha = 0.85) +
          scale_colour_gradientn(name = "Mean \u03b2",
                                 colors = plot_colours) +
          coord_cartesian(xlim = xlim_zoom, ylim = ylim_zoom) +
          labs(x = "NMDS1", y = "NMDS2") +
          theme_minimal() +
          theme(legend.position = "right")
        
        beta_fig <- ggplotly(plot_beta, tooltip = "text")
        main_x_style <- beta_fig$x$layout$xaxis
        main_y_style <- beta_fig$x$layout$yaxis
        
        beta_fig <- beta_fig |>
          add_markers(
            data = df_nmds,
            x = df_nmds$NMDS1,
            y = df_nmds$NMDS2,
            text = df_nmds$tooltip,
            hoverinfo = "text",
            name = "",
            xaxis = 'x2', yaxis = 'y2',
            marker = list(
              color = df_nmds$mean_beta,
              colorscale = lapply(seq_along(plot_colours), function(i) {
                c((i - 1) / (length(plot_colours) - 1),
                  plot_colours[i])
                }),
              cmin = min(df_nmds$mean_beta), cmax = max(df_nmds$mean_beta),
              showscale = FALSE
            ),
            showlegend = FALSE
          ) |>
          layout(
            xaxis = list(tickmode = "auto"),
            yaxis = list(tickmode = "auto"),
            # Position the secondary X-axis (fractional canvas coordinates)
            xaxis2 = list(
              domain = c(0.7, 0.95),
              anchor = "y2",
              fixedrange = TRUE,
              title = NA,
              tickfont = main_x_style$tickfont,
              linecolor = main_x_style$linecolor,
              gridcolor = main_x_style$gridcolor
            ),
            # Position the secondary Y-axis
            yaxis2 = list(
              domain = c(0.7, 0.95),
              anchor = "x2",
              fixedrange = TRUE,
              title = NA,
              tickfont = main_y_style$tickfont,
              linecolor = main_y_style$linecolor,
              gridcolor = main_y_style$gridcolor
            )
          )
        
        beta_fig
        
      } else {
        
        # PCoA via cmdscale
        pcoa_res <- tryCatch(
          vegan::wcmdscale(beta_mat, k = 2, eig = TRUE, add = "cailliez"),
          error = function(e) stop("PCoA failed: ", e$message)
        )
        
        eig     <- pcoa_res$eig
        eig_pos <- pmax(eig, 0)
        pct     <- round(eig_pos[1:2] / sum(eig_pos) * 100, 1)
        
        df_pcoa <- data.frame(
          site = rownames(res$comm.matrix),
          PC1  = pcoa_res$points[, 1],
          PC2  = pcoa_res$points[, 2]
        )
        
        beta_full       <- as.matrix(beta_mat)
        diag(beta_full) <- NA
        df_pcoa$mean_beta <- rowMeans(beta_full, na.rm = TRUE)
        
        df_pcoa$tooltip <- paste0(
          "Site: ", df_pcoa$site,
          "<br>Mean β: ", round(df_pcoa$mean_beta, 3)
        )
        
        plot_beta <- ggplot(df_pcoa, aes(label = site, colour = mean_beta, x = PC1, y = PC2)) +
          geom_hline(yintercept = 0, colour = "#B6CED2", linewidth = 0.4) +
          geom_vline(xintercept = 0, colour = "#B6CED2", linewidth = 0.4) +
          geom_point(aes(text = tooltip), size = 3.5, alpha = 0.85) +
          scale_colour_gradientn(name = "Mean β", colors = colorRampPalette(map_colours)(nrow(df_pcoa))) +
          labs(
            x = paste0("PC1 (", pct[1], "% variance)"),
            y = paste0("PC2 (", pct[2], "% variance)")
          ) +
          theme_minimal() +
          theme(legend.position = "right")
        
        ggplotly(plot_beta, tooltip = "text") |>
          layout(
            xaxis = list(autorange = TRUE),
            yaxis = list(autorange = TRUE)
          )
      }
      
    })
    
    # BIN consensus table
    output$bincons_table <- DT::renderDataTable(DT::datatable({
      droplevels(outdata$bin_consensus)
      },
      filter = 'top', 
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js, 
      options = DT_options),
      server = TRUE)
    
    # BIN reps table
    output$binrep_table <- DT::renderDataTable(DT::datatable({
      droplevels(rep_table())
      },
      filter = 'top',
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js, 
      options = DT_options),
      server = TRUE)
    
    # base map for occurrence data
    output$record_map <- renderLeaflet({
      leaflet() |>
        addTiles(group = "Base") |>
        addProviderTiles("Stadia.StamenTerrainBackground",
                         options = providerTileOptions(noWrap = TRUE),
                         group = "Terrain") |>
        addProviderTiles("Esri.WorldImagery",
                         options = providerTileOptions(noWrap = TRUE),
                         group = "Satellite") |>
        addScaleBar(position = 'topright')%>%
        addLayersControl(baseGroups = c("Base",
                                        "Terrain",
                                        "Satellite"),
                         options = layersControlOptions(collapsed = FALSE))
    })

    # convenience list object to handle download/copy from current tab 
    tab_data <- list(
      data = 
        list(basename = "BOLD_fetch_", 
             output = out_table,
             current_rows = reactive(input$data_table_rows_all),
             copy_btns = list(copy_table = FALSE, copy_fasta = TRUE, copy_reps = FALSE),
             dl_btns = TRUE),
      qhits_tab = 
        list(basename = "query_hit_report_", 
             output = reactive(outdata$hits),
             current_rows = reactive(outdata$hits[, .I]),
             copy_btns = list(copy_table = FALSE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      summary = 
        list(basename = "summary_", 
             output = reactive(outdata$summary),
             current_rows = reactive(input$summary_table_rows_all),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      div_profile = 
        list(basename = "diversity_profile_",
             output = reactive(outdata$div_profile),
             current_rows = reactive(outdata$sdiv_profil[, .I]),
             copy_btns = list(copy_table = FALSE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = FALSE),
      consensus_tab =
        list(basename = "bin_consensus_",
             output = reactive(outdata$bin_consensus),
             current_rows = reactive(input$bincons_table_rows_all),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      bin_reps = 
        list(basename = "BIN-tax_reps_",
             output = rep_table,
             current_rows = reactive(input$binrep_table_rows_all),
             copy_btns = list(copy_table = FALSE, copy_fasta = TRUE, copy_reps = TRUE),
             dl_btns = TRUE),
      map_tab = 
        list(basename = NULL,
             output = NULL,
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = FALSE)
    )
    
    # update button visibility depending on current tab
    observeEvent(input$tabs, {
      btns <- tab_data[[input$tabs]]$copy_btns
      for(i in seq_along(btns)) {
        if(btns[i] == TRUE) shinyjs::show(names(btns)[i]) else shinyjs::hide(names(btns)[i])
      }
      if(tab_data[[input$tabs]]$dl_btns == FALSE) {
        shinyjs::hide('table_buttons')
      } else {
        req(outdata$data)
        shinyjs::show('table_buttons')
      }
    })
    
    # copy button logic
    observeEvent(input$copy_table, { cb(as.data.frame(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ])) })
    observeEvent(input$copy_reps, { cb(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ][["processid"]], header = FALSE) })
    observeEvent(input$copy_fasta, { 
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
          collapsed_data()[processid %in% ids]
        } else if(!is.null(input$filt_seq)) {
          outdata$data[(processid %in% ids) & (marker_code %in% input$filt_seq)]
        } else {
          outdata$data[(processid %in% ids)]
        }
      } else {
        tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
      }
      clip_fasta(data, collapse_mrkrs = collapse_mrkrs)
      })
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
          paste0(tab_data[[input$tabs]]$basename, as.character(format(Sys.Date(),"%Y%m%d")), ".", format)
          },
        content = function(file) {
          data <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
          switch(format,
                 tsv = fwrite(data, file, sep="\t", na="", quote=FALSE, row.names = FALSE),
                 csv = fwrite(data, file, na="", row.names = FALSE),
                 xlsx = write_xlsx(data, file, format_headers=FALSE))
        }
      )}
    
    # open logic
    observeEvent(input$open_xlsx, {
      data <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), ]
      filename <- paste0(tab_data[[input$tabs]]$basename, as.character(format(Sys.Date(),"%Y%m%d")), "_")
      temp <- tempfile(pattern = filename, fileext = ".xlsx")
      write_xlsx(data, temp, format_headers = FALSE)
      browseURL(temp)
    })
    
    # download (save) functions
    output$save_tsv <- download_output("tsv")
    output$save_csv <- download_output("csv")
    output$save_xlsx <- download_output("xlsx")

  }