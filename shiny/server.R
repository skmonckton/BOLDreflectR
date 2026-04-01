  server <- function(input, output, session) {
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    # some manual JavaScript for placeholder values
    # session$onFlushed(function() {
    #   shinyjs::runjs("$('#seq_min').attr('placeholder', 'Min');
    #                   $('#seq_max').attr('placeholder', 'Max');")
    # }, once = TRUE)
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
          Shiny.setInputValue('local_markers', vals, {priority: 'event'});
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
    fetch_params <- reactiveValues(
      n_node_markers = 1
    )
    
    init_fetch_params <- list(
      fetch_ids = NULL,
      fetch_by = NULL,
      search_token = NULL,
      binmates_checked = FALSE,
      binmates_fetched = FALSE,
      n_node_markers = 1
    )
    
    # initialize object to store fetched records and associated data
    outdata <- reactiveValues()
    
    init_outdata <- list(
      data = NULL,
      binmates = NULL,
      select_fields = config$fieldsets$bcdm,
      id_field = "processid",
      markers = NULL,
      filt_seq = NULL,
      filt_seq_idx = NULL,
      summary = NULL,
      bin_reps = NULL,
      bin_consensus = NULL,
      bin_discordance = NULL,
      bin_portal_stats = NULL)
    
    # keep track of output tabs
    # tab_status <- new.env(parent = emptyenv())
    # tab_status$summary <- FALSE
    # tab_status$bin_reps <- FALSE
    # tab_status$consensus_tab <- FALSE
    # tab_status$discordance_tab <- FALSE
    # tab_status$map_tab <- FALSE
    
    tab_status <- reactiveValues(
      data = FALSE,
      summary = FALSE,
      bin_reps = FALSE,
      consensus_tab = FALSE,
      discordance_tab = FALSE,
      map_tab = FALSE
    )
    
    # convenience function for interrogating current tab status
    tab_monitor <- function(get = c("status", "current", "absent", "ins_target"), ins_tab = NULL) {
      tabs <- isolate(reactiveValuesToList(tab_status))
      switch(get,
             status = tabs,
             current = names(tabs)[tabs == TRUE],
             absent = names(tabs)[tabs == FALSE],
             ins_target = names(tabs)[max(which(tabs[1:match(ins_tab, names(tabs))] == TRUE))])
    }
    
    ### REACTIVE UI
    
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
    
    # conditional input logic to constrain marker search options when using API (min/max requires a marker to be selected)
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
    
    # logic for multiple markers when searching local data
    observeEvent(input$node_add_marker, {
      i <- fetch_params$n_node_markers
      n <- i + 1
      fetch_params$n_node_markers <- n
      insertUI(selector = '.tab-pane[data-value="source-local"] div.marker-select div.single-marker:nth-last-child(1 of .single-marker)',
               where = "afterEnd",
               ui = div(class="form-group shiny-input-container single-marker",
                        selectInput(
                          paste0("node_seq_marker_",n),
                          label = "",
                          selected = "",
                          marker_options),
                        numericInput(
                          paste0("node_seq_max_",n),
                          label = "",
                          value = NULL,
                          min = 5, max = 2000, step = 1),
                        numericInput(
                          paste0("node_seq_max_",n),
                          label = "",
                          value = NULL,
                          min = 5, max = 2000, step = 1)))
    })
    
    # remove marker from search
    observeEvent(input$node_del_marker, {
      i <- fetch_params$n_node_markers
      req(i > 1)
      n <- i - 1
      fetch_params$n_node_markers <- n
      shinyjs::runjs(paste0("Shiny.setInputValue(\"node_seq_marker_", i, "\", null);"))
      removeUI(selector = '.tab-pane[data-value="source-local"] div.marker-select div.single-marker:nth-last-child(1 of .single-marker)',
               immediate = TRUE)
    })
    
    # update select inputs to permit only one input per marker
    observeEvent(input$local_markers, {
      selected <- input$local_markers
      for(i in 1:isolate(fetch_params$n_node_markers)) {
        marker_i <- input[[paste0("node_seq_marker_", i)]]
        update_opts <- lapply(marker_options, function(m) m[!m %in% c(setdiff(selected, marker_i))])
        updateSelectInput(session,
                          paste0("node_seq_marker_", i),
                          choices = update_opts,
                          selected = ifelse(is.null(marker_i), "", marker_i))
      }
    })
    
    # automatically update min/max marker length to avoid impossible ranges
    observeEvent(input$last_blurred, {
      value <- input[[input$last_blurred]]
      if(isTRUE(value < 5)) { updateNumericInput(inputId = input$last_blurred, value = 5) }
      if(isTRUE(value > 2000)) { updateNumericInput(inputId = input$last_blurred, value = 2000) }
      if(isTRUE((input$last_blurred == "seq_max") & (input$seq_max < input$seq_min))) { updateNumericInput(inputId = "seq_max", value = input$seq_min) }
      if(isTRUE((input$last_blurred == "seq_min") & (input$seq_min > input$seq_max))) { updateNumericInput(inputId = "seq_min", value = input$seq_max) }
    }, ignoreInit = TRUE)
    
    # function to reset filter options
    reset_filter <- function() {
      if (!isolate(input$tabs) %in% c("data", "bin_reps", "map_tab")) {bslib::nav_select(id="tabs", selected="data")}
      outdata$select_fields <- if (is.null(isolate(outdata$data))) {
        config$fieldsets$bcdm
      } else {
        c(config$fieldsets$bcdm, isolate(coll_mrkr_fields()))
      }
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options(),
                           selected = NULL)
      updateSelectizeInput(session, "filt_seq",
                           choices = outdata$markers,
                           selected = NULL)
    }
    
    
    ### SERVER HELPERS
    
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
      for(i in seq_along(tab_status)) tab_status[[names(tab_status)[i]]] <- FALSE
      #for(t in ls(tab_status)) assign(t, FALSE, envir = tab_status)
      reset_filter()
      bslib::update_switch("include_binmates", label = "Include additional BIN members", value = FALSE, session)
      bslib::update_switch("include_nts", label = "Include NTS records", value = FALSE, session)
      bslib::update_switch("collapse_mrkrs", label = "Collapse multiple markers into single rows", value = FALSE, session)
      updateCheckboxInput(inputId = "disc_portal", value = FALSE)
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
      req(isTruthy(input$fetch_id_list) || isTruthy(input$search_tax) || isTruthy(input$search_geo) || 
            isTruthy(input$seq_marker) || isTruthy(Sys.getenv("api_key")))
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
                               marker_max_length = input$seq_max[!is.na(input$seq_max)])
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
          fetch_params$fetch_by <- input$fetch_by
          fetch_params$fetch_ids <- split_query(input$fetch_id_list)
        }
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
    
    # perform fetch and populate data
    observeEvent(fetch_params$fetch_ids, {
      req(fetch_params$fetch_ids, fetch_params$fetch_by)
      data <- as.data.table(bold.fetch.shiny(get_by = fetch_params$fetch_by,
                               query = fetch_params$fetch_ids,
                               BCDM_only = FALSE))
      req(nrow(data) > 0)
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
    })
    
    # logic for initiating search (or fetch) of additional BIN records ("BIN mates") 
    # if the BIN mates switch is toggled on, a modal opens to report the number of additional BIN records
    observeEvent(input$include_binmates, {
      req(outdata$data)
      if (input$include_binmates == TRUE) {
        if(fetch_params$binmates_checked == FALSE) {
          binmate_modal()  
        } else if(fetch_params$binmates_fetched == FALSE) {
          binmate_data <- as.data.table(bold.fetch.shiny(get_by = "processid", 
                                                         query = outdata$binmates,
                                                         BCDM_only = FALSE))[, c("id_date_parsed", "project_code", "lat", "lon") := c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdata$id_field)), parse_lat_lon(coord))]
          cols_to_factor <- intersect(config$fieldsets$factorfields, names(binmate_data))
          binmate_data[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]
          data <- unique(rbindlist(list(outdata$data,
                                        binmate_data),
                                   fill = TRUE))
          outdata$markers <- gsub("ZZZ", "None", sort(unique(c(levels(data$marker_code), if(anyNA(data$marker_code)) "ZZZ"))))
          outdata$data <- data
          fetch_params$binmates_fetched <- TRUE
        }
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
        binmate_pids <- get_binmate_pids(outdata$data[!is.na(bin_uri)])
        outdata$binmates <- binmate_pids
      }
      if(length(binmate_pids) == 0) {
        modal_msg <- div(HTML(paste0("No additional BIN members found.")))
        modal_footer <- tagList(
          div(id="modal_confirm",
              actionButton("binmate_btn", "OK")))
        binmate_list <- div("")
      } else if (fetch_params$binmates_checked == FALSE) {
        modal_msg <- div(HTML(paste0("Found <strong>",length(binmate_pids),"</strong> additional BIN members. Fetch and add to table?")))
        modal_footer <- tagList(
          actionButton("copy_binmates", "Copy"),
          div(id="modal_confirm",
              actionButton("binmate_btn", "Yes"),
              actionButton("binmate_no", "No"))
        )
        binmate_list <- verbatimTextOutput("binmate_pids")
      } else {
        modal_msg <- div(HTML(paste0("The following <strong>",length(binmate_pids),"</strong> additional BIN members were added to the fetched data.")))
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
        bslib::update_switch("include_binmates",label="Include additional BIN members",value=TRUE,session)
      } else {
        bslib::update_switch("include_binmates",label="Include additional BIN members",value=FALSE,session)
      }
      fetch_params$binmates_checked <- TRUE
      
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
              fields <- unique(c(fields, opt[2]))
            } else {
              fields <- unique(c(fields, config$fieldsets[o][[1]]))
            }
          # } else if (grepl("^userset", o)) {
          #   fields <- unique(c(fields, unlist(sapply(config$fieldsets$custom, function(x) if(x$id == o) x$fields))))
          } else {
            fields <- unique(c(fields, o))
          }
        }
        
        # if (any(input$filt_opt %in% c("all","bcdm","sequence"))) {
        #   fields <- unique(c(fields, coll_mrkr_fields()))
        #   show_copy_fasta <- TRUE
        # } else {
        #   show_copy_fasta <- FALSE
        # }
        id_idx <- which(c("processid", "sampleid") %in% fields)
        if(length(id_idx) == 0) {
          fields <- c("processid", fields)
          id_field <- "processid"
        } else {
          id_field <- c("processid", "sampleid")[min(id_idx)]
        }
      }
      # list(fields = fields, show_copy_fasta = show_copy_fasta, id_field = id_field)
      list(fields = fields, id_field = id_field)
    })
    
    # consume computed filter fields and record them in the reactive value list
    observeEvent(computed_fields(), {
      result <- computed_fields()
      outdata$select_fields <- result$fields
      outdata$id_field <- result$id_field
      #if (result$show_copy_fasta) shinyjs::show("copy_fasta") else shinyjs::hide("copy_fasta")
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
          keep_seq[[2]] <- (data[["marker_code"]] == "") | is.na(data[["marker_code"]])  
        }
      }
      if(length(nuc_cols) > 0) {
        #keep_seq[[3]] <- Reduce("|", data[, lapply(.SD, function(x) !is.na(x) & x != ""), .SDcols = nuc_cols])
        keep_seq[[3]] <- rowSums(data[, lapply(.SD, function(x) !is.na(x) & x != ""), .SDcols = nuc_cols]) > 0
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
      req(outdata$data)
      outdata$summary <- NULL
      outdata$summary <- if(input$ana_opt == "tax_summary") {
        count_taxa(filtered_data(), outdata$id_field)
      } else {
        summarize_table(filtered_data(), input$ana_opt, outdata$id_field)
      }
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
    
    # insert BIN consensus details into BIN discordance table if/when available
    update_discordance <- function(){
      if(!is.null(outdata$bin_consensus) & !is.null(outdata$bin_discordance)) {
        first_cols <- c("bin_uri", "record_count", "min_rank", "max_rank")
        consensus_cols <- c("concordant_rank", "concordant_id", "discordant_rank", "discordant_ids")
        rest_cols <- setdiff(names(outdata$bin_discordance), c(first_cols, consensus_cols))
        updated_disc <- merge(outdata$bin_discordance, outdata$bin_consensus[, .SD, .SDcols = c("bin_uri", consensus_cols)], all.x = TRUE, by = "bin_uri")
        outdata$bin_discordance <- updated_disc[, .SD, .SDcols = intersect(c(first_cols, consensus_cols, rest_cols), names(updated_disc))]
      }
    }
    
    # compute BIN consensus and inject as a new tab
    observeEvent(input$bin_consensus_btn, {
      req(outdata$data)
      outdata$bin_consensus <- NULL
      outdata$bin_consensus <- get_bin_consensus(filtered_data()[!is.na(bin_uri)],
                                               threshold = as.double(unlist(unname(input$bc_threshold))),
                                               min_ids = unlist(unname(input$bc_minids)),
                                               enforce_scientific = input$bc_enforcesci,
                                               discord_format = "text")
      
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
      
      update_discordance()
    })
    
    # compute BIN discordance and inject as a new tab
    observeEvent(input$bin_disc_btn, {
      req(outdata$data)
      outdata$bin_discordance <- NULL
      outdata$bin_discordance <- get_bin_discordance(filtered_data()[!is.na(bin_uri)])
      
      if(!tab_status$discordance_tab) {
        bslib::nav_insert(
          "tabs", target = tab_monitor("ins_target","discordance_tab"), position = "after", select = TRUE,
          bslib::nav_panel(
            id="discordance_tab",
            value="discordance_tab",
            "BIN discordance",
            div(id = 'disc_table_area',
                DT::dataTableOutput("bindisc_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6))
          )
        )
        tab_status$discordance_tab <- TRUE
      } else {
        bslib::nav_select(id = "tabs", selected = "discordance_tab")
      }
      update_discordance()
    })
    
    # portal stats can take a while to retrieve, so this is in a separate reactive expression to allow the UI to update with the initial BIN discordance table
    # (gives you something to look at while you wait)
    observeEvent(outdata$bin_discordance, {
      req(outdata$bin_discordance)
      delay(1000, {
        if(input$disc_portal == TRUE && is.null(outdata$bin_portal_stats)) {
          outdata$bin_portal_stats <- get_portal_bin_stats(as.character(isolate(outdata$bin_discordance$bin_uri)), shiny = TRUE)
          outdata$bin_discordance <- merge(isolate(outdata$bin_discordance),
                                         isolate(outdata$bin_portal_stats),
                                         by = "bin_uri",
                                         all.x = TRUE)
          bslib::nav_select(id="tabs", selected="discordance_tab")
        }
      })
    })
    
    # the portal API can be somewhat inconsistent; this will insert a warning message about any BINs for which the query timed out
    insert_portal_warning <- function(portal_stats) {
      removeUI("#disc_portal_warning", immediate = TRUE)
      failed <- nrow(portal_stats[is.na(member_count)])
      if(failed > 0) {
        insertUI(
          selector = "div.form-group:has(#disc_portal):not(:has(div.form-group:has(#disc_portal)))",
          where = "beforeEnd",
          ui = div(id = "disc_portal_warning",
                   class="control-label warn-text",
                   HTML(paste0("Portal stats timed out for ", failed, " BINs. ")),
                   actionLink("disc_portal_retry", "Click to retry."))
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
    observeEvent(input$disc_portal_retry, {
      init_stats <- outdata$bin_portal_stats
      req(init_stats)
      retry_stats <- get_portal_bin_stats(as.character(init_stats[is.na(member_count), bin_uri]), shiny = TRUE)
      init_stats[retry_stats, (names(init_stats)) := mget(paste0("i.", names(init_stats))), on = "bin_uri"]
      disc <- isolate(outdata$bin_discordance)
      outdata$bin_discordance <- merge(disc[, .SD, .SDcols = c("bin_uri",names(disc)[!names(disc) %in% names(init_stats)])],
                                     init_stats,
                                     by = "bin_uri",
                                     all.x = TRUE)
      bslib::nav_select(id="tabs", selected="discordance_tab")
      outdata$bin_portal_stats <- init_stats
      insert_portal_warning(init_stats)
    })
    
    # select BIN reps and inject as a new tab
    observeEvent(input$bin_rep_btn, {
      req(outdata$data)
      outdata$bin_reps <- NULL
      outdata$bin_reps <- get_bin_reps(filtered_data()[!is.na(bin_uri)])
      
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
      req(outdata$data)
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
      data <- filtered_data()[input$data_table_rows_all, ]
      data <- unique(data[!is.na(lat) & !is.na(lon) & lat != "" & lon != ""], by = "processid")
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
    DT_extensions <- c("FixedHeader", "FixedColumns")
    DT_options <- list(headerCallback = header_js,
                   columnDefs = list(
                     list(
                       targets = "_all",
                       render = column_js)),
                   scrollCollapse = TRUE,
                   lengthMenu = list(c(100, 500, 1000, 2000), c('100', '500', '1000', '2000')),
                   fixedHeader = TRUE,
                   fixedColumns = list(leftColumns = 1),
                   stateSave = FALSE,
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
      out_table()
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
        outdata$summary
      },
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js,
      options = DT_options),
      server = TRUE)
    
    # BIN consensus table
    output$bincons_table <- DT::renderDataTable(DT::datatable({
      outdata$bin_consensus
      },
      filter = 'top', 
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js, 
      options = DT_options),
      server = TRUE)
    
    # BIN discordance table
    output$bindisc_table <- DT::renderDataTable(DT::datatable({
      outdata$bin_discordance
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
      rep_table()
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
      summary = 
        list(basename = "summary_", 
             output = reactive(outdata$summary),
             current_rows = reactive(input$summary_table_rows_all),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      consensus_tab =
        list(basename = "bin_consensus_",
             output = reactive(outdata$bin_consensus),
             current_rows = reactive(input$bincons_table_rows_all),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      discordance_tab =
        list(basename = "bin_discordance_",
             output = reactive(outdata$bin_discordance),
             current_rows = reactive(input$bindisc_table_rows_all),
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
    observeEvent(input$copy_table, { cb(as.data.frame(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows()])) })
    observeEvent(input$copy_reps, { cb(tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows()][["processid"]], header = FALSE) })
    observeEvent(input$copy_fasta, { 
      #collapse_mrkrs <- if(input$collapse_mrkrs == TRUE) { coll_mrkr_fields()[grepl(paste0("^",paste(input$filt_seq, collapse="|")), coll_mrkr_fields())] } else { NULL }
      collapse_mrkrs <- if(input$collapse_mrkrs == TRUE) { 
        coll_fields <- coll_mrkr_fields()[grepl(paste0("^",paste(input$filt_seq, collapse="|")), coll_mrkr_fields())]
        coll_fields[grepl("_nuc$", coll_fields)]
      } else {
        NULL
      }
      
      data <- if(input$tabs == "data") {
        fetch_by <- fetch_params$fetch_by
        ids <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows(), get(fetch_by)]
        if(input$collapse_mrkrs == TRUE) {
          collapsed_data()[get(fetch_by) %in% ids]
        } else if(!is.null(input$filt_seq)) {
          outdata$data[(get(fetch_by) %in% ids) & (marker_code %in% input$filt_seq)]
        } else {
          outdata$data[(get(fetch_by) %in% ids)]
        }
      } else {
        tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows()]
      }
      clip_fasta(data, collapse_mrkrs = collapse_mrkrs)
      })
    observeEvent(input$copy_binmates, { cb(outdata$binmates, header = FALSE) })
    
    # column copy logic
    observeEvent(input$col_copy_clicked, {
      col_index <- input$col_copy_clicked$col  # JS uses 0-based index
      vals <- tab_data[[input$tabs]]$output()[tab_data[[input$tabs]]$current_rows()][[col_index + 1]]
      vals[vals == "NA"] <- ""
      cb(vals, header = FALSE)
    })
    
    # download (save) logic
    download_output <- function(format) { downloadHandler(
      filename = function() {
        paste0(tab_data[[input$tabs]]$basename, as.character(format(Sys.Date(),"%Y%m%d")), ".", format)
        },
      content = function(file) {
        switch(format,
               tsv = fwrite(tab_data[[input$tabs]]$output(), file, sep="\t", na="", quote=FALSE, row.names = FALSE),
               csv = fwrite(tab_data[[input$tabs]]$output(), file, na="", row.names = FALSE),
               xlsx = write_xlsx(tab_data[[input$tabs]]$output(), file, format_headers=FALSE))
        }
      )}
    
    # download (save) functions
    output$save_tsv <- download_output("tsv")
    output$save_csv <- download_output("csv")
    output$save_xlsx <- download_output("xlsx")
    
  }