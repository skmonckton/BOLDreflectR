  server <- function(input, output, session) {
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    # some manual JavaScript for placeholder values
    session$onFlushed(function() {
      shinyjs::runjs("$('#seq_min').attr('placeholder', 'Min');
                  $('#seq_max').attr('placeholder', 'Max');")
    }, once = TRUE)
    
    ### SETUP
    
    # initialize object to store search parameters
    fetch_params <- reactiveValues()
    
    init_fetch_params <- list(
      fetch_ids = NULL,
      fetch_by = NULL,
      search_token = NULL,
      binmates_fetched = FALSE
    )
    
    # initialize object to store fetched records and associated data
    outdf <- reactiveValues()
    
    init_outdf <- list(
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
    tab_status <- new.env(parent = emptyenv())
    tab_status$summary <- FALSE
    tab_status$bin_reps <- FALSE
    tab_status$consensus_tab <- FALSE
    tab_status$discordance_tab <- FALSE
    tab_status$map_tab <- FALSE
    
    ### REACTIVE UI
    
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
      value <- input[[input$last_blurred]]
      if(isTRUE(value < 5)) { updateNumericInput(inputId = input$last_blurred, value = 5) }
      if(isTRUE(value > 2000)) { updateNumericInput(inputId = input$last_blurred, value = 2000) }
      if(isTRUE((input$last_blurred == "seq_max") & (input$seq_max < input$seq_min))) { updateNumericInput(inputId = "seq_max", value = input$seq_min) }
      if(isTRUE((input$last_blurred == "seq_min") & (input$seq_min > input$seq_max))) { updateNumericInput(inputId = "seq_min", value = input$seq_max) }
    }, ignoreInit = TRUE)
    
    # function to reset filter options
    reset_filter <- function() {
      if (!isolate(input$tabs) %in% c("data", "bin_reps")) {bslib::nav_select(id="tabs", selected="data")}
      outdf$select_fields <- if (is.null(isolate(outdf$data))) {
        config$fieldsets$bcdm
      } else {
        c(config$fieldsets$bcdm, isolate(coll_mrkr_fields()))
      }
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options(),
                           selected = NULL)
      updateSelectizeInput(session, "filt_seq",
                           choices = outdf$markers,
                           selected = NULL)
    }
    
    
    ### SERVER HELPERS
    
    # function to reset UI (and reactive values) to blank state
    reset_ui <- function() {
      for(i in seq_along(init_outdf)) {
        outdf[[names(init_outdf[i])]] <- unname(unlist(init_outdf[i]))
      }
      for(i in seq_along(init_fetch_params)) {
        fetch_params[[names(init_fetch_params[i])]] <- unname(unlist(init_fetch_params[i]))
      }
      bslib::nav_select(id="tabs", selected = "data")
      for(t in names(tab_data)[names(tab_data) != "data"]) bslib::nav_remove(id="tabs", target = t, session)
      for(t in ls(tab_status)) assign(t, FALSE, envir = tab_status)
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
      try(key_set_with_value("BOLD.apikey", password = input$api_key), silent=TRUE)
      bold.apikey(input$api_key)
      req(isTruthy(input$fetch_id_list) || isTruthy(input$search_tax) || isTruthy(input$search_geo) || isTruthy(input$seq_marker))
      shinyjs::show('table_area')
      shinyjs::hide('table_buttons')
      reset_ui()
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
    })
    
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
      showNotification(paste0("Downloading ids of ",
                              format(search_token$num_of_accessible, big.mark = ",", scientific = FALSE),
                              "  matching records..."), id = "fetch_msg", duration=NULL, type = "message")
      fetch_params$fetch_by <- "processid"
      fetch_params$fetch_ids <- bold.full.search.step2(search_token)$processid
    })
    
    # perform fetch and populate data
    observeEvent(fetch_params$fetch_ids, {
      req(fetch_params$fetch_ids, fetch_params$fetch_by)
      tryCatch({
        data <- as.data.table(bold.fetch.shiny(get_by = fetch_params$fetch_by,
                                 query = fetch_params$fetch_ids,
                                 BCDM_only = FALSE))[, c("id_date_parsed", "project_code", "lat", "lon") := c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdf$id_field)), parse_lat_lon(coord))]
        cols_to_factor <- intersect(config$fieldsets$factorfields, names(data))
        data[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]
        if(fetch_params$fetch_by == "bin_uris") {
          shinyjs::hide("include_binmates")
          shinyjs::hide("view_binmates")
        } else {
          shinyjs::show("include_binmates")
          shinyjs::show("view_binmates")
        }
        outdf$markers <- gsub("ZZZ", "None", sort(unique(c(levels(data$marker_code), if(anyNA(data$marker_code)) "ZZZ"))))
        outdf$data <- data
        shinyjs::show('table_buttons')
        bslib::accordion_panel_close(id="optpanels", values="fetchdata")
        bslib::accordion_panel_open(id="optpanels", values=c("customize","summarize","analyze"))
        if (input$fetch_binmates == TRUE) {
          binmate_modal()
        }
      }, error = function(e) {
        showNotification(paste0("Error fetching data: ", e$message), id="fetch_msg", type = "error")
      })
    })
    
    # logic for initiating search for additional BIN records ("BIN mates")
    # if the BIN mates switch is toggled on, a modal opens to report the number of additional BIN records
    observeEvent(input$include_binmates, {
      if ((input$include_binmates == TRUE) & (fetch_params$binmates_fetched == FALSE)) {
        binmate_modal()
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # the modal can also be accessed without toggling the switch, in case the user simply wants to review the BIN mate list
    observeEvent(input$view_binmates, {
      binmate_modal()
    })
    
    # modal logic for BIN mates
    binmate_modal <- function() {
      req(outdf$data)
      if (is.null(outdf$binmates)) {
        outdf$binmates <- get_binmate_pids(filtered_data()[!is.na(bin_uri)])
      }
      if(length(outdf$binmates) == 0) {
        modal_msg <- div(HTML(paste0("No additional BIN members found.")))
        modal_footer <- tagList(
          div(id="modal_confirm",
              actionButton("binmate_btn", "OK")))
        binmate_list <- div("")
      } else if (fetch_params$binmates_fetched == FALSE) {
        modal_msg <- div(HTML(paste0("Found <strong>",length(outdf$binmates),"</strong> additional BIN members. Fetch and add to table?")))
        modal_footer <- tagList(
          actionButton("copy_binmates", "Copy"),
          div(id="modal_confirm",
              actionButton("binmate_btn", "Yes"),
              actionButton("binmate_no", "No"))
        )
        binmate_list <- verbatimTextOutput("binmate_pids")
      } else {
        modal_msg <- div(HTML(paste0("The following <strong>",length(outdf$binmates),"</strong> additional BIN members were added to the fetched data.")))
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
          easyClose = (fetch_params$binmates_fetched == TRUE)
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
      
      if(length(outdf$binmates) > 0) {
        binmate_data <- as.data.table(bold.fetch.shiny(get_by = "processid", 
                                                       query = outdf$binmates,
                                                       BCDM_only = FALSE))[, c("id_date_parsed", "project_code", "lat", "lon") := c(list(parse_id_date(.SD), list_recordsets(.SD, "parse_project", outdf$id_field)), parse_lat_lon(coord))]
        cols_to_factor <- intersect(config$fieldsets$factorfields, names(binmate_data))
        binmate_data[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]
        outdf$data <- unique(rbindlist(list(outdf$data,
                                            binmate_data),
                                       fill = TRUE))
        bslib::update_switch("include_binmates",label="Include additional BIN members",value=TRUE,session)
      } else {
        bslib::update_switch("include_binmates",label="Include additional BIN members",value=FALSE,session)
      }
      fetch_params$binmates_fetched <- TRUE
      
    })
    
    ### REACTIVE COMPUTATIONS AND OBSERVERS
    
    # keep summary analysis options updated according to available fields
    observeEvent(outdf$data, {
      req(outdf$data)
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
                            names(outdf$data)[
                              !names(outdf$data) %in% c("identification","bin_uri","country.ocean","id_date_parsed","project_code")])
                        ))
    })
    
    # logic for generating marker-collapsed data table
    collapsed_data <- reactive({
      req(outdf$data)
      collapse_markers(outdf$data)
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
        show_copy_fasta <- TRUE
        id_field <- "processid"
      } else {
        # determine which filter selections are field sets from config specification
        sets <- input$filt_opt[(input$filt_opt %in% names(config$fieldsets)) | grepl("|", input$filt_opt, fixed=T)]
        fields <- c()
        # step through selected fields, handling them according to whether they are preset field sets, custom field sets, or individual fields
        for(o in input$filt_opt) {
          if(o %in% sets) {
            opt <- unlist(strsplit(o, "|", fixed=TRUE))
            if(length(opt) > 1) {
              fields <- unique(c(fields, opt[2]))
            } else {
              fields <- unique(c(fields, config$fieldsets[o][[1]]))
            }
          } else if (grepl("^userset", o)) {
            fields <- unique(c(fields, unlist(sapply(config$fieldsets$custom, function(x) if(x$id == o) x$fields))))
          } else {
            fields <- unique(c(fields, o))
          }
        }
        if (any(input$filt_opt %in% c("all","bcdm","sequence"))) {
          fields <- unique(c(fields, coll_mrkr_fields()))
          show_copy_fasta <- TRUE
        } else {
          show_copy_fasta <- FALSE
        }
        id_idx <- which(c("processid", "sampleid") %in% fields)
        if(length(id_idx) == 0) {
          fields <- c("processid", fields)
          id_field <- "processid"
        } else {
          id_field <- c("processid", "sampleid")[min(id_idx)]
        }
      }
      list(fields = fields, show_copy_fasta = show_copy_fasta, id_field = id_field)
    })
    
    # consume computed filter fields and record them in the reactive value list
    observeEvent(computed_fields(), {
      result <- computed_fields()
      outdf$select_fields <- result$fields
      outdf$id_field <- result$id_field
      if (result$show_copy_fasta) shinyjs::show("copy_fasta") else shinyjs::hide("copy_fasta")
    })
    
    # modal logic for saving custom field sets
    observeEvent(input$save_filter_set, {
      showModal(
        modalDialog(
          title = "Save column set",
          textInput("save_filter_name", "Save as:", width = "100%"),
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
      removeModal()
      filter_set <- save_custom_fieldset(input$save_filter_name, outdf$select_fields)
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options(),
                           selected = filter_set)
    })
    
    # keep marker filters updated based on available data
    observeEvent(outdf$markers, {
      updateSelectizeInput(session, "filt_seq", choices = outdf$markers, selected = NULL)
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
        keep_seq[[3]] <- Reduce("|", data[, lapply(.SD, function(x) !is.na(x) & x != ""), .SDcols = nuc_cols])
      }
      
      if(length(keep_seq) > 0) {
        return(Reduce("|", keep_seq[!sapply(keep_seq, is.null)]))
      } else {
        return(TRUE)
      }
    }
    
    # reactive computation of filtered data table
    filtered_data <- reactive({
      req(outdf$data)
      data <- if (input$collapse_mrkrs == TRUE) {
        cd <- collapsed_data()
        req(cd)
        cd
      } else {
        outdf$data
      }
      
      if (!isTRUE(input$include_binmates) || length(outdf$binmates) == 0) {
        data <- data[!processid %in% outdf$binmates]
      }
      
      if(!isTRUE(input$include_nts)) {
        data <- data[!grepl("NTS(?:_[A-Za-z]+)?_OF:", associated_specimens, perl = TRUE) &
                       !grepl("^BIOUG.+?\\.NTS[0-9]*?$", sampleid, perl = TRUE)]
      }
      
      data[seq_filter(data, input$filt_seq)]
      
    })
    
    # compute summary counts and inject as a new tab
    observeEvent(input$ana_btn, {
      req(outdf$data)
      outdf$summary <- NULL
      outdf$summary <- if(input$ana_opt == "tax_summary") {
        count_taxa(filtered_data(), outdf$id_field)
      } else {
        summarize_table(filtered_data(), input$ana_opt, outdf$id_field)
      }
      if(!tab_status$summary) {
        bslib::nav_insert(
          "tabs", target = "data", select = TRUE,
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
      if(!is.null(outdf$bin_consensus) & !is.null(outdf$bin_discordance)) {
        first_cols <- c("bin_uri", "record_count", "min_rank", "max_rank")
        consensus_cols <- c("concordant_rank", "concordant_id", "discordant_rank", "discordant_ids")
        rest_cols <- setdiff(names(outdf$bin_discordance), c(first_cols, consensus_cols))
        updated_disc <- merge(outdf$bin_discordance, outdf$bin_consensus[, .SD, .SDcols = c("bin_uri", consensus_cols)], all.x = TRUE, by = "bin_uri")
        outdf$bin_discordance <- updated_disc[, .SD, .SDcols = intersect(c(first_cols, consensus_cols, rest_cols), names(updated_disc))]
      }
    }
    
    # compute BIN consensus and inject as a new tab
    observeEvent(input$bin_consensus_btn, {
      req(outdf$data)
      outdf$bin_consensus <- NULL
      outdf$bin_consensus <- get_bin_consensus(filtered_data()[!is.na(bin_uri)],
                                               threshold = as.double(unlist(unname(input$bc_threshold))),
                                               min_ids = unlist(unname(input$bc_minids)),
                                               enforce_scientific = input$bc_enforcesci,
                                               discord_format = "text")
      
      if(!tab_status$consensus_tab) {
        bslib::nav_insert(
          "tabs", select = TRUE,
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
      req(outdf$data)
      outdf$bin_discordance <- NULL
      outdf$bin_discordance <- get_bin_discordance(filtered_data()[!is.na(bin_uri)])
      
      if(!tab_status$discordance_tab) {
        bslib::nav_insert(
          "tabs", select = TRUE,
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
    observeEvent(outdf$bin_discordance, {
      req(outdf$bin_discordance)
      delay(1000, {
        if(input$disc_portal == TRUE && is.null(outdf$bin_portal_stats)) {
          outdf$bin_portal_stats <- get_portal_bin_stats(as.character(isolate(outdf$bin_discordance$bin_uri)), shiny = TRUE)
          outdf$bin_discordance <- merge(isolate(outdf$bin_discordance),
                                         isolate(outdf$bin_portal_stats),
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
    observeEvent(outdf$bin_portal_stats, {
      portal_stats <- outdf$bin_portal_stats
      req(portal_stats)
      if("member_count" %in% names(portal_stats)) {
        insert_portal_warning(portal_stats)
      }
    })
    
    # logic for retrying portal BIN query
    observeEvent(input$disc_portal_retry, {
      init_stats <- outdf$bin_portal_stats
      req(init_stats)
      retry_stats <- get_portal_bin_stats(as.character(init_stats[is.na(member_count), bin_uri]), shiny = TRUE)
      init_stats[retry_stats, (names(init_stats)) := mget(paste0("i.", names(init_stats))), on = "bin_uri"]
      disc <- isolate(outdf$bin_discordance)
      outdf$bin_discordance <- merge(disc[, .SD, .SDcols = c("bin_uri",names(disc)[!names(disc) %in% names(init_stats)])],
                                     init_stats,
                                     by = "bin_uri",
                                     all.x = TRUE)
      bslib::nav_select(id="tabs", selected="discordance_tab")
      outdf$bin_portal_stats <- init_stats
      insert_portal_warning(init_stats)
    })
    
    
    # select BIN reps and inject as a new tab
    observeEvent(input$bin_rep_btn, {
      req(outdf$data)
      outdf$bin_reps <- NULL
      outdf$bin_reps <- get_bin_reps(filtered_data()[!is.na(bin_uri)])
      
      if(!tab_status$bin_reps) {
        bslib::nav_insert(
          "tabs", select = TRUE,
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
    
    # plot records on a map and display in a new tab
    observeEvent(input$map_btn, {
      req(outdf$data)
      if(!tab_status$map_tab) {
        bslib::nav_insert(
          "tabs", select = TRUE,
          bslib::nav_panel(
            id="map_tab",
            value="map_tab",
            "Occurrence map",
            div(id = 'map_area',
                leafletOutput("record_map"))
          )
        )
        tab_status$map_tab <- TRUE
      } else {
        bslib::nav_select(id = "tabs", selected = "map_tab")
      } 
    })
    
    # reactively subset the filtered data table based on selected fields
    out_table <- reactive({
      data <- filtered_data()
      data[, .SD, .SDcols = intersect(outdf$select_fields, names(data))]
    })
    
    # BIN reps are also updated reactively, using the same filters and field selections as the full data table
    rep_table <- reactive({
      req(outdf$data, outdf$bin_reps)
      outdf$data[record_id %in% outdf$bin_reps,
                 .SD,
                 .SDcols = intersect(outdf$select_fields, names(outdf$data))]
    })
    
    # all filtered rows, any page:
    # out_table()[input$TABLEID_rows_all, ]
    # current page rows:
    # out_table()[input$TABLEID_rows_current, ]
    # selected rows:
    # out_table()[input$TABLEID_rows_selected, ]
    
    ### OUTPUT RENDERING
    
    # build a reactive JSON lookup map (and corresponding callback JS) to support hyperlinking of sample IDs and process IDs
    lookup_map_json <- reactive({
      jsonlite::toJSON(outdf$data[, .(sampleid, processid, specimenid)], auto_unbox = TRUE)
    })
    
    callback_js <- reactive({
      DT::JS(paste0(
        "var rawMap = ", lookup_map_json(), ";
        window.mapBySID = {};
        window.mapByPID = {};
        rawMap.forEach(function(row) {
          window.mapBySID[row.sampleid] = row.processid;
          window.mapByPID[row.processid] = row.specimenid;
        });
        "
      ))
    })
    
    # define options once, then add them to each table individually
    DT_extensions <- c("FixedHeader", "FixedColumns")
    DT_options <- list(headerCallback = header_js,
                   columnDefs = list(
                     list(
                       targets = "_all",
                       render = column_js)),
                   # scrollX = TRUE,
                   # scrollY = "900px",
                   scrollCollapse = TRUE,
                   lengthMenu = list(c(100, 500, 1000, -1), c('100', '500', '1000', 'All')),
                   fixedHeader = TRUE,
                   fixedColumns = list(leftColumns = 1),
                   initComplete = DT::JS("function(settings, json) {
                          var dt = this.api();
                          $(document).on('shown.bs.tab', function() {
                            dt.columns.adjust();
                          });
                        }"))
    
    # generate binmate list for display
    output$binmate_pids <- renderPrint(writeLines(outdf$binmates))
    
    # main data table
    output$data_table <- DT::renderDataTable(DT::datatable({
      out_table()
      },
      filter = 'top',
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js(),
      options = DT_options)) 
    
    # summary analysis table
    output$summary_table <- DT::renderDataTable(DT::datatable({
        outdf$summary
      },
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js(),
      options = DT_options))
    
    # BIN consensus table
    output$bincons_table <- DT::renderDataTable(DT::datatable({
      outdf$bin_consensus
      },
      filter = 'top', 
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js(), 
      options = DT_options))
    
    # BIN discordance table
    output$bindisc_table <- DT::renderDataTable(DT::datatable({
      outdf$bin_discordance
      },
      filter = 'top', 
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js(), 
      options = DT_options))
    
    # BIN reps table
    output$binrep_table <- DT::renderDataTable(DT::datatable({
      rep_table()
      },
      filter = 'top',
      rownames = FALSE,
      selection = 'none',
      extensions = DT_extensions,
      callback = callback_js(), 
      options = DT_options))
    
    output$record_map <- renderLeaflet({
      
      #colors <- c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
      colors <- c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#ffffbf","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2")
      pal <- colorFactor(palette = colorRampPalette(colors, alpha = TRUE)(20), 
                         domain = isolate(filtered_data()$identification))
      
      leaflet(data = filtered_data()[input$data_table_rows_all, ]) |> 
        addTiles(group = "Base") |>
        addProviderTiles("Stadia.StamenTerrainBackground",
                         options = providerTileOptions(noWrap = TRUE),
                         group = "Terrain") |>
        addProviderTiles("Esri.WorldImagery",
                         options = providerTileOptions(noWrap = TRUE), 
                         group = "Satellite") |>
        addCircleMarkers(lat = ~lat,
                   lng = ~lon,
                   color = "#1D1F21",
                   fillColor = ~pal(identification),
                   weight = 2,
                   radius = 9,
                   opacity = 0.3,
                   fillOpacity = 0.7,
                   popup = ~paste0(
                     '<span class="tag">Sample ID:</span><span class="value"><a href="https://bench.boldsystems.org/index.php/MAS_DataRetrieval_OpenSpecimen?selectedrecordid=', processid, '" target="_blank">', sampleid,'</a></span>',
                     '<span class="tag">Process ID:</span><span class="value">', processid, '</span>',
                     '<span class="tag">Identification:</span><span class="value">', identification,'</span>',
                     '<span class="tag">Country:</span><span class="value">', country.ocean, '</span>',
                     '<span class="tag">Province/State:</span><span class="value">', province.state, '</span>',
                     '<span class="tag">Region:</span><span class="value">', region, '</span>',
                     '<span class="tag">Sector:</span><span class="value">', sector, '</span>',
                     '<span class="tag">Site:</span><span class="value">', site, '</span>'),
                   clusterOptions = markerClusterOptions())|>
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
             copy_btns = list(copy_table = FALSE, copy_fasta = TRUE, copy_reps = FALSE),
             dl_btns = TRUE),
      summary = 
        list(basename = "summary_", 
             output = reactive(outdf$summary),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      bin_reps = 
        list(basename = "BIN-tax_reps_",
             output = rep_table,
             copy_btns = list(copy_table = FALSE, copy_fasta = TRUE, copy_reps = TRUE),
             dl_btns = TRUE),
      consensus_tab =
        list(basename = "bin_consensus_",
             output = reactive(outdf$bin_consensus),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
             dl_btns = TRUE),
      discordance_tab =
        list(basename = "bin_discordance_",
             output = reactive(outdf$bin_discordance),
             copy_btns = list(copy_table = TRUE, copy_fasta = FALSE, copy_reps = FALSE),
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
        req(outdf$data)
        shinyjs::show('table_buttons')
      }
    })
    
    # copy button logic
    observeEvent(input$copy_table, { cb(as.data.frame(tab_data[[input$tabs]]$output())) })
    observeEvent(input$copy_reps, { cb(tab_data[[input$tabs]]$output()[["processid"]], header = FALSE) })
    observeEvent(input$copy_fasta, { 
      collapse_mrkrs <- if(input$collapse_mrkrs == TRUE) { coll_mrkr_fields()[grepl(paste0("^",paste(input$filt_seq, collapse="|")), coll_mrkr_fields())] } else { NULL }
      clip_fasta(filtered_data(), collapse_mrkrs = collapse_mrkrs) })
    observeEvent(input$copy_binmates, { cb(outdf$binmates, header = FALSE) })
    
    # column copy logic
    observeEvent(input$col_copy_clicked, {
      col_index <- input$col_copy_clicked$col  # 0-based index from JS
      cb(tab_data[[input$tabs]]$output()[[col_index + 1]], header = FALSE)
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