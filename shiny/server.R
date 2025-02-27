  server <- function(input, output, session) {
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    reset.filter <- function() {
      if (input$tabs == "summary") {nav_select(id="tabs", selected="data")}
      outdf$filt_seq <- NULL
      outdf$fields <- if(is.null(outdf$collapsed)) {
          config$fieldsets$bcdm
        } else {
          c(config$fieldsets$bcdm,outdf$coll_mrkr_fields)
        }
      updateSelectizeInput(session,"filt_opt",
                           choices = filter_options,
                           selected = NULL)
      updateSelectizeInput(session, "filt_seq",
                           choices = outdf$markers,
                           selected = NULL)
    }
    
    binmate.modal <- function() {
      showModal(
        modalDialog(
          title = "Additional BIN members",
          div(HTML(paste0("Found <strong>",nrow(outdf$binmates),"</strong> additional BIN members. Fetch and add to table?"))),
          verbatimTextOutput("binmate_pids"),
          footer = tagList(
            actionButton("copy_binmates", "Copy"),
            div(id="modal_confirm",
                modalButton("No"),
                actionButton("binmate_btn", "Yes"))
            )
        )
      )
    }
    
    observeEvent(input$api_key, {
      try(key_set_with_value("BOLD.apikey", password = input$api_key), silent=TRUE)
    })
  
    # initialize object to store fetched records and associated data
    outdf <- reactiveValues(
      data=NULL,
      binmates=NULL,
      collapsed=NULL,
      coll_mrkr_fields=NULL,
      fields=config$fieldsets$bcdm,
      markers=NULL,
      filt_seq=NULL,
      summary=NULL,
      sumtab=FALSE,
      bin_reps=NULL,
      reptab=FALSE)
    
    # reactive dataframe
    observeEvent(input$fetch_btn, { 
      bold.apikey(input$api_key)
      show('table_area')
      show('table_buttons')
      show('fasta_btn')
      reset.filter()
      updateCheckboxInput(
        session,
        "collapse_mrkrs", 
        value = FALSE)
      outdf$collapsed <- NULL
      outdf$summary <- NULL
      outdf$bin_reps <- NULL
      nav_select(id="tabs", selected="data")
      ids <- str_split(input$fetch_id_list,"\n")[[1]]
      
      tryCatch({
        data <- bold.fetch.plus(get_by = input$fetch_by, 
                        identifiers=ids, 
                        verbatim_cols=TRUE) %>% 
          mutate(id_date_parsed = get.id.date(.))
        outdf$markers <- data %>% distinct(marker_code) %>% replace(is.na(.), "ZZZ") %>% arrange(marker_code) %>% .$marker_code %>% str_replace("ZZZ","None")
        outdf$data <- data
        if (input$fetch_binmates == TRUE) {
          bin_pids <- BOLDconnectR:::get.bin.dataset.project.pids(data.frame(data %>% filter(!is.na(bin_uri)) %>% distinct(bin_uri) %>% .$bin_uri),"bin_uris")
          print(bin_pids)
          outdf$binmates <- setdiff(bin_pids,data %>% rename(records = processid) %>% select(records))
          binmate.modal()
        }
        bslib::accordion_panel_close(id="optpanels", values="fetchdata")
        bslib::accordion_panel_open(id="optpanels", values=c("customize","summarize"))
      }, error = function(e) {
        showNotification(paste0("Error fetching data. Please verify that identifiers match chosen fetch-by parameter. If error persists, please check API key or try again in a few moments."), id="fetch_msg", type = "error")
      })
      
    })
    
    observeEvent(input$binmate_btn, { 
      removeModal()
      binmate_data <- bold.fetch.plus(get_by = "processid", 
                      identifiers=outdf$binmates, 
                      verbatim_cols=TRUE) %>% 
        mutate(id_date_parsed = get.id.date(.))
      #updateCheckboxInput(session,"include_binmates",value=TRUE)
      outdf$data <- outdf$data %>% bind_rows(binmate_data)
    })
    
    #observeEvent(input$include_binmates, {
    #  if(is.null(outdf$binmates)) {
    #    binmate.modal()
    #  }
    #})
    
    observeEvent(input$collapse_mrkrs, {
      if(is.null(outdf$collapsed) & (!is.null(outdf$data))) {
        outdf$collapsed <- collapse.markers(outdf$data)
        outdf$coll_mrkr_fields <- c(outdf$fields,colnames(outdf$collapsed)[!colnames(outdf$collapsed) %in% colnames(outdf$data)])
        outdf$fields <- c(outdf$fields,outdf$coll_mrkr_fields)
      }
    })
    #  collapse.markers(outdf$data)
    #})
    
    observeEvent(input$ana_btn, {
      if(outdf$sumtab == FALSE){
        outdf$sumtab = TRUE
        nav_insert(
          "tabs", target = "data", select = TRUE,
          nav_panel(
            id="summary",
            value="summary",
            "Summary",
            div(id = 'table_area2',
                DT::dataTableOutput("table2") %>% withSpinner(color="#aaaaaa", type=5, size=0.6)),
            actionButton("copy_btn","Copy table",icon=icon("copy"))
            )
        )
      } else {
        nav_select(id="tabs", selected="summary")
      }
      if (input$ana_opt == "tax_summary") {
        outdf$summary <- count.taxa(outdf$data)
      } else {
        outdf$summary <- summarize.table(outdf$data, input$ana_opt)
      }
      
      #dfsum(outdf$summary)
    })
    
    observeEvent(input$bin_rep_btn, {
      if(outdf$reptab == FALSE){
        outdf$reptab = TRUE
        nav_insert(
          "tabs", select = TRUE,
          nav_panel(
            id="bin_reps",
            value="bin_reps",
            "BIN reps",
            div(id = 'table_area3',
                DT::dataTableOutput("table3") %>% withSpinner(color="#aaaaaa", type=5, size=0.6)),
            div(id = 'reps_buttons',
                actionButton("fasta_reps_btn","Copy FASTA",icon=icon("copy")),
                actionButton("copy_reps_btn","Copy process IDs",icon=icon("copy")))
          )
        )
      } else {
        nav_select(id="tabs", selected="bin_reps")
      }
      if (is.null(outdf$bin_reps)) {
        outdf$bin_reps <- get.bin.reps(outdf$data)
      }
    })
    
    observe({
      updateSelectizeInput(session, "filt_seq",
                        choices = outdf$markers,
                        selected = NULL)
      updateSelectInput(session, "ana_opt",
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
                      colnames(outdf$data)[
                        !colnames(outdf$data) %in% c("identification","bin_uri","country.ocean","id_date_parsed")])
                  ))
      
      #outdf$data <- df()
      #outdf$summary <- dfsum()
      #outdf$filt_seq <- outdf$data %>% distinct(marker_code) %>% .$marker_code
      #updateSelectizeInput(session,
      #                     "filt_seq",
      #                     choices = outdf$filt_seq,
      #                     server=TRUE)
    })
    
    observeEvent(input$filter_btn, {
      
      if (length(input$filt_seq) > 0){
        outdf$filt_seq <- input$filt_seq
      } else {
        outdf$filt_seq <- NULL
      }
      
      fields <- if(is.null(outdf$collapsed)) {
        outdf$fields
        } else {
        c(outdf$fields,outdf$coll_mrkr_fields)  
        }
      
      if (length(input$filt_opt) > 0){
        fields <- c()
        for(o in input$filt_opt) {
          fields <- c(fields,config$fieldsets[o][[1]])
        }
        if (any(input$filt_opt %in% c("all","bcdm","sequence"))) {
          fields <- c(fields,outdf$coll_mrkr_fields)
        }
      }
      
      if (input$tabs == "summary") {nav_select(id="tabs", selected="data")}
      
      outdf$fields <- fields
      
    })
    
    observeEvent(input$reset_btn, {
      reset.filter()
    })
    
    out_table <- reactive({
      if (is.null(outdf$data)) {
        NULL
      } else if (input$collapse_mrkrs == TRUE) {
        if ((length(outdf$filt_seq) > 0)) {
          outdf$collapsed %>%
            filter(if_all(unlist(lapply(outdf$filt_seq[!outdf$filt_seq == "None"], paste0, "_nuc")), ~ !is.na(.x))) %>%
            filter(if_all(unlist(lapply(outdf$filt_seq[!outdf$filt_seq == "None"], paste0, "_nuc")), ~ .x != "")) %>% 
            select(any_of(outdf$fields))
        } else {
          outdf$collapsed %>% 
            select(any_of(outdf$fields))
        }
      } else if ((length(outdf$filt_seq) > 0)) {
        outdf$data %>%
          {if ("None" %in% outdf$filt_seq) filter(., marker_code %in% outdf$filt_seq | is.na(marker_code)) else filter(., marker_code %in% outdf$filt_seq)} %>%
          select(any_of(outdf$fields))
      } else {
        outdf$data %>% 
          select(any_of(outdf$fields))
      }
    })
    
    output$binmate_pids <- renderPrint(write.table(outdf$binmates,row.names=FALSE,col.names=FALSE,quote=FALSE))
    
    output$table <- DT::renderDataTable(DT::datatable({
      out_table()
    }, filter='top', rownames= FALSE, selection = 'none', options = list(lengthMenu = list(c(100, 500, 1000, -1), c('100', '500', '1000', 'All'))))) 
    
    sum_table <- reactive({
      outdf$summary
    })
    
    output$table2 <- DT::renderDataTable(DT::datatable({
      sum_table()
    }, rownames=FALSE, options=list(order=list(0,"asc"), searching=FALSE, lengthMenu = list(c(100, 500, 1000, -1), c('100', '500', '1000', 'All'))), selection = 'none'))
    
    rep_table <- reactive({
      outdf$bin_reps %>%
        select(any_of(outdf$fields))
    })
    
    output$table3 <- DT::renderDataTable(DT::datatable({
      rep_table()
    }, filter='top', rownames=FALSE, selection = 'none', options = list(lengthMenu = list(c(100, 500, 1000, -1), c('100', '500', '1000', 'All')))))
    
    observeEvent(input$copy_binmates, {
      cb(outdf$binmates,header=FALSE)
    })
    
    observeEvent(input$copy_btn, {
      cb(as.data.frame(outdf$summary))
    })
    
    observeEvent(input$copy_reps_btn, {
      cb(outdf$bin_reps %>% select(processid), header=FALSE)
    })
    
    observeEvent(input$fasta_btn, {
      clip.fasta(outdf$data,cols_for_fas_names="processid")
    })
    
    observeEvent(input$fasta_reps_btn, {
      clip.fasta(outdf$bin_reps,cols_for_fas_names="processid")
    })
    
    selectedTab <- reactive({
      switch(input$tabs,
             data = {
               list(basename = "BOLD_fetch_",
                    output = out_table())
             },
             summary = {
               list(basename = paste0(input$ana_opt,"_count_"),
                    output = sum_table())
             },
             bin_reps = {
               list(basename = paste0("BIN-tax_reps_"),
                    output = rep_table())
             })
      
    })
    
    output$save_tsv <- downloadHandler(
      filename = function() {paste0(selectedTab()$basename,as.character(format(Sys.Date(),"%Y%m%d")),".tsv")},
      content = function(file) {
        write.table(selectedTab()$output, file, sep="\t", na="", quote=FALSE, row.names = FALSE)
      }
    )
    
    output$save_csv <- downloadHandler(
      filename = function() {paste0(selectedTab()$basename,as.character(format(Sys.Date(),"%Y%m%d")),".csv")},
      content = function(file) {
        write.csv(selectedTab()$output, file, na="", row.names = FALSE)
      }
    )
    
    output$save_xlsx <- downloadHandler(
      filename = function() {paste0(selectedTab()$basename,as.character(format(Sys.Date(),"%Y%m%d")),".xlsx")},
      content = function(file) {
        write_xlsx(selectedTab()$output, file, format_headers=FALSE)
      }
    )
  }