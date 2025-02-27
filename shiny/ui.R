ui <- page_fillable(
  title = "BOLDreflectR",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      div(id="side-title",span(img(src = "reflectr.png", height = 35)),h2("BOLDreflectR")),
      bslib::accordion(
        id="optpanels",
        bslib::accordion_panel(
          value="fetchdata",
          "Fetch data",
          icon = bsicons::bs_icon("cloud-arrow-down"),
          passwordInput( 
            "api_key", 
            "BOLD API key:",
            value=tryCatch({
              key_get("BOLD.apikey")
            }, error=function(e){
              ""
            })
          ), 
          selectInput("fetch_by",
                      "Parameter to fetch by:",
                      list("Process ID" = "processid",
                           "Sample ID" = "sampleid",
                           "BIN" = "bin_uris",
                           "Dataset codes" = "dataset_codes",
                           "Project codes" = "project_codes"
                      )
          ),
          textAreaInput( 
            "fetch_id_list", 
            "Identifiers:"
          ),
          checkboxInput(
            "fetch_binmates",
            "Find additional BIN members"
          ),
          actionButton(
            "fetch_btn",
            "Fetch data"
          )
        ),  
        bslib::accordion_panel(
          value="customize",
          "Customize",
          icon = bsicons::bs_icon("sliders"),
          selectizeInput("filt_opt",
                         "Columns to show:",
                         filter_options,
                         multiple = TRUE
          ),
          selectizeInput("filt_seq",
                         "Filter by marker:",
                         list("COI-5P" = "COI-5P", 
                              "COI-3P" = "COI-3P",
                              "28S" = "28S",
                              "28S-D2-D3" = "28S-D2-D3",
                              "18S" = "18S",
                              "18S-5P" = "18S-5P", 
                              "EF1-alpha" = "EF1-alpha"),
                         multiple = TRUE
          ),
          div(class="form-group shiny-input-container",
              actionButton(
                "filter_btn",
                "Apply"
                ),
              actionButton(
                "reset_btn",
                "Reset"
                )),
          #checkboxInput(
          #  "include_binmates",
          #  "Include additional BIN members",
          #  value = FALSE
          #),
          checkboxInput(
            "collapse_mrkrs",
            "Collapse multiple markers into single rows",
            value = FALSE
          )
        ),  
        bslib::accordion_panel(
          value="summarize",
          "Summarize",
          icon = bsicons::bs_icon("bar-chart"),
          selectInput("ana_opt",
                      "Summarize by:",
                      list(
                        "Taxonomy" = list(
                          "Taxonomic summary" = "tax_summary"),
                        "Unique values - selected fields" = list(
                          "Taxon" = "identification",
                          "BIN" = "bin_uri",
                          "Identification dates (parsed)" = "id_date_parsed",
                          "Projects" = "projects",
                          "Datasets" = "datasets",
                          "Country" = "country.ocean"),
                        "Unique values - all fields" = as.list(config$fieldsets$bcdm[!config$fieldsets$bcdm %in% c("identification","bin_uri","country.ocean","id_date_parsed")])
                        ),
                      multiple = FALSE
          ),
          div(class="form-group shiny-input-container",
              actionButton(
                "ana_btn",
                "Summarize"
                )),
          div(class="form-group shiny-input-container",
              div(class="control-label", "Sample one record for each unique BIN-taxon combination:",),
              div(actionButton(
                "bin_rep_btn",
                "Get BIN reps"
                ),
                tooltip(
                  icon("circle-question"),
                  "BIN reps are chosen per the following criteria in order of priority: (1) sequence length modal, with preference for 658 bp; (2) identification by morphology or image; (3) voucher deposited at the CBG; (4) recent collection date.",
                  id = "tip"
                )
              )
          )
        )
      ), width = 3
    ),
    mainPanel(
      navset_underline(
        id="tabs",
        nav_panel(
          value="data",
          "Data",
          hidden(div(
            id = 'table_area',
            DT::dataTableOutput("table") %>% withSpinner(color="#aaaaaa", type=5, size=0.6)),
            actionButton("fasta_btn","Copy FASTA",icon=icon("copy")))
          )),
      hidden(div(id = "table_buttons",
              downloadButton("save_tsv","TSV"),
              downloadButton("save_csv","CSV"),
              downloadButton("save_xlsx","XLSX"))
      ),
    width = 9)
  )
)