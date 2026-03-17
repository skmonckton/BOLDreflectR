ui <- bslib::page_fillable(
  title = "BOLDreflectR",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "brstyles.css"),
    tags$script(HTML(r"(
      $(document).on('blur', '#seq_min, #seq_max', function() {
          let current = Shiny.shinyapp.$inputValues.blur_trigger || 0;
          Shiny.setInputValue('last_blurred', this.id, {priority: 'event'});      
        });
      $(document).on('keydown', 'div[data-value=fetchdata] div.tabbable input, div[data-value=fetchdata] div.tabbable textarea', function(e) {
          if (e.ctrlKey && e.key === 'Enter') {
            Shiny.setInputValue('fetch_ctrl_enter', Math.random(), {priority: 'event'});
          }
        });
    )"))
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      #div(id="side-title",span(img(src = "reflectr.png", height = 35)),HTML(paste0("<h2 data-ver=\"v",ver,"\">BOLDreflectR</h2>"))),
      div(id="side-title",span(img(src = "reflectR-name.png", alt = "BOLDreflectR", style = "height: 3rem;")), `data-ver` = paste0("v", ver), style = "height: 4.16rem;"),
      bslib::accordion(
        id="optpanels",
        bslib::accordion_panel(
          value="fetchdata",
          "Get data",
          icon = bsicons::bs_icon("cloud-arrow-down"),
          passwordInput( 
            "api_key", 
            div("BOLD API key:",
                bslib::tooltip(
                  icon("circle-question"),
                  paste0("The API key is saved to your system's credential store (",keystore,") when the 'Get data' button is pressed."),
                  id = "api_key_tip")),
            value=tryCatch({
              key_get("BOLD.apikey")
            }, error=function(e){
              ""
            })
          ),
          bslib::navset_pill( 
            id="query_params",
            bslib::nav_panel(value="fetch_opts",
                      "Fetch",
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
                        div("Identifiers:",
                            bslib::tooltip(
                              icon("circle-question"),
                              paste0("Comma-separated or one per line."),
                              id = "query_tip"))
                      )), 
            bslib::nav_panel(value="search_opts",
                      "Full search",
                      textAreaInput( 
                        "search_tax", 
                        div("Taxonomy:",
                            bslib::tooltip(
                              icon("circle-question"),
                              paste0("Comma-separated or one taxon per line, no quotes."),
                              id = "tax_search_tip"))
                      ),
                      textAreaInput( 
                        "search_geo", 
                        div("Geography:",
                            bslib::tooltip(
                              icon("circle-question"),
                              paste0("Comma-separated or one term per line, no quotes."),
                              id = "geo_search_tip"))
                      ),
                      div(class="form-group shiny-input-container", id = "marker-select",
                          div(class="control-label", "Marker:"),
                          div(class="form-group shiny-input-container",
                            selectInput(
                              "seq_marker",
                              label = "",
                              selected = "",
                              marker_options),
                            numericInput(
                              "seq_min",
                              label = "",
                              value = NULL,
                              min = 5, max = 2000, step = 1),
                            numericInput(
                              "seq_max",
                              label = "",
                              value = NULL,
                              min = 5, max = 2000, step = 1))
                      ))
          ),
          checkboxInput(
            "fetch_binmates",
            "Find additional BIN members"
          ),
          actionButton(
            "fetch_btn",
            "Get data"
          )
        ),  
        bslib::accordion_panel(
          value="customize",
          "Customize",
          icon = bsicons::bs_icon("sliders"),
          selectizeInput("filt_opt",
                         tagList(div("Columns to show:"),
                                 actionLink(
                                   "save_filter_set",
                                   icon("floppy-disk"))),
                         filter_options(),
                         multiple = TRUE,
                         options = list(plugins = list("drag_drop"))
          ),
          selectizeInput("filt_seq",
                         "Filter by marker:",
                         list("COI-5P" = "COI-5P", 
                              "COI-3P" = "COI-3P",
                              "28S" = "28S",
                              "28S-D2-D3" = "28S-D2-D3",
                              "18S" = "18S",
                              "18S-5P" = "18S-5P"),
                         multiple = TRUE
          ),
          div(class="form-group shiny-input-container",
              # actionButton(
              #   "filter_btn",
              #   "Apply"
              #   ),
              actionButton(
                "reset_btn",
                "Reset"
                )),
          #actionButton(
          #  "get_binmates_btn",
          #  "Get additional BIN members"
          #),
          div(class="flex-div",
            bslib::input_switch(
              "include_binmates",
              "Include additional BIN members"
              ),
            actionLink(
              "view_binmates",
              #tags$i(class = "fa-regular fa-square-arrow-up-right")
              icon("square-arrow-up-right")
              )),
          bslib::input_switch(
            "include_nts",
            "Include NTS records"
          ),
          bslib::input_switch(
            "collapse_mrkrs",
            "Collapse multiple markers into single rows"
          )
        ),  
        bslib::accordion_panel(
          value="summarize",
          "Summarize",
          icon = bsicons::bs_icon("bar-chart"),
          div(class="option-group form-group",
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
                          "Unique values - all fields" = as.list(config$fieldsets$bcdm[!config$fieldsets$bcdm %in% c("identification","bin_uri","country.ocean","id_date_parsed","project_code")])
                          ),
                        multiple = FALSE
            ),
            div(class="form-group shiny-input-container",
                actionButton(
                  "ana_btn",
                  "Summarize"
                  ))),
          div(class="option-group form-group shiny-input-container", id="map-opts",
              div(class="control-label summary-section-header", "Distribution map"),
              div(class="form-group",
                  "Plot filtered records on a map:",
                  bslib::tooltip(
                    icon("circle-question"),
                    HTML("Only rows matching current filters that have valid coordinates will be included on the map."))),
              div(actionButton(
                "map_btn",
                "Generate map"
              ))
          )
        ),
        bslib::accordion_panel(
          value="analyze",
          "Analyze",
          icon = bsicons::bs_icon("graph-up"),
          div(class="option-group form-group shiny-input-container", id = "bin-cons-opts",
              div(class="control-label summary-section-header", "Consensus BIN taxonomy"),
              numericInput(
                "bc_threshold",
                div("Consensus ID threshold:",
                    bslib::tooltip(
                      icon("circle-question"),
                      paste0("Minimum proportion of records in a BIN that must have a concordant identification in order to establish consensus."))),
                1, min = 0.5, max = 1.0, step = 0.05),
              numericInput(
                "bc_minids",
                div("Minimum IDs for consensus:",
                    bslib::tooltip(
                      icon("circle-question"),
                      paste0("Minimum number of concordant identifications needed to establish a consensus identification. If a BIN contains fewer records than this number, then they must all have matching identifications to reach consensus."))),                2, min = 1, step = 1),
              checkboxInput(
                "bc_enforcesci",
                "Limit to scientific names",
                value = TRUE
              ),
              div(actionButton(
                "bin_consensus_btn",
                "Get BIN consensus"))
          ),
          div(class="option-group form-group shiny-input-container", id = "bin-disc-opts",
              div(class = "control-label summary-section-header", "BIN discordance"),
              div(class = "form-group",
                  "Compute BIN discordance at all ranks. Returns counts and lists of unique taxa per rank, by BIN, from the available data."),
              checkboxInput(
                "disc_portal",
                div("Include BOLD Portal stats",
                    bslib::tooltip(
                      icon("circle-question"),
                      HTML("<strong>Please note</strong>: This can take <strong>several minutes</strong> for more than a few dozen BINs. Additional statistics include member counts (total, private, public), pairwise distances (average, maximum), and nearest-neighbour information (NN distance, NN BIN)."),
                      id = "portaltip"
                    ))),
              div(actionButton(
                "bin_disc_btn",
                "Get BIN discordance"
              ))
          ),
          div(class="option-group form-group shiny-input-container", id = "bin-rep-opts",
              div(class="control-label summary-section-header", "BIN representatives"),
              div(class="form-group",
                  "Select one record for each unique BIN-taxon combination:",
                  bslib::tooltip(
                    icon("circle-question"),
                    "BIN reps are chosen per the following criteria in order of priority: (1) sequence length modal and/or closest to 658 bp; (2) identification by morphology or image; (3) voucher deposited at the CBG; (4) recent collection date.",
                    id = "bintip"
                  )),
              div(actionButton(
                "bin_rep_btn",
                "Get BIN reps"
              ))
          )
        )
      ), width = 3
    ),
    hidden(mainPanel(
      id = "main_panel",
      bslib::navset_underline(
        id="tabs",
        bslib::nav_panel(
          value="data",
          class = "testclass",
          "Data",
          hidden(div(
            id = 'table_area',
            DT::dataTableOutput("data_table") |> withSpinner(color="#aaaaaa", type=5, size=0.6)))
          )),
      hidden(div(id = "table_buttons",
                 class="testclass",
                 div(id = "copy_buttons",
                     actionButton("copy_fasta","Copy FASTA",icon=icon("copy")),
                     actionButton("copy_table","Copy table",icon=icon("copy")),
                     actionButton("copy_reps","Copy process IDs",icon=icon("copy"))),
                 div(id = "save_buttons",
                     downloadButton("save_tsv","TSV"),
                     downloadButton("save_csv","CSV"),
                     downloadButton("save_xlsx","XLSX")))
      ),
    width = 9))
  )
)