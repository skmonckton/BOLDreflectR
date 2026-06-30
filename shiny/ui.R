ui <- bslib::page_fillable(
  title = "BOLDreflectR",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "brstyles.css"),
    tags$script(HTML(r"(
      $(document).on('blur', '.single-marker .shiny-input-number', function() {
        Shiny.setInputValue('last_blurred', this.id, {priority: 'event'});
      });
      $(document).on('keydown', 'div[data-value=fetchdata] div.tabbable input, div[data-value=fetchdata] div.tabbable textarea', function(e) {
          if (e.ctrlKey && e.key === 'Enter') {
            Shiny.setInputValue('fetch_ctrl_enter', Math.random(), {priority: 'event'});
          }
        });
      Shiny.addCustomMessageHandler("initLookupMap", function(data) {
        window.mapBySID = {};
        data.forEach(function(row) {
          window.mapBySID[row.sampleid] = row.processid;
        });
      });
    )"))
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      div(id="side-title",
          span(img(src = "reflectR-name.png", alt = "BOLDreflectR", style = "height: 3rem;")),
          `data-ver` = paste0("v", ver),
          style = "height: 4.16rem;"),
      actionLink("cbg_btn", label = " "),
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
            value = tryCatch({
              key_get("BOLD.apikey")
            }, error = function(e){
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
                                                 HTML("Country, ocean, province, state, region, sector, realm*, biome*, or ecoregion*. Comma-separated or one term per line, no quotes.<br><span style=\"font-size: 0.7rem;\">*As defined by RESOLVE; case-sensitive, spaces replaced by underscores.</span>"),
                                                 id = "geo_search_tip"))
                                         ),
                                         div(class="form-group shiny-input-container marker-select",
                                             div(class="control-label", "Marker:"),
                                             div(class="form-group shiny-input-container single-marker",
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
                                         ),
                                         shinyWidgets::airDatepickerInput(
                                           "search_dates",
                                           label = "Collection date range:",
                                           value = NULL,
                                           range = TRUE,
                                           view = "years",
                                           clearButton = TRUE,
                                           addon = "none",
                                           placeholder = "Start - End",
                                           minDate = "2000-01-01", maxDate = "2075-01-01"),
                                         textAreaInput( 
                                           "search_inst", 
                                           div("Institutes:",
                                               bslib::tooltip(icon("circle-question"),
                                                              "Institute where physical specimen is stored. Comma-separated or one per line, no quotes."))
                                         )
                        )),
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
                                   icon("bookmark"))),
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
              actionButton(
                "reset_btn",
                "Reset"
                )),
          div(class="flex-div",
            bslib::input_switch(
              "include_binmates",
              "Include additional BIN members"
              ),
            actionLink(
              "view_binmates",
              icon("square-arrow-up-right")
              )),
          hidden(bslib::input_switch(
            "include_nts",
            div("Show NTS records in fetched data",
                bslib::tooltip(
                  icon("circle-question"),
                  "Shows or hides non-target sequence records within existing results (i.e., NTS records matching initial query terms)."))
          )),
          bslib::input_switch(
            "collapse_mrkrs",
            "Collapse multiple markers into single rows"
          )
        ),  
        bslib::accordion_panel(
          value="summarize",
          "Summarize",
          icon = bsicons::bs_icon("bar-chart"),
          inputGroup(
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
                          ), multiple = FALSE),
            div(class="form-group shiny-input-container",
                actionButton(
                  "ana_btn",
                  "Summarize"))),
          inputGroup(title = "Query hit report",
                     groupId = "qhits-opts",
                     div(class="form-group",
                         "Generate a report of query terms that were found or not:",
                         bslib::tooltip(
                           icon("circle-question"),
                           HTML("This reflects the original query only, not additional BIN members."))),
                     div(actionButton(
                       "query_hits_btn",
                       "Generate report"
                     ))),
          inputGroup(title = "Distribution map",
                     groupId = "map-opts",
                     div(class="form-group",
                         "Plot filtered records on a map:",
                         bslib::tooltip(
                           icon("circle-question"),
                           HTML("Only rows matching current filters that have valid coordinates will be included on the map."))),
                     div(actionButton(
                       "map_btn",
                       "Generate map"
                     )))),
        bslib::accordion_panel(
          value="analyze",
          "Analyze",
          icon = bsicons::bs_icon("graph-up"),
          inputGroup(title = "Consensus BIN taxonomy",
                     groupId = "bin-cons-opts",
                     numericInput(
                       "bc_threshold",
                       div("Consensus threshold:",
                           bslib::tooltip(
                             icon("circle-question"),
                             paste0("Minimum proportion of records in a BIN that must have a concordant identification in order to establish consensus."))),
                       1, min = 0.5, max = 1.0, step = 0.05),
                     numericInput(
                       "bc_minids",
                       div("Minimum IDs:",
                           bslib::tooltip(
                             icon("circle-question"),
                             paste0("Minimum number of concordant identifications needed to establish a consensus identification. If a BIN contains fewer records than this number, then they must all have matching identifications to reach consensus."))),                2, min = 1, step = 1),
                     checkboxInput(
                       "bc_enforcesci",
                       "Limit to scientific names",
                       value = TRUE
                     ),
                     checkboxInput(
                       "bc_portal_stats",
                       div("Include BOLD Portal stats",
                           bslib::tooltip(
                             icon("circle-question"),
                             HTML("<strong>Please note</strong>: This can take <strong>several minutes</strong> for more than a few dozen BINs. Additional statistics include member counts (total, private, public), pairwise distances (average, maximum), and nearest-neighbour information (NN distance, NN BIN)."),
                             id = "portaltip"
                           ))),
                     div(actionButton(
                       "bin_consensus_btn",
                       "Get BIN consensus"))),
          inputGroup(title = "BIN representatives",
                     groupId = "bin-rep-opts",
                     numericInput(
                       "bin_rep_num",
                       "Number of representatives per BIN:",
                       value = 1,
                       min = 1, step = 1
                     ),
                     checkboxInput(
                       "bin_rep_tax",
                       "Add representatives per distinct taxon",
                       value = FALSE
                     ),
                     hidden(div(id = "bin_rep_tax_opts",
                         checkboxInput(
                           "bin_rep_non_redundant",
                           "Lowest non-redundant taxa only",
                           value = TRUE
                         ),
                         checkboxInput(
                           "bin_rep_scientific",
                           "Limit to scientific names",
                           value = TRUE
                         ))),
                     bslib::input_switch(
                       "bin_rep_default",
                       "Use default selection criteria",
                       value = TRUE
                     ),
                     hidden(div(id = "bin_rep_criteria_wrapper",
                       div(id = "bin-rep-tooltip",
                           bslib::tooltip(icon("circle-question"),
                                          options = list(customClass = "grey_tooltip"),
                                          "Drag and drop to order by priority, or move unwanted criteria to 'ignored' box.")),
                       bucket_list(
                         header = "Select representative records by:",
                         group_name = "bucket_list_group",
                         orientation = "vertical",
                         add_rank_list(
                           input_id = "bin_rep_criteria",
                           text = "Selected criteria:",
                           labels = list(
                             "bin_rep_vouchered" = div("Sequence is vouchered",
                                                       bslib::tooltip(icon("circle-question"),
                                                                      options = list(customClass = "grey_tooltip"),
                                                                      "Prioritize records with known voucher repositories over those mined from databases like GenBank.")),
                             "bin_rep_seq" = tagList(selectInput("bin_rep_seq_opt",
                                                         "COI sequence length",
                                                         choices = list("nearest to 658bp" = 658,
                                                                        "nearest to ____bp (specify)" = "custom",
                                                                        "nearest to mode for BIN" = "COI_auto",
                                                                        "longest",
                                                                        "shortest"),
                                                         selected = "COI_auto"),
                                                     hidden(numericInput("bin_rep_seq_len", "", value = 658))),
                             "bin_rep_id" = selectizeInput("bin_rep_id_opt", "Preferred ID method(s)",
                                                           choices = list("BIN based", "BOLD ID Engine", "Tree based", "Morphology",
                                                                          "Morphology and sequence based", "Image based",
                                                                          "Image and sequence based", "Other sequence based approach", "Other"),
                                                           selected = "Morphology",
                                                           multiple = TRUE),
                             "bin_rep_inst" = selectizeInput("bin_rep_inst_opt", "Preferred institution(s)",
                                                             choices = list("Centre for Biodiversity Genomics"),
                                                             selected = "Centre for Biodiversity Genomics",
                                                             multiple = TRUE),
                             "bin_rep_date" = selectInput("bin_rep_date_opt", "Collection date", choices = list("latest", "oldest")),
                             "bin_rep_up" = selectInput("bin_rep_up_opt", "Sequence upload date", choices = list("latest", "oldest"))
                           )
                         ),
                         add_rank_list(
                           input_id = "bin_rep_unused",
                           text = "Ignored criteria:",
                           labels = NULL
                         )
                       ))),
                     div(actionButton(
                       "bin_rep_btn",
                       "Get BIN reps"
                     ))))
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
                     downloadButton("save_xlsx","XLSX"),
                     actionButton("open_xlsx","Open XLSX",icon=icon("file-excel"))))
      ),
    width = 9))
  )
)