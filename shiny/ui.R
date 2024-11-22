ui <- page_fillable(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  titlePanel("BOLDreflectR"),
  sidebarLayout(
    sidebarPanel(
      passwordInput( 
        "api_key", 
        "BOLD API key:",
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
      input_switch("fetch_all", "Include non-BCDM fields (e.g. verbatim fields)"), 
      actionButton(
        "fetch_btn",
        "Fetch data"
      ),
    ),
    mainPanel(
      hidden(div(id = 'table_area',
          DT::dataTableOutput("table") %>% withSpinner(color="#aaaaaa", type=5, size=0.6)),
        div(id = "dl_buttons",
          downloadButton("save_tsv","TSV"),
          downloadButton("save_csv","CSV"),
          downloadButton("save_xlsx","XLSX"))
    ))
  )
)