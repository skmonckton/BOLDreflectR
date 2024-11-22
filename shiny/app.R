library(shiny)
library(shinyjs)
library(shinycssloaders)
library(bslib)
library(BOLDconnectR)
library(stringr)
library(writexl)

timestamp = as.character(format(Sys.Date(),"%Y%m%d"))

bold.fetch.all <- function (get_by, identifiers, cols = NULL, export = NULL, na.rm = FALSE, 
          filt_taxonomy = NULL, filt_geography = NULL, filt_latitude = NULL, 
          filt_longitude = NULL, filt_shapefile = NULL, filt_institutes = NULL, 
          filt_identified.by = NULL, filt_seq_source = NULL, filt_marker = NULL, 
          filt_collection_period = NULL, filt_basecount = NULL, filt_altitude = NULL, 
          filt_depth = NULL) 
{
  stopifnot(nrow(identifiers) > 0)
  input_data = data.frame(col1 = base::unique(identifiers))
  names(input_data)[names(input_data) == "col1"] <- get_by
  switch(get_by, processid = , sampleid = {
    if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
    json.df = BOLDconnectR:::fetch.bold.id(data.input = input_data, query_param = get_by)
  }, dataset_codes = , project_codes = , bin_uris = {
    if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
    processids = BOLDconnectR:::get.bin.dataset.project.pids(data.input = input_data, 
                                              query_param = get_by)
    json.df = BOLDconnectR:::fetch.bold.id(data.input = processids, query_param = "processid")
  }, stop("Input params can only be processid, sampleid, dataset_codes, project_codes, or bin_uris."))
  #json.df = json.df[, intersect(names(json.df), bold.fields.info()$field)]
  json.df = BOLDconnectR:::bold.fetch.filters(bold.df = json.df, taxon.name = filt_taxonomy, 
                               location.name = filt_geography, latitude = filt_latitude, 
                               longitude = filt_longitude, shapefile = filt_shapefile, 
                               institutes = filt_institutes, identified.by = filt_identified.by, 
                               seq.source = filt_seq_source, marker = filt_marker, 
                               collection.period = filt_collection_period, basecount = filt_basecount, 
                               altitude = filt_altitude, depth = filt_depth)
  if (!is.null(cols)) {
    bold_field_data = BOLDconnectR:::bold.fields.info(print.output = F) %>% 
      dplyr::select(field)
    if (!all(cols %in% bold_field_data$field)) 
      stop("Names provided in the 'cols' argument must match with the names in the 'field' column that is available using the bold.fields.info function.")
    json.df = json.df %>% dplyr::select(all_of(cols))
  }
  if (na.rm) {
    json.df = json.df %>% tidyr::drop_na(.)
  }
  if (!is.null(export)) {
    utils::write.table(json.df, paste0(export, ".tsv", sep = ""), 
                       sep = "\t", row.names = FALSE, quote = FALSE)
  }
  return(json.df)
}

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

server <- function(input, output) {
  
  out_table <- reactiveVal()
  
  observeEvent(input$fetch_btn, {
    output$table <- NULL
    
    bold.apikey(input$api_key)
    fetch_ids <- str_split(input$fetch_id_list,"\n")
    
    show('table_area')
    
    if (input$fetch_all == TRUE) {
      tryCatch({
        data <- bold.fetch.all(get_by = input$fetch_by, identifiers=fetch_ids)
        out_table(data)
        output$table <- DT::renderDataTable(DT::datatable({
          data
          }, filter='top'))
        show('dl_buttons')
        }, error=function(e){
          hide('table_area')
          showNotification(paste("ERROR: Data could not be retrieved. Please check identifiers and try again."), type = "error")
          }
      )
    } else {
      tryCatch({
        data <- bold.fetch(get_by = input$fetch_by, identifiers=fetch_ids)
        out_table(data)
        output$table <- DT::renderDataTable(DT::datatable({
          data
          }, filter='top'))
        show('dl_buttons')
        }, error=function(e){
          hide('table_area')
          showNotification(paste("ERROR: Data could not be retrieved. Please check identifiers and try again."), type = "error")
        }
      )
    }
  })
  
  output$save_tsv <- downloadHandler(
    filename = paste0("BOLD_fetch_",as.character(format(Sys.Date(),"%Y%m%d")),".tsv"),
    content = function(file) {
      write.table(out_table(), file, sep="\t", na="", quote=FALSE, row.names = FALSE)
    }
  )
  
  output$save_csv <- downloadHandler(
    filename = paste0("BOLD_fetch_",as.character(format(Sys.Date(),"%Y%m%d")),".csv"),
    content = function(file) {
      write.csv(out_table(), file, na="", row.names = FALSE)
    }
  )
  
  output$save_xlsx <- downloadHandler(
    filename = paste0("BOLD_fetch_",as.character(format(Sys.Date(),"%Y%m%d")),".xlsx"),
    content = function(file) {
      write_xlsx(out_table(), file, format_headers=FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)