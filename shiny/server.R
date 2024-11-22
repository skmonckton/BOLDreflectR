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