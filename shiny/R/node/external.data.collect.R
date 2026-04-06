#' Collect and export BOLD NODE search results
#'
#' @description collects,outputs and exports the tbl_sql 'bold.data.search' results object, processing large datasets in user defined manageable chunks.
#'
#' @param bold.search.res A tbl_sql object obtained from 'bold.data.search'
#' @param chunk.size Maximum number of rows to process in each chunk (default: 4e6)
#' @param sys.sleep Time to sleep between chunks in seconds (default: 0)
#' @param export Logical value that allows user to export the output locally. Default value is FALSE
#' @param output.path Character string specifying the local path for data export
#'
#' @return A data frame containing all collected results
#'
#' @importFrom dplyr summarise collect pull bind_rows
#' @importFrom DBI dbExecute
#' @importFrom dbplyr remote_con sql_render
#'
#' @export

bold.data.collect <- function(bold.search.res,
                              chunk.size = 4e6,
                              sys.sleep = 0,
                              export = FALSE,
                              output.path) {
  
  check.tbl.sql(bold.search.res)
  
  # DB connection
  con <- dbplyr::remote_con(bold.search.res)
  
  # Disable DuckDB progress bar temporarily
  DBI::dbExecute(con, "PRAGMA disable_progress_bar;")
  
  # Get chunk metadata using your helper
  chunk_info <- get_chunk_indices(
    input_file = bold.search.res,
    chunksize = chunk.size
  )
  
  total_rows   <- chunk_info$total_rows
  chunk_size   <- chunk_info$chunk_size
  chunk_indices <- chunk_info$chunk_indices
  total_chunks <- length(chunk_indices)
  
  DBI::dbExecute(con, "PRAGMA enable_progress_bar;")
  
  # If only one chunk
  if (total_chunks == 1) {
    
    message(sprintf("Collecting all %d rows in a single chunk...", total_rows))
    
    res_chunks <- bold.search.res %>% 
      dplyr::collect()
    
  } else {
    
    # Get SQL query for the lazy table
    tbl_sql <- dbplyr::sql_render(bold.search.res)
    
    res_chunks <- lapply(chunk_indices, function(i) {
      
      offset <- (i - 1) * chunk_size
      size <- min(chunk_size, total_rows - offset)
      
      message(sprintf("[%s] Collecting chunk %d/%d (rows: %d)...",
                      Sys.time(), i, total_chunks, size))
      
      sql_query <- paste0(
        "SELECT * FROM (", as.character(tbl_sql), ") AS sub_tbl ",
        "LIMIT ", size, " OFFSET ", offset
      )
      
      res <- DBI::dbGetQuery(con, sql_query)
      
      gc()
      
      if (sys.sleep > 0) {
        Sys.sleep(sys.sleep)
      }
      
      return(res)
    })
  }
  
  # Combine all chunks
  res <- dplyr::bind_rows(res_chunks)
  
  if (export) {
    utils::write.table(res,
                       output.path,
                       sep = "\t",
                       row.names = FALSE,
                       quote = FALSE)
  }
  
  return(res)
}