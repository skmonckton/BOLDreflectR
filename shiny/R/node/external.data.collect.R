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

bold.data.collect <- function(
  bold.search.res,
  chunk.size = 4e6,
  sys.sleep = 0,
  export = FALSE,
  export.type = c("tsv", "parquet"),
  output.path = NULL
) {
  export.type <- match.arg(export.type)

  check.tbl.sql(bold.search.res)

  con <- dbplyr::remote_con(bold.search.res)

  # parquet export (direct and doesnt need chunks)

  if (export && export.type == "parquet") {
    if (is.null(output.path)) {
      stop("Please provide output.path for parquet export")
    }

    query <- dbplyr::sql_render(bold.search.res)

    DBI::dbExecute(
      con,
      paste0(
        "
      COPY (
        ",
        query,
        "
      )
      TO '",
        output.path,
        "' 
      (FORMAT PARQUET, COMPRESSION ZSTD)
    "
      )
    )

    message("Parquet export complete.")

    return(invisible(NULL))
  }

  # TSV export needs chunking of data for collection before export
  #1 Getting chunks

  DBI::dbExecute(con, "PRAGMA disable_progress_bar;")

  chunk_info <- get_chunk_indices(
    input_file = bold.search.res,
    chunksize = chunk.size
  )

  total_rows <- chunk_info$total_rows
  chunk_size <- chunk_info$chunk_size
  chunk_indices <- chunk_info$chunk_indices
  total_chunks <- length(chunk_indices)

  DBI::dbExecute(con, "PRAGMA enable_progress_bar;")

  if (total_chunks == 1) {
    message(sprintf("Collecting all %d rows in a single chunk...", total_rows))
    res <- bold.search.res %>% dplyr::collect()
  } else {
    tbl_sql <- dbplyr::sql_render(bold.search.res)

    res_chunks <- progressr::with_progress({
      p <- progressr::progressor(steps = total_chunks)

      lapply(chunk_indices, function(i) {
        offset <- (i - 1) * chunk_size
        size <- min(chunk_size, total_rows - offset)

        p(sprintf("Chunk %d/%d (%d rows)", i, total_chunks, size))

        sql_query <- paste0(
          "SELECT * FROM (",
          tbl_sql,
          ") AS sub_tbl ",
          "LIMIT ",
          size,
          " OFFSET ",
          offset
        )

        out <- DBI::dbGetQuery(con, sql_query)

        gc()

        if (sys.sleep > 0) {
          Sys.sleep(sys.sleep)
        }

        out
      })
    })

    res <- dplyr::bind_rows(res_chunks)
  }

  #2. Converting data format to match API-fetched data
  
  res <- res %>%
    dplyr::mutate(coord = gsub("\\[|\\]|\\s", "", coord),
                  bold_recordset_code_arr = gsub("\\[|\\]|\\'|\\s", "", bold_recordset_code_arr)) %>%
    dplyr::rename(all_of(c(country.ocean = "country/ocean", province.state = "province/state")))
  
  #3. Exporting as a TSV

  if (export && export.type == "tsv") {
    if (is.null(output.path)) {
      stop("Please provide output.path for TSV export")
    }

    utils::write.table(
      res,
      file = output.path,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE
    )

    message("TSV export complete.")
  }

  return(invisible(res))
}
