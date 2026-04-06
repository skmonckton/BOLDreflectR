#' Convert the BOLD search to a DNAStringSet object
#'
#' @description Converts the sequence data from the BOLD search into a DNAStringSet object for downstream multiple sequence alignment with customized headers.
#'
#' @param bold.search.res A tbl_sql object containing BOLD search results
#' @param marker Character vector specifying the genetic marker
#' @param cols_for_seq_names Character vector of field names to include in the header (separated by "|")
#' 
#' @return DNAStringSet object
#'
#' @importFrom dplyr select filter mutate compute collect
#' @importFrom dbplyr sql
#'
#' @export
get.DNAStringSet <- function(bold.search.res,
                             marker = NULL,
                             cols_for_seq_names) {
  
  # Check if input is a tbl_sql (helper function you already have)
  check.tbl.sql(bold.search.res)
  
  # Ensure required columns exist
  required_cols <- c("nuc", "marker_code",cols_for_seq_names)
  
  missing_cols <- setdiff(required_cols, colnames(bold.search.res))
  
  if(length(missing_cols) > 0) {
    stop("The following required columns are missing from the input: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Start processing tbl_sql
  seq.data <- bold.search.res %>%
    select(nuc,
           marker_code,
      all_of(cols_for_seq_names)
    ) %>%
    filter(!is.na(nuc)) %>%
    filter(nuc != "") %>%
    # Remove '-' in SQL if using DuckDB/DBI
    mutate(nuc = sql("REGEXP_REPLACE(nuc, '-', '')")) %>%
    # Filter by marker if provided
    { if(!is.null(marker)) filter(., marker_code %in% marker) else . }
  
  # Build sequence names from specified columns
  obtain.seq.from.data <- seq.data %>%
    select(nuc, all_of(cols_for_seq_names)) %>%
    mutate(
      # Quote each column name to avoid SQL keyword issues
      msa.seq.name = sql(
        paste0(
          paste0('"', cols_for_seq_names, '"', collapse = " || '|' || ")
        )
      )
    ) %>%
    select(msa.seq.name, nuc)
  
  # Pull into R and convert to named character vector
  seq.from.data <- obtain.seq.from.data %>%
    collect() %>%
    { setNames(as.character(.$nuc), .$msa.seq.name) }
  
  # # Clean up names if NA
  # names(seq.from.data) <- ifelse(is.na(names(seq.from.data)),
  #                                paste0("seq", seq_along(seq.from.data)),
  #                                names(seq.from.data))
  
  # Convert to DNAStringSet
  dna.4.align <- DNAStringSet(seq.from.data)
  
  return(dna.4.align)
}


#get.dnastringset(bold_search_1,marker = "COI-5P",cols_for_seq_names = c("order","processid"))







