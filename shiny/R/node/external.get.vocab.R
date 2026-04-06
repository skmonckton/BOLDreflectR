#' Extract unique vocabulary from a BOLD dataset
#'
#' @description Extracts distinct values of specified field/s in BOLD parquet data
#'
#' @param input.parquet Path to the input parquet file
#' @param specific.cols Name of the column to extract unique values from
#' @param save.data Logical indicating whether to save the results to disk as a .rds file (default: FALSE)
#' @param output.file Path (without extension) for saving results as .rds file (required if save.data = TRUE)
#'
#' @return A data frame containing unique values from the specified column
#'
#' @importFrom dplyr filter distinct collect
#'
#' @export

bold.get.vocab <- function(
  input.parquet,
  specific.cols,
  save.data = FALSE,
  output.file = NULL
) {
  # Allow both parquet path OR tbl_sql input
  if (inherits(input.parquet, "tbl_sql")) {
    bold_parquet_data <- input.parquet
  } else {
    bold_parquet_data <- import_parquet_data(input.parquet)
  }

  # Get unique values per column separately
  terms_list <- lapply(specific.cols, function(col) {
    bold_parquet_data %>%
      dplyr::filter(!is.na(.data[[col]]) & .data[[col]] != "") %>%
      dplyr::distinct(.data[[col]]) %>%
      dplyr::collect() %>%
      dplyr::pull(.data[[col]])
  })

  names(terms_list) <- specific.cols

  if (save.data) {
    if (is.null(output.file)) {
      stop("output.file must be provided when save.data = TRUE")
    }

    saveRDS(terms_list, paste0(output.file, ".rds"))
  }

  return(terms_list)
}
