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