library(shiny)
library(shinyjs)
library(shinycssloaders)
library(bslib)
library(BOLDconnectR)
library(stringr)
library(writexl)
library(dplyr)
library(yaml)
library(bsicons)
library(keyring)
library(clipr)

timestamp = as.character(format(Sys.Date(),"%Y%m%d"))

config <- read_yaml("data/config.yaml", readLines.warn=FALSE)

ranks <- c("phylum","class","order","family","subfamily","tribe","genus","species","subspecies")

filter_options <- list("Default" = list("BCDM fields" = "bcdm", 
                                        "All verbatim fields" = "all"),
                       "Custom" = list("Sample ID, Process ID, BIN" = "min",
                                       "Taxonomic identification" = "acctax",
                                       "Verbatim identification" = "verbtax",
                                       "Verbatim ID method" = "verbid",
                                       "Sequence data" = "sequence",
                                       "Voucher/specimen details" = "specimen",
                                       "Verbatim specimen details" = "verbspec",
                                       "Collection data" = "collection",
                                       "Verbatim collection data" = "verbcollect",
                                       "Projects & datasets" = "recset"),
                       "Parsed" = list("Identification date" = "iddate"))

#data %>% select(!record_id) %>% tidyr::pivot_wider(names_from = marker_code, values_from = c(insdc_acs,nuc,nuc_basecount,sequence_run_site,sequence_upload_date))

cb <- function(df, header=TRUE, sep="\t", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, col.names=header, row.names=FALSE, quote=FALSE, na="NA")
  showNotification(paste0("Copied to clipboard."),type = "message")
}

search.public.recs <- function(input) {
  if(input$fetch_by == "processid" | input$fetch_by == "sampleid"){
    pubids <- bold.public.search.plus(ids=fetch_ids)$processid
  } else if (input$fetch_by == "bin_uris"){
    pubids <- bold.public.search.plus(bins=fetch_ids)$processid
  } else if (input$fetch_by == "dataset_codes") {
    pubids <- bold.public.search.plus(dataset_codes=fetch_ids)$processid
  } else if (input$fetch_by == "project_codes") {
    pubids <- bold.public.search.plus(project_codes=fetch_ids)$processid
  }
  return(pubids)
}
 
list.recordsets <- function(data,set_type) {
  filt <- data
  if(set_type == "projects") {
    filt <- data %>%
      distinct(sampleid, .keep_all=TRUE) %>%
      mutate(project_code = str_split(bold_recordset_code_arr, ",")) %>%
      tidyr::unnest(project_code) %>% 
      #distinct(project_code) %>% 
      filter(!str_detect(project_code, "^DS-")) %>%
      count(project_code, name="count", .drop=FALSE)
  } else if(set_type == "datasets") {
    filt <- data %>%
      distinct(sampleid, .keep_all=TRUE) %>%
      mutate(dataset_code = str_split(bold_recordset_code_arr, ",")) %>%
      tidyr::unnest(dataset_code) %>% 
      #distinct(dataset_code) %>% 
      filter(str_detect(dataset_code, "^DS-|^DATASET-")) %>%
      count(dataset_code, name="count", .drop=FALSE)
  }
  return(filt)
}

collapse.markers <- function(data) {
  data %>%
    select(!record_id) %>%
    tidyr::pivot_wider(
      names_from = marker_code,
      values_from = c(insdc_acs,nuc,nuc_basecount,sequence_run_site,sequence_upload_date),
      names_glue = "{marker_code}_{.value}") %>%
    select(!starts_with("NA_"))
}

summarize.table <- function(data, sum_type) {
  if (sum_type %in% c("projects","datasets")){
    summary <- list.recordsets(data, sum_type)
  } else if (is.null(sum_type)) {
    summary <- data
  } else {
    summary <- data %>% mutate(across(where(is.character), ~ na_if(.,""))) %>% distinct(sampleid,.keep_all=TRUE) %>% count(!!as.name(sum_type), name="count", .drop=FALSE)
  }
  return(summary)
}

get.id.date <- function(data) {
  id_dates <- data %>% 
    mutate(id_date_verb = case_when(is.na(specimendetails.verbatim_identification_method) ~ NA,
                                    .default = lubridate::my(str_match(specimendetails.verbatim_identification_method, "\\(([A-Za-z]{3,9} [0-9]{2,4})\\)")[,2]))) %>%
    mutate(id_date_note = case_when(is.na(taxonomy_notes) ~ NA,
                                    .default = lubridate::my(str_match(taxonomy_notes, "id-date:\\s?([A-Za-z]{3,9} [0-9]{2,4})")[,2]))) %>%
    mutate(id_date =
             format(as.Date(
               do.call(pmax, c(select(., c('id_date_verb','id_date_note')), na.rm = TRUE)),
               format = "%Y-%m-%d"), format = "%b %Y")
    ) %>%
    .$id_date
    #select(!any_of(c("id_date_verb","id_date_note")))
  return(id_dates)
}

count.taxa <- function(data) {
  tax_count <- data.frame(identification=character(), count=integer())
  
  data <- data %>% distinct(sampleid,.keep_all=TRUE)
  
  for (r in ranks) {
    tax_count <- tax_count %>% 
      bind_rows(data %>%
                  count(!!as.name(r)) %>%
                  rename("identification" = !!as.name(r), "count" = n)) %>%
      distinct(identification,.keep_all=TRUE)
  }
  
  tax_hier <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(tax_hier) <- ranks
  tax_hier$identification <- character()
  
  for (r in ranks) {
    tax_hier <- tax_hier %>% 
      mutate_if(is.logical, as.character) %>% 
      bind_rows(data %>%
                  distinct(!!as.name(r),.keep_all=TRUE) %>%
                  mutate(identification = !!as.name(r)) %>%
                  select(all_of(c("identification",ranks[1:which(ranks==r)])))) %>%
      distinct(identification,.keep_all=TRUE)
  }
  
  tax_count <- tax_count %>%
    left_join(tax_hier,by="identification") %>%
    select(all_of(c(ranks,"count"))) %>%
    distinct(!!!syms(ranks), .keep_all = TRUE) %>%
    replace(is.na(.), "0") %>%
    arrange(phylum,class,order,family,subfamily,tribe,genus,species,subspecies) %>%
    replace(.=="0"," ") %>%
    select(where(~!all(.x == " ")))
  
  return(tax_count)
}

bp.mode <- function(x) {
  t <- table(x)
  names(t)[ t == max(t) ]
}

get.bin.reps <- function(df) {
  
  df <- df %>% filter(!is.na(bin_uri))
  
  if (df %>% filter(!str_detect(inst,"GenBank")) %>% nrow() > 0) {
    df <- df %>% filter(!str_detect(inst,"GenBank"))
  }
  
  lengths <- df %>% group_by(bin_uri) %>% reframe(mode=list(bp.mode(nuc_basecount))) #%>% .$mode
  
  reps <- df %>%
    left_join(lengths, by = "bin_uri" ) %>%
    rowwise() %>%
    mutate(nuc_pref = nuc_basecount %in% mode) %>%
    mutate(nuc_exact = (nuc_basecount %in% mode) & (nuc_basecount == 658)) %>%
    ungroup() %>%
    mutate(inst_pref = str_detect(inst,"Centre for Biodiversity Genomics")) %>%
    mutate(cbg_pref = str_starts(sampleid,"BIOUG|CBG")) %>% 
    mutate(id_pref = str_detect(identification_method,"(?i)morphology|(?i)image")) %>%
    group_by(bin_uri, identification) %>%
    arrange(desc(id_pref), desc(inst_pref), desc(cbg_pref), desc(collection_date_start), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(!any_of(c("mode","nuc_pref","nuc_exact","inst_pref","cbg_pref","id_pref")))
  
  return(reps)
}




bold.fetch.plus <- function (get_by, identifiers, cols = NULL, export = NULL, na.rm = FALSE, 
          filt_taxonomy = NULL, filt_geography = NULL, filt_latitude = NULL, 
          filt_longitude = NULL, filt_shapefile = NULL, filt_institutes = NULL, 
          filt_identified.by = NULL, filt_seq_source = NULL, filt_marker = NULL, 
          filt_collection_period = NULL, filt_basecount = NULL, filt_altitude = NULL, 
          filt_depth = NULL, verbatim_cols = FALSE)
{
  stopifnot(nrow(identifiers) > 0)
  input_data = data.frame(col1 = base::unique(identifiers))
  names(input_data)[names(input_data) == "col1"] <- get_by
  switch(get_by, processid = , sampleid = {
    if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
    json.df = fetch.bold.id.shiny(data.input = input_data, query_param = get_by)
  }, dataset_codes = , project_codes = , bin_uris = {
    if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
    processids = BOLDconnectR:::get.bin.dataset.project.pids(data.input = input_data, 
                                              query_param = get_by)
    json.df = fetch.bold.id.shiny(data.input = processids, query_param = "processid")
  },{
    stop("Input params can only be processid, sampleid, dataset_codes, project_codes, or bin_uris.")
    #showNotification(paste0("Input params can only be processid, sampleid, dataset_codes, project_codes, or bin_uris."), type = "error")
    #return(NULL)
    })
  if (verbatim_cols == FALSE) json.df = json.df[, intersect(names(json.df), bold.fields.info()$field)]
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
    if (!all(cols %in% bold_field_data$field)) {
      stop("Names provided in the 'cols' argument must match with the names in the 'field' column that is available using the bold.fields.info function.")
      #showNotification(paste0("Names provided in the 'cols' argument must match with the names in the 'field' column that is available using the bold.fields.info function."), type = "error")
      #return(NULL)
    }
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

bold.public.search.plus <- function(taxonomy = NULL,
                               geography = NULL,
                               ids = NULL,
                               bins = NULL,
                               dataset_codes=NULL,
                               project_codes=NULL)
  
{
  
  # Arguments list (list used since combination could entail long vectors of any of the 5 arguments in any numbers and combinations)
  
  args <- list(taxonomy = taxonomy,
               geography = geography,
               ids = ids,
               bins = bins,
               dataset_codes=dataset_codes,
               project_codes=project_codes)
  
  # Colors for printing the progress on the console
  
  green_col <- "\033[32m"
  
  red_col<-"\033[31m"
  
  reset_col <- "\033[0m"
  
  # Filter out NULL values and get their values
  
  # Null arguments
  
  null_args=sapply(args,is.null)
  
  # Selecting the non null arguments
  
  non_null_args = args[!null_args]
  
  # The query input
  
  trial_query_input = unlist(non_null_args)|>unname()
  
  # Empty list for the data if downloaded in batches
  
  downloaded_data<-list()
  
  # Condition to see whether non null arguments are 1 or more than 1 and what the length of the query based on the arguments is
  
  if(length(non_null_args)>1||length(non_null_args)==1 && length(trial_query_input)<=5)
  {
    cat(red_col,"Downloading ids.",reset_col,'\r')
    
    result = BOLDconnectR:::fetch.public.data(query = trial_query_input)
    
    cat("\n", green_col, "Download complete.\n", reset_col, sep = "")
    
  }
  else
  {
    generate.batch.ids = BOLDconnectR:::generate.batches(trial_query_input,batch.size = 5)
    
    cat(red_col,"Downloading ids.",reset_col,'\r')
    
    # The tryCatch here has been added such that for every loop, if the search terms do not fetch any results, those will return NULL instead of the function entirely stopping and no output generated.
    
    result.pre.filter = lapply(generate.batch.ids,function(x){
      result <- tryCatch(
        {
          # Download the data
          BOLDconnectR:::fetch.public.data(x)
        },
        error = function(e) {
          # Error
          # message(paste("Error with", batch, ":", e$message))
          return(NULL)
        }
      )
      # appending the result to the empty list
      downloaded_data[[length(downloaded_data) + 1]] = result
    })
    
    # Binding the list of dataframes
    
    result=result.pre.filter%>%
      bind_rows(.)
    
    cat("\n", green_col, "Download complete.\n", reset_col, sep = "")
  }
  
  # If the query doesnt return anything due to query terms not existing in the database or the combination of search returning zero
  
  if(is.null(result)||nrow(result)==0) return(NULL)
  
  if(nrow(result)>1000000) warning("Data cap of 1 million records has been reached. If there are still more data available on BOLD, please contact BOLD support for obtaining the same OR (if applicable) break the query into subsets and use the function to loop through it.")
  
  result = result%>%
    dplyr::select(processid,
                  sampleid)%>%
    dplyr::distinct(sampleid,
                    .keep_all = TRUE)
  
  invisible(result)
  
}

fetch.bold.id.shiny<-function(data.input,
                        query_param)
  
{
  
  # query parameters for API
  
  query_params <- list('input_type'= query_param,
                       'record_sep' = ",",
                       last_updated_threshold=0)
  
  # base url for API
  
  base_url= "https://data.boldsystems.org/api/records/retrieve?"
  
  # Colors for printing the progress on the console
  
  green_col <- "\033[32m"
  
  red_col<-"\033[31m"
  
  reset_col <- "\033[0m"
  
  # Check to assess the number of requests
  
  # < 5000
  
  if (nrow(data.input) <= 5000)
    
    
  {
    
    # Extract the ids to a single character file to be used for the url used for POST
    
    id_data<-paste(data.input[,1],
                   collapse = ",")
    
    # Custom function id.files used here. This will generate the ids as a temp_file required by the body of the POST API
    
    temp_file=BOLDconnectR:::id.files(id_data)
    
    
    # The helper function 'post.api.res.fetch' is used to retrieve the data.
    
    showNotification(paste0("Initiating download..."), id="fetch_msg", type = "default")

    showNotification(paste0("Downloading data in a single batch..."), id="fetch_msg", duration = NULL, type = "message")
    
    result=BOLDconnectR:::post.api.res.fetch(base.url=base_url,
                                                 query.params=query_params,
                                                 temp.file=temp_file)
    
    # Custom function fetch.data used here. The function will convert the POST output to a dataframe (text/jsonlines->json->data.frame). The function also resolves the issues of columns having multiple values in single cells into a single character with values separated by commas using helper function 'bold.multirecords.set'. Helper function 'reassign.data.type' is then used to assign the requisite data types to respective columns.
    
    json.df=BOLDconnectR:::fetch.data(result)
    
    # Stop if there is no data
    
    if (nrow(json.df) == 0) {
      showNotification(paste0("The output generated has no data. Please re-check whether the correct 'get_by' and corresponding identifier argument has been specified."), id="fetch_msg", type = "error")
      return(NULL)
    }
    
    # Else print the output
    
    showNotification(paste0("Download complete & BCDM dataframe generated."), duration = 2, id="fetch_msg", type = "message")
    
  }
  
  # > 5000
  
  else if (nrow(data.input)>5000)
    
  {
    
    ## If the ids input is more than 5000, a batch of 5000 ids is generated using the 'generate.batches' helper function, output of which is used to create the temp_files for POST.
    
    # Declare batch size
    
    generate.batch.ids = BOLDconnectR:::generate.batches(data = data.input[,1],
                                          batch.size = 5000)
    
    
    # generate the temp_file/s
    
    temp_file = lapply(generate.batch.ids,
                       BOLDconnectR:::id.files)
    
    
    # Obtain the POST result. Here lapply is used with the post.api.res.fetch to generate the output (BOLD data) based on the 5000 processids. A custom message is printed on the console displaying the status of download.
    
    # Defining the colors for printing download updates on the console
    
    showNotification(paste0("Initiating download in batches..."), id="fetch_msg", type = "default")
    
    result <- lapply(seq_along(temp_file), function(f) {
      
      # file index
      
      file <- temp_file[[f]]
      
      # Print the index and total number of files using sprintf
      
      #a Creating the message
      
      download_message<-sprintf("Downloading batch: %d of %d", f, length(temp_file))
      
      #b Show it in a notification

      showNotification(paste0(download_message,"..."), id="fetch_msg", duration = NULL, type = "message")
      
      
      # Download data (list of data frames)
      post_res=BOLDconnectR:::post.api.res.fetch(
        base.url = base_url,
        query.params = query_params,
        temp.file = file)
      
      result_df=BOLDconnectR:::fetch.data(post_res)
      
    })
    
    showNotification(paste0("Batch download complete. Generating the combined BCDM dataframe..."), id="fetch_msg", type = "message")
    
    json.df=result%>%
      dplyr::bind_rows(.)
    
    # Stop if there is no data
    
    if (nrow(json.df) == 0) {
      showNotification(paste0("The output generated has no data. Please re-check whether the correct 'get_by' and corresponding identifier argument has been specified."), id="fetch_msg", type = "error")
      return(NULL)
    }
    
    
    showNotification(paste0("BCDM dataframe generated."), duration=2, id="fetch_msg", type = "message")
  }
  
  return(json.df)
  
}



clip.fasta<-function(bold_df,cols_for_fas_names=NULL)
  
{
  
  if (is.null(cols_for_fas_names)) cols_for_fas_names <- processid
  
  showNotification(paste0("Copying FASTA..."), id="fas_msg", type = "default")
  
  clip_fas <- bold_df %>%
    filter(!is.na(nuc)) %>%
    mutate(f1 = ">") %>% 
    mutate(f2 = "\n") %>%  
    tidyr::unite(fasta,c(f1,any_of(cols_for_fas_names),f2,nuc), sep="") %>%
    .$fasta %>%
    unlist(str_split(.,"\n"))

  #if (object.size(clip_fas) > (2000*1000)) {
  #  showNotification(paste0("FASTA output too large. Please try again with fewer sequences."), id="fas_msg", type = "error")
  #} else {
  #  write_clip(clip_fas)
  #  showNotification(paste0("Copied to clipboard!"), id="fas_msg", type = "message")
  #}
  
  tryCatch(
    {
      write.table(x = clip_fas,
                  file = "clipboard-100000",
                  sep = "",
                  row.names = FALSE,
                  col.names = FALSE,
                  quote=FALSE)
      showNotification(paste0("Copied to clipboard."),
                       id = "fas_msg",
                       type = "message")
    },
    error = function(e) {
      showNotification(paste0("FASTA output too large. Please try again with fewer sequences."), id="fas_msg", type = "error")
    }
  )
  
  

  
}
