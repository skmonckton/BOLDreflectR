## Helper functions for BOLD NODE

#1 Import Parquet File

import_parquet_data<-function(path)
{
  
  #1 Establish a temporary connection
  temp_connection <- DBI::dbConnect(duckdb::duckdb())
  
  #2 Create a query
  get_parquet_data<-sprintf("Select * from parquet_scan('%s')",path)
  
  parquet_data<-tbl(temp_connection, 
                    sql(get_parquet_data))
  
  return(parquet_data)
  
  # Ensure disconnection at the end, even if an error occurs
  on.exit(DBI::dbDisconnect(temp_connection, 
                            shutdown = TRUE), 
          add = TRUE)
  
  
}

#2 Check tbl_sql object

check.tbl.sql <- function(tbl) {
  if (!inherits(tbl, c("tbl_sql", "tbl_dbi", "tbl"))) {
    stop("Error: Input must be a tbl_sql / dbplyr table")
  }
  TRUE
}


#3 BOLD search filters

bold.search.filters<-function (bold.df,
                               ids=NULL,
                               bins=NULL,
                               taxonomy=NULL,
                               geography=NULL,
                               institutes=NULL,
                               identified.by=NULL,
                               seq.source=NULL,
                               marker=NULL,
                               basecount=NULL,
                               biogeo_cat=NULL,
                               dataset.projects=NULL,
                               bounding.box=NULL,
                               ambi.base.cutoff=NULL)



{
  

  #0. ids
  
  # ids is your vector of IDs to filter
  if (!is.null(ids)) {
    
    bold.df = bold.df %>%
      filter(processid %in% !!ids |
               sampleid  %in% !!ids)
    
  }
  
  #1. BIN
  
  if(!is.null(bins)) {
    
    bold.df = bold.df %>%
      filter(bin_uri %in% !!bins)
    
  }
  
  
  #2. taxon name
  
  # condition to check if the taxon name is of the correct data type
  
  if(!is.null(taxonomy))
    
  {
    
    bold.df <- bold.df %>%
      dplyr::filter(
        kingdom   %in% !!taxonomy |
          phylum    %in% !!taxonomy |
          class     %in% !!taxonomy |
          `order`   %in% !!taxonomy |
          family    %in% !!taxonomy |
          subfamily %in% !!taxonomy |
          genus     %in% !!taxonomy |
          species   %in% !!taxonomy
      )
    
  }
  
  
  #3. specific country/region/site/sector
  
  
  if(!is.null(geography))
    
  {
    
    
    bold.df <- bold.df %>%
      dplyr::filter(`country/ocean` %in% !!geography |
                      `province/state`  %in% !!geography |
                      region %in% !!geography |
                      sector %in% !!geography | 
                      site %in% !!geography)
    
  }
  
  
  #4. Latitude/Longitude bounding box
  
  if (!is.null(bounding.box)) {
    
    min_lon <- bounding.box[1]
    max_lon <- bounding.box[2]
    min_lat <- bounding.box[3]
    max_lat <- bounding.box[4]
    
    bold.df <- bold.df %>%
      mutate(coord_clean = regexp_replace(coord, '\\[|\\]|\\s', ''),
             lat = sql("split_part(coord_clean, ',', 1)"),
             # Extract longitude = second part
             lon = sql("replace(split_part(coord_clean, ',', 2), ']', '')"),
             lat = as.numeric(lat),
             lon = as.numeric(lon))%>%
      filter(between(lon, min_lon, max_lon),
             between(lat, min_lat, max_lat)) %>%
      select(-c(coord_clean,lat,lon))


  }
  
  #5. Institutes storing the specimen
  
  
  if(!is.null(institutes))
    
  {
    
    
    bold.df=bold.df%>%
      dplyr::filter(inst %in% !!institutes)
    
  }
  
  
  #6. Identified by
  
  
  if(!is.null(identified.by))
    
  {
    
    
    if(is.character(identified.by)==FALSE)
      
    {
      
      warning("identified by should be a character data type")
      
      return(FALSE)
      
    }
    
    bold.df=bold.df%>%
      dplyr::filter(identified_by %in% !!identified.by)
    
  }
  
  
  #7. sequence source
  
  
  if(!is.null(seq.source))
    
  {
    
    
    bold.df=bold.df%>%
      dplyr::filter(sequence_run_site %in% !!seq.source)
    
  }
  
  
  #8. Type of marker
  
  
  if(!is.null(marker))
    
  {
    
    bold.df=bold.df%>%
      dplyr::filter(marker_code %in% !!marker)
    
  }
  
  
  #9. basecount
  
  
  if(!is.null(basecount))
    
  {
    
    # if user has selected multiple markers, lengths should be provided as a named list
    if(is.list(basecount)) {
      
      for(i in seq_along(basecount)) {
        marker <- names(basecount[i])
        vals <- unname(unlist(basecount[i]))
        if(length(vals)==1) {
          # user may also provide unnamed length(s) to apply a general filter
          if(!is.null(marker) || (marker == "")) {
            bold.df <- bold.df %>%
              dplyr::filter(((marker_code == marker) & (nuc_basecount %in% vals))
                            | (marker_code != marker)) 
          } else {
            bold.df <- bold.df %>%
              dplyr::filter(nuc_basecount %in% basecount)
          }
        } else if (length(vals) == 2) {
          first_val <- vals[1]
          last_val  <- vals[2]
          if(!is.null(marker) || (marker == "")) {
            bold.df <- bold.df %>%
              dplyr::filter(((marker_code == marker) & 
                               (dplyr::between(nuc_basecount, first_val, last_val)))
                            | (marker_code != marker)) 
          } else {
            bold.df <- bold.df %>%
              dplyr::filter(dplyr::between(nuc_basecount, first_val, last_val))
          }
        }
      }
    } else {
      
      if(length(basecount)==1)
        
      {
        
        bold.df=bold.df%>%
          dplyr::filter(nuc_basecount %in% basecount)
        
      }
      
      
      else if (length(basecount)==2)
        
      {
        
        first_val <- basecount[1]
        
        last_val  <- basecount[2]
        
        bold.df <- bold.df %>%
          dplyr::filter(dplyr::between(nuc_basecount, 
                                       first_val, 
                                       last_val))
      }
      
      else
        
      {
        
        stop("Incorrect value input")
        
      }
    
    }
    
  }
  
  #10. Biogeo/ecological categories
  
  if(!is.null(biogeo_cat))
    
  {
    
    # filter condition to select the specific taxon name.
    
    bold.df <- bold.df %>%
      dplyr::filter(biome %in% !!biogeo_cat |
                      realm %in% !!biogeo_cat |
                      ecoregion  %in% !!biogeo_cat)
    
  }
  
  #11. Dataset or project code
  
  if (!is.null(dataset.projects)) {
    
    codes_regex <- paste(dataset.projects, collapse = "|")
    
    bold.df <- bold.df %>%
      mutate(
        bold_recordset_code_arr = regexp_replace(bold_recordset_code_arr, '\\[|\\]', '')) %>%
      filter(regexp_matches(bold_recordset_code_arr, 
                            codes_regex))
    
  }
  
  #12. Ambiguous bases cutoff
  
  # Return unchanged if no cutoff provided
  if (!is.null(ambi.base.cutoff)) {
    
    bold.df = bold.df %>%
      mutate(
        count_ambi = sql("
        ARRAY_LENGTH(
          ARRAY_FILTER(
            REGEXP_SPLIT_TO_ARRAY(UPPER(nuc), ''),
            x -> REGEXP_MATCHES(x, '[NRYSWKMBDHV]')
          ))"),
        percent_ambi = (count_ambi * 100.0 / nuc_basecount)) %>%
      filter(case_when(
        percent_ambi < 1.00 ~ '<1%',
        percent_ambi >= 1.00 & percent_ambi <= 5.00 ~ '1-5%',
        percent_ambi > 5.00 ~ '>5%'
      ) == !!ambi.base.cutoff) }
      #select(-count_ambi, -percent_ambi)
  
  
  return(bold.df)
  
  #on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
}


#4 Chunked data indices

get_chunk_indices<-function(input_file,
                            chunksize=100000)
{
  
  chunk_size=chunksize
  
  
  # Determine total rows 
  total_rows <- input_file %>% 
    summarize(n = n()) %>% 
    collect() %>% 
    pull(n)
  
  # Calculating the number of chunks required
  
  n_chunks <- ceiling(total_rows / chunk_size)
  
  # Generate chunk indices
  
  chunk_indices <- seq_len(n_chunks)
  
  chunk_res = list(total_rows = total_rows,
                   chunk_size=chunk_size,
                   chunk_indices=chunk_indices)
  
  return(chunk_res)
}

#5 Convert utf8 tsv to parquet

convert_to_parquet <- function(input.file, 
                               output.file)
{
  
  #### Add col checks so that only bcdm data is converted
  
  # Create a simple in-memory DuckDB connection
  con <- dbConnect(duckdb())
  
 # --- Step 1: Read the column names from the TSV ---
  cols <- dbGetQuery(con, sprintf(
    "SELECT * FROM read_csv_auto('%s', delim='\\t') LIMIT 0;", 
    input.file
  ))
  col_names <- colnames(cols)
  
  # --- Step 2: Build NULLIF expressions for all columns ---
  # This converts any 'None' string to actual NULL in DuckDB
  select_expr <- paste(
    sprintf("NULLIF(\"%s\", 'None') AS \"%s\"", col_names, col_names),
    collapse = ",\n"
  )
  
  # --- Step 3: Build full COPY query to Parquet ---
  sql <- sprintf("
COPY (
  SELECT %s
  FROM read_csv_auto('%s', delim='\\t',ALL_VARCHAR=TRUE)
)
TO '%s' (FORMAT PARQUET, COMPRESSION 'ZSTD');
", select_expr, input.file, output.file)

dbExecute(con, sql)

dbDisconnect(con, 
             shutdown = TRUE)
}

#6 Convert to UTF 8 encoded TSV

#function to convert to utf-8 (optional)

convert_to_utf<-function(input.file,
                         output.file)
{
  
  system2("iconv",
          args = c("-f", "ISO-8859-1",
                   "-t", "UTF-8//TRANSLIT",
                   
                   shQuote(input.file)),
          stdout = output.file)
  
}