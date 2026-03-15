re_int <- "\\.\\Z|\\S{2,}\\.\\S|[0-9]|\\s[A-Z]|[A-Z]\\Z|[A-Z]{2}|[a-z][A-Z]|_(?!(hn|sl|ss)\\Z)|%|\\?|!|\\[|\\]|\\{|\\}|\\(|\\)|,|\\s(?:aff|agg|cf|complex|group|grp|gr|gp|cmplx|pr|ms|cfr|nr|nsp|near|nomen|hybrid|voucher|form|from|ss|ssl|see|spp?|sample)\\.?(?:\\s|\\Z)"

bold.fetch.plus <- function(get_by, identifiers, cols = NULL, BCDM_only = TRUE, 
    export = NULL, na.rm = FALSE, filt_taxonomy = NULL, filt_geography = NULL, 
    filt_latitude = NULL, filt_longitude = NULL, filt_shapefile = NULL, 
    filt_institutes = NULL, filt_identified.by = NULL, filt_seq_source = NULL, 
    filt_marker = NULL, filt_collection_period = NULL, filt_basecount = NULL, 
    filt_altitude = NULL, filt_depth = NULL) 
{
  
    stopifnot(nrow(identifiers) > 0)
    if (nchar(Sys.getenv("api_key")) == 0) {
        message("No API key in memory; checking system keystore...")
        suppressPackageStartupMessages(BOLDconnectR::bold.apikey(get_apikey()))
        message("BOLD API key set. Proceeding with fetch...")
    }
    input_data = data.frame(col1 = base::unique(identifiers))
    names(input_data)[names(input_data) == "col1"] <- get_by
    switch(get_by, processid = , sampleid = {
        if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
        json.df = suppressPackageStartupMessages(BOLDconnectR:::fetch.bold.id(data.input = input_data, 
            query_param = get_by))
    }, dataset_codes = , project_codes = , bin_uris = {
        if (!nrow(input_data) > 0) stop("Please re-check the data provided in the identifiers argument.")
        processids = suppressPackageStartupMessages(BOLDconnectR:::get.bin.dataset.project.pids(data.input = input_data, 
            query_param = get_by))
        json.df = BOLDconnectR:::fetch.bold.id(data.input = processids, 
            query_param = "processid")
    }, stop("Input params can only be processid, sampleid, dataset_codes, project_codes, or bin_uris."))
    if (BCDM_only) 
        json.df = json.df[, intersect(names(json.df), BOLDconnectR::bold.fields.info()[["field"]])]
    json.df = BOLDconnectR:::bold.fetch.filters(bold.df = json.df, 
        taxon.name = filt_taxonomy, location.name = filt_geography, 
        latitude = filt_latitude, longitude = filt_longitude, 
        shapefile = filt_shapefile, institutes = filt_institutes, 
        identified.by = filt_identified.by, seq.source = filt_seq_source, 
        marker = filt_marker, collection.period = filt_collection_period, 
        basecount = filt_basecount, altitude = filt_altitude, 
        depth = filt_depth)
    if (!is.null(cols)) {
        bold_field_data = BOLDconnectR::bold.fields.info(print.output = F) %>% 
            dplyr::select(dplyr::all_of("field"))
        if (!all(cols %in% bold_field_data[["field"]])) 
            stop("Names provided in the 'cols' argument must match with the names in the 'field' column that is available using the bold.fields.info function.")
        json.df = json.df %>% dplyr::select(dplyr::all_of(cols))
    }
    if (na.rm) {
        json.df = tidyr::drop_na(json.df)
    }
    if (!is.null(export)) {
        if (!grepl("[/\\\\]", export)) {
            export <- file.path(getwd(), export)
        }
        file.type <- if (grepl("\\.csv$", export, ignore.case = TRUE)) {
            "csv"
        }
        else if (grepl("\\.tsv$", export, ignore.case = TRUE)) {
            "tsv"
        }
        else {
            stop("Unsupported file type. Please provide a valid '.csv' or '.tsv' filename.")
        }
        switch(file.type, csv = {
            utils::write.table(json.df, export, sep = ",", row.names = FALSE, 
                quote = FALSE)
        }, tsv = {
            utils::write.table(json.df, export, sep = "\t", row.names = FALSE, 
                quote = FALSE)
        })
    }
    return(json.df)
}

get_bin_consensus <- function(df, ranks = c("kingdom", "phylum", "class", "order", 
    "family", "subfamily", "tribe", "genus", "species", "subspecies"), 
    threshold = 1, min_ids = 2, enforce_scientific = TRUE, groups = "bin_uri", 
    discord_format = c("list", "text")) 
{
    stopifnot(`One or more provided \`ranks\` is/are missing from \`df\`.` = all(ranks %in% 
        names(df)), `Provided \`groups\` column is missing from \`df\`.` = (groups %in% 
        names(df)), `\`threshold\` value(s) must be one or more real numbers (i.e. doubles) between 0 and 1.` = is.double(unlist(threshold)) & 
        all(unlist(threshold) >= 0) & all(unlist(threshold) <= 
        1), `\`threshold\` must be either a single number, a vector of unnamed numbers equal in length to \`ranks\`, or a named list or vector of numbers with names corresponding to ranks.` = ((length(threshold) == 
        1) | (length(threshold) == length(ranks)) | (!is.null(names(threshold)))), 
        `\`min_ids\` value(s) must be one or more whole numbers greater than zero.` = is.numeric(unlist(min_ids)) & 
            all(unlist(min_ids) > 0) & all(unlist(min_ids)%%1 == 
            0), `\`min_ids\` must be either a single number, a vector of unnamed numbers equal in length to \`ranks\`, or a named list or vector of numbers with names corresponding to ranks.` = ((length(min_ids) == 
            1) | (length(min_ids) == length(ranks)) | (!is.null(names(min_ids)))), 
        `\`discord_format\` must be one of "list" or "text".` = all(discord_format %in% 
            c("list", "text")))
    parse_param_vector <- function(param) {
        if ((length(param) != 1) | !is.null(names(param))) {
            if (is.null(names(param))) {
                param <- unlist(unname(param))
            }
            else {
                named <- as.list(param[(names(param) %in% ranks) & 
                  (!duplicated(param))])
                default <- ifelse("default" %in% names(param), 
                  param[["default"]], max(unlist(param)))
                if ((!length(named) %in% c(0, length(ranks))) & 
                  (!"default" %in% names(param))) {
                  warning(paste0("Only some ranks found among `", 
                    substitute(param), "` values, with no default given; highest value applied to all unspecified ranks."))
                }
                param <- rep(default, length(ranks))
                for (r in names(named)) param[match(r, ranks)] <- named[[r]]
            }
        }
        else {
            param <- rep(unlist(param), length(ranks))
        }
        return(param)
    }
    threshold <- parse_param_vector(threshold)
    min_ids <- parse_param_vector(min_ids)
    dt <- as.data.table(copy(df))
    if (enforce_scientific) {
        dt[, `:=`((c(ranks)), lapply(.SD, function(x) data.table::fifelse(is.na(x), 
            "", data.table::fifelse(grepl(re_int, x, 
                perl = TRUE), "", as.character(x))))), .SDcols = c(ranks)]
    }
    else {
        dt[, `:=`((c(ranks)), lapply(.SD, function(x) data.table::fifelse(is.na(x), 
            "", as.character(x)))), .SDcols = c(ranks)]
    }
    mat <- as.matrix(dt[, c(groups, ranks), with = FALSE])
    for (i in seq_len(nrow(mat))) {
        row_vals <- mat[i, ]
        non_blank_idx <- which(row_vals != "")
        if (length(non_blank_idx) > 0) {
            last <- max(non_blank_idx)
            if (last < ncol(mat)) {
                mat[i, (last + 1):ncol(mat)] <- NA_character_
            }
        }
        else {
            mat[i, ] <- NA_character_
        }
    }
    dt <- as.data.table(mat)
    setnames(dt, c(groups, ranks))
    consensus <- dt[!is.na(get(groups)), do.call(get_consistent_taxon, 
        list(.SD, ranks, threshold, min_ids)), by = groups, .SDcols = ranks]
    if (discord_format[1] == "text") {
        data.table::set(consensus, j = "discordant_ids", value = sapply(consensus[["discordant_ids"]], 
            function(x) {
                if (length(x) == 0) 
                  return("")
                sort(x, decreasing = TRUE)
                pairs <- paste0(names(x), " (", formatC(as.numeric(x), 
                  format = "f", digits = 2), ")")
                paste(pairs, collapse = ", ")
            }))
    }
    return(consensus)
}

get_bin_discordance <- function(data) {
  
  count_ne <- function(x, out = c("N", "vals", "recs")) {
    if(out == "recs") return(length(x[x != "" & !is.na(x)]))
    uvals <- unique(x[x != "" & !is.na(x)])
    if(out == "N") return(length(uvals))
    if(out == "vals") {
      if(length(uvals) == 0) return("")
      return(paste0(sort(uvals), collapse = ","))
    }
  }
  
  summary <- data[!is.na(bin_uri), .(record_count = .N,
                                     min_rank = as.factor(ranks[min(match(identification_rank, ranks))]),
                                     max_rank = as.factor(ranks[max(match(identification_rank, ranks))]),
                                     distinct_countries = count_ne(country.ocean, "N"),
                                     distinct_taxa = count_ne(identification, "N"),
                                     distinct_phylum = count_ne(phylum, "N"),
                                     distinct_class = count_ne(class, "N"),
                                     distinct_orders = count_ne(order, "N"),
                                     distinct_families = count_ne(family, "N"),
                                     distinct_subfamilies = count_ne(subfamily, "N"),
                                     distinct_tribes = count_ne(tribe, "N"),
                                     distinct_genera = count_ne(genus, "N"),
                                     distinct_species = count_ne(species, "N"),
                                     distinct_subspecies = count_ne(subspecies, "N"),
                                     country_list = count_ne(country.ocean, "vals"),
                                     phylum_list = count_ne(phylum, "vals"),
                                     class_list = count_ne(class, "vals"),
                                     oder_list = count_ne(order, "vals"),
                                     family_list = count_ne(family, "vals"),
                                     subfamily = count_ne(subfamily, "vals"),
                                     tribe = count_ne(tribe, "vals"),
                                     genera = count_ne(genus, "vals"),
                                     species = count_ne(species, "vals"),
                                     subspecies = count_ne(subspecies, "vals"),
                                     recs_with_phylum_info = count_ne(phylum, "recs"),
                                     recs_with_class_info = count_ne(class, "recs"),
                                     recs_with_order_info = count_ne(order, "recs"),
                                     recs_with_family_info = count_ne(family, "recs"),
                                     recs_with_subfamily_info = count_ne(subfamily, "recs"),
                                     recs_with_tribe_info = count_ne(tribe, "recs"),
                                     recs_with_genus_info = count_ne(genus, "recs"),
                                     recs_with_species_info = count_ne(species, "recs"),
                                     recs_with_subspecies_info = count_ne(subspecies, "recs")), by = "bin_uri"]
}

get_portal_bin_stats <- function(bins, shiny = TRUE) {
  
  # function to run taxonomy query (seemingly most accurate count of public records from portal API)
  get_public_count <- function(bin) {
    req <- httr::RETRY("GET",
                       url = paste0("https://portal.boldsystems.org/api/summary?query=bin:uri:", bin, "&fields=marker_code&reduce_operation=count"),
                       httr::add_headers('accept' = 'application/json'), body = FALSE,
                       times = 5,
                       quiet = TRUE, 
                       terminate_on = 200,
                       terminate_on_success	= FALSE)
    if(req$status_code != 200) { return(NA) }
    json_preprocess <- suppressMessages(suppressWarnings(httr::content(req, "text")))
    count <- jsonlite::fromJSON(json_preprocess)$marker_code[["COI-5P"]]
    ifelse(is.null(count), 0, count)
  }
  
  # function to get summary by BIN
  bin_query <- function(bin) {
    url <- paste0("https://portal.boldsystems.org/api/ancillary?collection=barcodeclusters&key=barcodecluster.uri&values=", bin)
    req <- httr::RETRY("GET", url=url, httr::add_headers('accept' = 'application/json'), body = FALSE,
                       times = 5,
                       quiet = TRUE,
                       terminate_on = 200,
                       terminate_on_success	= FALSE)
    if(req$status_code != 200) { return(list()) }
    json_preprocess <- suppressMessages(suppressWarnings(httr::content(req,"text")))
    resp <- jsonlite::fromJSON(json_preprocess)
    if(length(resp) == 0) { return(list())}
    resp[["marker_code.count"]] <- get_public_count(bin)
    return(resp)
  }
  
  # assemble the table
  if(shiny == TRUE) {
    pboptions(type = "shiny", title = "Retrieving BIN stats from BOLD Portal API", label = "")
    bin_resp <- rbindlist(pbapply::pblapply(bins, bin_query), fill = TRUE)
  } else {
    pboptions(type = "text", title = "Retrieving BIN stats from BOLD Portal API", label = "")
    bin_resp <- rbindlist(pbapply::pblapply(bins, bin_query), fill = TRUE)
  }
  
  if(!is.null(bin_resp) && nrow(bin_resp) > 0) {
    failed <- length(bins) - nrow(bin_resp[!is.na(record_count), ])
    if(failed > 0) warning(paste0("Portal stats retrieval timed out for ", failed, " BINs."))
    bin_resp[, .(bin_uri = barcodecluster.uri,
                 member_count = record_count,
                 private_members = record_count - marker_code.count,
                 public_members = marker_code.count,
                 avg_dist = barcodecluster.avgdist,
                 max_dist = barcodecluster.maxdist,
                 nn_dist = barcodecluster.nn_dist,
                 nn_bin_uri = nearestneighbour.uri)]
  } else {
    warning("Portal stats retrieval failed for all requested BINs.")    
  }
  
}

get_apikey <- function() 
{
    keystore <- switch(Sys.info()["sysname"], Windows = "Windows Credential Manager", 
        macOS = "macOS Keychain", Linux = "Linux Secret Service")
    tryCatch({
        keyring::key_get("BOLD-API-key")
    }, error = function(msg) {
        stop(paste0("Error retrieving API key from system's credential store (", 
            keystore, ")."))
    })
}

get_consistent_taxon <- function(sub_dt, ranks = c("kingdom", "phylum", "class", "order", 
    "family", "subfamily", "tribe", "genus", "species", "subspecies"), 
    threshold = 1, min_ids = 2) 
{
    id_hier <- sapply(ranks, function(x) NULL)
    concordant = FALSE
    rank_set <- ranks
    if (any(min_ids > nrow(sub_dt))) {
        for (i in seq_along(min_ids)) min_ids[[i]] <- nrow(sub_dt)
    }
    if (length(threshold) == 1) {
        threshold <- rep(threshold, length(ranks))
    }
    if (length(min_ids) == 1) {
        min_ids <- rep(min_ids, length(ranks))
    }
    result <- list(member_count = nrow(sub_dt), concordant_rank = NA_character_, 
        concordant_id = NA_character_, concordant_id_count = 0L, 
        discordant_rank = NA_character_, discordant_ids = list(), 
        discordant_id_count = 0L)
    for (rank_col in rev(ranks)) {
        rank_threshold <- threshold[which(ranks == rank_col)]
        rank_min_ids <- min_ids[which(ranks == rank_col)]
        vals <- sub_dt[[rank_col]]
        filtered <- table(vals[!is.na(vals)])
        name_vals <- proportions(filtered)
        props <- proportions(filtered)[(proportions(filtered) >= 
            rank_threshold) & (filtered >= rank_min_ids)]
        names(name_vals) <- sub("^$", "<None>", names(name_vals))
        if ((length(props) != 1) & (length(unique(filtered)) > 
            0)) {
            concordant <- FALSE
            if (id_hier[rank_col] != "") {
                rank_set <- ranks[0:(which(ranks == rank_col) - 
                  1)]
                id_hier <- id_hier[rank_set]
            }
            result$discordant_rank <- rank_col
            result$discordant_ids <- list(stats::setNames(as.vector(name_vals), 
                names(name_vals)))
            result$discordant_id_count <- sum(filtered)
        }
        else if ((length(props) == 1) && (names(props)[1] != 
            "")) {
            if (!concordant) {
                result$concordant_rank <- rank_col
                result$concordant_id <- names(props)[1]
                result$concordant_id_count <- unname(filtered[names(props)[1]])
            }
            concordant <- TRUE
            if (is.null(id_hier[[rank_col]]) || is.na(id_hier[[rank_col]]) || 
                is.na(names(props)[1]) || (names(props)[1] != 
                id_hier[[rank_col]])) {
                rank_set <- ranks[0:which(ranks == rank_col)]
                id_hier <- as.list(sub_dt[get(rank_col) == names(props)[1], 
                  .SD, .SDcols = rank_set][1])
            }
        }
    }
    for (r in setdiff(ranks, names(id_hier))) {
        id_hier[r] = NA_character_
    }
    result[ranks] <- id_hier
    return(result)
}

