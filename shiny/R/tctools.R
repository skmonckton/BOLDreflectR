re_int <- "\\.\\Z|\\S{2,}\\.\\S|[0-9]|\\s[A-Z]|[A-Z]\\Z|[A-Z]{2}|[a-z][A-Z]|_(?!(hn|sl|ss)\\Z)|%|\\?|!|\\[|\\]|\\{|\\}|\\(|\\)|,|\\s(?:aff|agg|cf|complex|group|grp|gr|gp|cmplx|pr|ms|cfr|nr|nsp|near|nomen|hybrid|voucher|form|from|ss|ssl|see|spp?|sample)\\.?(?:\\s|\\Z)"

bold.fetch.plus <- function (get_by, identifiers, cols = NULL, BCDM_only = TRUE, 
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
        tryCatch({
            processids = suppressPackageStartupMessages(BOLDconnectR:::get.bin.dataset.project.pids(data.input = input_data, 
                query_param = get_by))
        }, error = function(msg) {
            stop("No records were found matching the provided identifiers.")
        })
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
    json.df <- switch(get_by, processid = , sampleid = {
        ids <- intersect(identifiers, json.df[[get_by]])
        json.df[order(match(json.df[[get_by]], ids)), ]
    }, bin_uris = {
        ids <- intersect(identifiers, json.df$bin_uri)
        json.df[order(match(json.df$bin_uri, ids)), ]
    }, json.df)
    return(json.df)
}

empty <- function (x) 
{
    (is.na(x) | (x == ""))
}

get_bin_discordance <- function (bold_df) 
{
    
    count_ne <- function(x, out = c("N", "vals", "recs")) {
        if (out == "recs") 
            return(length(x[x != "" & !is.na(x)]))
        uvals <- unique(x[x != "" & !is.na(x)])
        if (out == "N") 
            return(length(uvals))
        if (out == "vals") {
            if (length(uvals) == 0) 
                return("")
            return(paste0(sort(uvals), collapse = ","))
        }
    }
    data <- as.data.table(bold_df)[!is.na(bold_df[["bin_uri"]])]
    summary <- data[, .(record_count = .N, min_rank = as.factor(ranks[min(match(get("identification_rank"), 
        ranks))]), max_rank = as.factor(ranks[max(match(get("identification_rank"), 
        ranks))]), distinct_countries = count_ne(get("country.ocean"), 
        "N"), distinct_taxa = count_ne(get("identification"), 
        "N"), distinct_phylum = count_ne(get("phylum"), "N"), 
        distinct_class = count_ne(get("class"), "N"), distinct_orders = count_ne(get("order"), 
            "N"), distinct_families = count_ne(get("family"), 
            "N"), distinct_subfamilies = count_ne(get("subfamily"), 
            "N"), distinct_tribes = count_ne(get("tribe"), "N"), 
        distinct_genera = count_ne(get("genus"), "N"), distinct_species = count_ne(get("species"), 
            "N"), distinct_subspecies = count_ne(get("subspecies"), 
            "N"), country_list = count_ne(get("country.ocean"), 
            "vals"), phylum_list = count_ne(get("phylum"), "vals"), 
        class_list = count_ne(get("class"), "vals"), oder_list = count_ne(get("order"), 
            "vals"), family_list = count_ne(get("family"), "vals"), 
        subfamily_list = count_ne(get("subfamily"), "vals"), 
        tribe_list = count_ne(get("tribe"), "vals"), genera_list = count_ne(get("genus"), 
            "vals"), species_list = count_ne(get("species"), 
            "vals"), subspecies_list = count_ne(get("subspecies"), 
            "vals"), recs_with_phylum_info = count_ne(get("phylum"), 
            "recs"), recs_with_class_info = count_ne(get("class"), 
            "recs"), recs_with_order_info = count_ne(get("order"), 
            "recs"), recs_with_family_info = count_ne(get("family"), 
            "recs"), recs_with_subfamily_info = count_ne(get("subfamily"), 
            "recs"), recs_with_tribe_info = count_ne(get("tribe"), 
            "recs"), recs_with_genus_info = count_ne(get("genus"), 
            "recs"), recs_with_species_info = count_ne(get("species"), 
            "recs"), recs_with_subspecies_info = count_ne(get("subspecies"), 
            "recs")), by = "bin_uri"]
}

get_portal_bin_stats <- function (bins, shiny = FALSE) 
{
    get_public_count <- function(bin) {
        req <- httr::RETRY("GET", url = paste0("https://portal.boldsystems.org/api/summary?query=bin:uri:", 
            bin, "&fields=marker_code&reduce_operation=count"), 
            httr::add_headers(accept = "application/json"), body = FALSE, 
            times = 5, quiet = TRUE, terminate_on = 200, terminate_on_success = FALSE)
        if (req$status_code != 200) {
            return(NA)
        }
        json_preprocess <- suppressMessages(suppressWarnings(httr::content(req, 
            "text")))
        count <- jsonlite::fromJSON(json_preprocess)$marker_code[["COI-5P"]]
        ifelse(is.null(count), 0, count)
    }
    bin_query <- function(bin) {
        url <- paste0("https://portal.boldsystems.org/api/ancillary?collection=barcodeclusters&key=barcodecluster.uri&values=", 
            bin)
        req <- httr::RETRY("GET", url = url, httr::add_headers(accept = "application/json"), 
            body = FALSE, times = 5, quiet = TRUE, terminate_on = 200, 
            terminate_on_success = FALSE)
        if (req$status_code != 200) {
            return(list())
        }
        json_preprocess <- suppressMessages(suppressWarnings(httr::content(req, 
            "text")))
        resp <- jsonlite::fromJSON(json_preprocess)
        if (length(resp) == 0) {
            return(list())
        }
        resp[["marker_code.count"]] <- get_public_count(bin)
        return(resp)
    }
    if (shiny == TRUE) {
        opi <- pbapply::pboptions(type = "shiny", title = "Retrieving BIN stats from BOLD Portal API", 
            label = "")
        bin_resp <- data.table::rbindlist(pbapply::pblapply(bins, 
            bin_query), fill = TRUE)
    }
    else {
        opi <- pbapply::pboptions(type = "txt", title = "Retrieving BIN stats from BOLD Portal API", 
            label = "")
        bin_resp <- data.table::rbindlist(pbapply::pblapply(bins, 
            bin_query), fill = TRUE)
    }
    pbapply::pboptions(opi)
    if (!is.null(bin_resp) && nrow(bin_resp) > 0) {
        failed <- length(bins) - nrow(bin_resp[!is.na(bin_resp[["record_count"]]), 
            ])
        if (failed > 0) 
            warning(paste0("Portal stats retrieval timed out for ", 
                failed, " BINs."))
        bin_resp[, .(bin_uri = bin_resp[["barcodecluster.uri"]], 
            member_count = bin_resp[["record_count"]], private_members = bin_resp[["record_count"]] - 
                bin_resp[["marker_code.count"]], public_members = bin_resp[["marker_code.count"]], 
            avg_dist = bin_resp[["barcodecluster.avgdist"]], 
            max_dist = bin_resp[["barcodecluster.maxdist"]], 
            nn_dist = bin_resp[["barcodecluster.nn_dist"]], nn_bin_uri = bin_resp[["nearestneighbour.uri"]])]
    }
    else {
        warning("Portal stats retrieval failed for all requested BINs.")
    }
}

get_apikey <- function () 
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
