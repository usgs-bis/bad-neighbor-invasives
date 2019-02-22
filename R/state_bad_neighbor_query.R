# load the required source codes
source("R/state_fips_lookup.R")

#' state_bad_neighbor_list
#' 
#' The primary BISON query and parsing function to develop both the state and buffer state lists of non-native species.
#'
#' @param fips_list a single state two-digit FIPS code or a parenthetical group of FIPS codes.  Pairs of state FIPS codes and surrounding states are pre-developed and stored in `data/state_lookup.csv`.  Note: the list includes the Distict of Columbia;
#' @param taxon A string hierarchy_homonym_string in the form: "*\\-179913\\-*".  Predefined lists are stored in: `data/heirarchy_strings.csv`.  The list of taxa is currently restricted to the best represented taxa in BISON.
#' @param state_name (Optional) string name of the state to process.
#' @param get_buffer_fips (Optional) flag to lookup the buffer states FIPS codes
#'
#' @param useEstMeans (Optional) flag to use the non-native flag in BISON.  Defaults to TRUE.
# 
#' @return a tibble.  A data frame with a list of bad neighbor species with associated TSN.
#' @export
#'
#' @examples
#' # look up all non-native plants in Virginia
#' va_nn <- state_bad_neighbor_query(fips_list = 51, taxon = "*\\-202422\\-*")
#' 
#' # same query using state name
#' va_nn <- state_bad_neighbor_query(state_name = "Virginia", taxon = "*\\-202422\\-*")
#' 
#' # all non-native plants in the surrounding buffer states 
#' buff_states <- "(24 11 37 47 21 54)"
#' buff_nn <- state_bad_neighbor_query(fips_list = buff_states, taxon = "*\\-202422\\-*")
#' 
#' # same query using state name and optional flag
#' buff_nn <- state_bad_neighbor_query(state_name = "Virginia", taxon = "*\\-202422\\-*", get_buffer_fips = TRUE)
state_bad_neighbor_query <- function(fips_list, taxon, state_name, 
                                     useEstMeans = TRUE, get_buffer_fips = FALSE) {
    # test for missing taxon
    if(missing(taxon)) {
        stop("Taxon is required.")
    }
    
    # test for fips or state name
    if(missing(fips_list) & missing(state_name)) {
        stop("Either a FIPS, a list of FIPS, or a state name are required.")
    }
    
    # lookup a fips code if needed
    if(missing(fips_list) & !missing(state_name)) {
        # lookup the codes
        f <- state_fips_lookup(state_string = state_name)
        
        # lookup will fail if there is more than one match
        if (is.null(f)) {
            stop("There are multiple matches for the state name, be more specific.")
        }
        
        # get the buffer fips
        if (get_buffer_fips) {
            fips_list <- f$buffer_fips
        } else {
            # default to state FIPS if unknown
            fips_list <- f$state_fips
        }
    }
    # test for using establishmentMeans
    if(!useEstMeans) {
        qstring <- stringr::str_c(c("computedStateFips:", fips_list,
                                    " AND hierarchy_homonym_string:", taxon),
                                  collapse = "")
    } else {
        # build the string for the query
        qstring <- stringr::str_c(c("establishmentMeans:L48 AND computedStateFips:", fips_list,
                                    " AND hierarchy_homonym_string:", taxon),
                                  collapse = "")
    }
    
    # build the url and execute
    query <- list(q = qstring,
                  fl = "ITISscientificName,ITIStsn",
                  facet = 'true', # facet the result
                  facet.mincount = 1, # minimum count to be included in result
                  facet.pivot = "ITISscientificName,ITIStsn",
                  # facet.field = 'ITIStsn',
                  # facet.field = "ITISscientificName",
                  # facet.field = "ITIScommonName", # common name if available
                  facet.limit = -1, #return all the results
                  wt = 'json', # return result as json
                  json.nl = "arrarr", # return result as an array
                  rows = 0) # get all rows
    
    # build the url and execute
    query_url <- httr::GET("https://bison.usgs.gov/solr/occurrences/select/?q=",
                           query = query)
    
    
    # return the request as json
    occurrence_json <- httr::content(query_url, "parsed")
    
    # convert the JSON to an r object
    data <- jsonlite::fromJSON(occurrence_json)
    
    # extract values
    n <- data$facet_counts$facet_pivot$`ITISscientificName,ITIStsn`$value
    v <- data$facet_counts$facet_pivot$`ITISscientificName,ITIStsn`$count

    # test for empty result
    if(!is.null(n)) {
        
        # combine the scientific names with the counts
        sn_df <- tibble::tibble(name = n, cnt = v)
    
        # test for ambiguous results (there have been instances with more than one TSN result
        # row per pivot, but there should only be one)
        # extract the list of tsn from the pivot
        a <- data$facet_counts$facet_pivot$`ITISscientificName,ITIStsn`$pivot
    
        # the result of the pivot is a list of data frames, so count the number of rows in
        # each element of the list
        b <- sapply(a, nrow)
    
        # find the index of any list element with more than 1 row
        bad_idx <- which(b > 1)
    
        # remove and export the anomaly
        if(length(bad_idx) > 0) {
            # drop the unwanted elements
            tsn_list <- a[-c(bad_idx)]
            full_df <- sn_df[-c(bad_idx), ]
        } else {
            # keep all
            tsn_list <- a
            full_df <- sn_df
        }
    
        # the result contains a pivot list of TSN associated with the ITIS scientific names
        # this has to be extracted and put in a data frame
        tsn_df <- purrr::map_df(tsn_list, `[`, c("field", "value", "count"))
    
        # combine the two
        full_df <- dplyr::bind_cols(full_df, tsn = tsn_df$value)#, tsn_cnt = tsn_df$count)
    
        # The TSN field *might* contain multiple entries where taxonomic synonyms or hominymns occur.  This should be fixed
        # (or reduced) in future ITIS updates.
        # The semicolon separator precludes promoting the field to integer as we want, so, for now,
        # remove the questions.
        
        # keep only the tsn without semicolons
        full_df <- dplyr::filter(full_df, !stringr::str_detect(tsn, ";"))
    
        # # promote the data types
        full_df$tsn <- as.numeric(full_df$tsn)
    
        # if useEstMeans is false, intersect the USGS nn-list
        if(!useEstMeans) {
            nn_list <- readxl::read_excel("data/Non-native_Master_07_06_2018.xlsx")
            # intersect the lists and keep the matches
            full_df <- dplyr::semi_join(full_df, nn_list, by=c("tsn" = "TSN"))
        }
        
        # return the result
        return(full_df)
    } # end null test
}
