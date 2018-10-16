# load the required source codes
source("R/state_fips_lookup.R")

#' state_all_species_query
#' 
#' A BISON query/parsing function returning all species in a given state.  The code will lookup
#' the state FIPS code if needed.
#'
#' @param fips_list a single state two-digit FIPS code or a parenthetical group of FIPS codes.  Pairs of state FIPS codes and surrounding states are pre-developed and stored in `data/state_lookup.csv`.  Note: the list includes the Distict of Columbia;
#' @param taxon a string hierarchy_homonym_string in the form: "*\\-179913\\-*".  Again, a predefined list is stored in: `data/heirarchy_strings.csv`.  The list of taxa is currently restricted to the best represented taxa in BISON.
#' @param state_name a string name of the state to process.
#' @param get_buffer_fips an optional flag to lookup the buffer states FIPS codes
#'
#' @return a tibble with the name of the state, the overall occurrence count for all species in the taxon, and a count of the number of species in the taxon in the state.
#' @export
#'
#' @examples
#' # find all birds ("*\-174371\-*") in Virginia
#' all_species <- state_all_species_query(fips_list = 51, taxon = "*\\-174371\\-*", state_name = "Virginia")
#' 
#' # same as above without knowing the fips
#' all_species <- state_all_species_query(taxon = "*\\-174371\\-*", state_name = "Virginia")
state_all_species_query <- function(fips_list, taxon, state_name, get_buffer_fips = FALSE) {
    # stop if state name and taxon are not present
    if(missing(taxon) | missing(state_name)) {
        stop("Taxon and state name are required.")
    }
    
    # lookup a fips code if needed
    if(missing(fips_list) & !missing(state_name)) {
        # lookup the codes
        f <- state_fips_lookup(state_string = state_name)
        
        # get the buffer fips
        if (get_buffer_fips) {
            fips_list <- f$buffer_fips
        } else {
            # default to state FIPS if unknown
            fips_list <- f$state_fips
        }
    }
    
    # build the query string from arguments
    qstring <- stringr::str_c(c("computedStateFips:", fips_list,
                                " AND hierarchy_homonym_string:", taxon),
                                # " AND scientificName:/[A-Za-z]\\*[%20]\\{1,1\\}[A-Za\\-z]*/"),
                              collapse = "")
    
    # build the url and execute
    query <- list(q = qstring,
                  facet = 'true', # facet the result
                  facet.mincount = 1, # minimum count to be included in result
                  facet.field = "ITISscientificName",
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
    
    # extract the names
    n <- data$facet_counts$facet_fields$ITISscientificName
    
    # test for empty set
    if (length(n) > 0) {
        
        # we really only want to keep two values, all records and the number of species
        df <- tibble::tibble(state_name = state_name, # name of state
                        abundance = data$response$numFound,  # all records (abundance) is the numRecords result
                        # the count is the number of rows in the result (i.e. distinct species)
                        count = nrow(n)) 

        # return the result
        return(df)
    }
}