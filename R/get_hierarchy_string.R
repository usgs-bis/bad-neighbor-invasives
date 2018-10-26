#' get_hierarchy_string
#' 
#' This a helper function to generate a formatted hierarchy string for a given taxon to use in the Bad Neighbor BAP.  The function uses 
#' the SOLR service from the Integrated Taxonomic Information System ([ITIS](https://www.itis.gov)).
#'
#' @param taxon_name A string for a scientific name for a taxonomic group to search
#'
#' @return A tibble with the returned tsn, taxonomic rank and formatted hierarchy string to use in other parts of the BAP 
#' @export
#'
#' @examples
#' # query ITIS to get the string
#' h_string <- get_hierarchy_string("Poaceae")
#' 
#' # print the result
#' h_string
#' # A tibble: 1 x 4
#' # tsn   rank   taxon   hierarchy_homonym_string
#' # <chr> <chr>  <chr>   <chr>                   
#' #    1 40351 Family Poaceae "*\\-40351\\-*"
#'     
#' # use the result in the main bad neighbor query using state name and optional flag
#' buff_nn <- state_bad_neighbor_query(state_name = "Virginia", taxon = h_string$hierarchy_homonym_string, get_buffer_fips = TRUE)
get_hierarchy_string <- function(taxon_name) {
    # check for input
    if (missing(taxon_name)) {
        stop("A scientific name for a taxon to search is required.")
    }
    
    # build the string parameter
    qstring <- stringr::str_c(c("nameWOInd:", taxon_name), collapse = "")
    
    # build the query
    query <- list(q = qstring, # the query
                  wt = 'json') # return result as json
    
    # build the url and execute
    query_url <- httr::GET("https://services.itis.gov/?q=",
                           query = query)
    
    # parse the request
    occurrence_json <- httr::content(query_url, "text", encoding = "UTF-8")
    
    # convert the JSON to an r object
    data <- jsonlite::fromJSON(occurrence_json)
    
    # extract values
    tsn <- data$response$docs$tsn
    rank <- data$response$docs$rank
    name <- data$response$docs$nameWInd
    
    # build the hierarchy_string in the form "*\\-tsn\\-*
    hierarchy_string <- stringr::str_c("*\\-", # wildcard value
                                       tsn, # the tsn from the results
                                       "\\-*", # wildcard search
                                       collapse = "")
    
    # return a tibble with the values
    result <- tibble::tibble(tsn = tsn, # the returned tsn
                             rank = rank, # the taxonomix rank
                             taxon = name, # the name searched
                             hierarchy_homonym_string = hierarchy_string) # the formatted string

    # return the result
    return(result)
}