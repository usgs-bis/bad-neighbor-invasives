


state_all_species_query <- function(fips_list, taxon) {
    
    
    # https://bison.usgs.gov/solr/occurrences/select/?q=computedStateFips:51%20AND%20hierarchy_homonym_string%3A%2A%5C-202422%5C-%2A%20AND%20scientificName:/[A-Za-z]*[%20]{1,1}[A-Za-z]*/&rows=0&facet=true&facet.field=ITISscientificName&facet.limit=-1&facet.mincount=1&wt=json
    
    qstring <- stringr::str_c(c("computedStateFips:", fips_list,
                                " AND hierarchy_homonym_string:", taxon),
                                # " AND scientificName:/[A-Za-z]\\*[%20]\\{1,1\\}[A-Za\\-z]*/"),
                              collapse = "")
    
    # build the url and execute
    # https://services.itis.gov/?q=nameWOInd:/[A-Za-z0-9]*[%20]{1,1}[A-Za-z0-9]*/ - matches only binomials
    query <- list(q = qstring,
                  facet = 'true', # facet the result
                  facet.mincount = 1, # minimum count to be included in result
                  facet.field = "ITISscientificName",
                  facet.limit = -1, #return all the results
                  wt = 'json', # return result as json
                  json.nl = "arrarr", # return result as an array
                  rows = 0) # get all rows
    
    # build the url and execute
    query_url <- httr::GET("https://data.usgs.gov/solr/occurrences/select/?q=",
                           query = query)
    
    
    # return the request as json
    occurrence_json <- httr::content(query_url, "parsed")
    
    # convert the JSON to an r object
    data <- jsonlite::fromJSON(occurrence_json)
    
    # we really only want to keep two values, all records and the number of species
    df <- tibble::tibble(abundance = data$response$numFound,  # all records (abundance) is the numRecords result
                         # the count is the number of rows in the result (i.e. distinct species)
                         count = nrow(data$facet_counts$facet_fields$ITISscientificName)) 
    
    # return the result
    return(df)
    
}