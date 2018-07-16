# require(tidyverse)

# clean_result <- function(l) {
#     
# }


state_occurrence_query <- function(fips_list, taxon) {
    
    # build the string for the query
    qstring <- stringr::str_c(c("establishmentMeans:L48 AND computedStateFips:", fips_list,
                                " AND hierarchy_homonym_string:", taxon),
                              collapse = "")
    
    # build the url and execute
    # https://services.itis.gov/?q=nameWOInd:/[A-Za-z0-9]*[%20]{1,1}[A-Za-z0-9]*/ - matches only binomials
    query <- list(q = qstring,
                  fl = "ITISscientificName,ITIStsn",
                  facet = 'true', # facet the result
                  facet.mincount = 1, # minimum count to be included in result
                  facet.pivot = "ITISscientificName,ITIStsn",
                  # facet.field = 'ITIStsn',
                  # facet.field = "ITISscientificName",
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
    

    # combine the scientific names with the counts
    sn_df <- tibble::tibble(name = data$facet_counts$facet_pivot$`ITISscientificName,ITIStsn`$value, cnt = data$facet_counts$facet_pivot$`ITISscientificName,ITIStsn`$count)

    # test for ambiguous results (there have been instances with more than one TSN result row per pivot, but there should only be one)
    # extract the list of tsn from the pivot
    a <- data$facet_counts$facet_pivot$`ITISscientificName,ITIStsn`$pivot

    # the result of the pivot is a list of data frames, so count the number of rows in each element of the list
    b <- sapply(a, nrow)

    # find the index of any list element with more than 1 row
    bad_idx <- which(b > 1)

    # remove (and export?) the anomaly
    if(length(bad_idx) > 0) {
        # drop the unwanted elements
        tsn_list <- a[-c(bad_idx)]
        full_df <- sn_df[-c(bad_idx), ]

        # export the anomaly
        exp_df <- tibble::tibble(tsn = a[bad_idx], sname = sn_df$name[c(bad_idx)], cnt = sn_df$cnt[c(bad_idx)])
    } else {
        # keep all
        tsn_list <- a
    }

    # the result contains a pivot list of TSN associated with the ITIS scientific names
    # this has to be extracted and put in a data frame
    tsn_df <- purrr::map_df(tsn_list, `[`, c("field", "value", "count"))


    # combine the two
    full_df <- dplyr::bind_cols(full_df, tsn = tsn_df$value)#, tsn_cnt = tsn_df$count)

    # The TSN field *might* contain multiple entries where taxonomic synonyms or hominymns occur.
    # The semicolon separator precludes promoting the field to integer as we want, so, for now,
    # we can split the field on the semicolon and keep the first entry (assumed to be the accepted TSN)
    # test for presence of a semicolon
    
    # if(sum(stringr::str_detect(full_df$tsn, ";")) > 0) {
    #     # split the string on the first semicolon (there will be two parts regardless of the number of semicolons)
    #     tmp1 <- str_split_fixed(full_df$tsn, ";", n = 2)
    #     # reassign the first column of TSN to the data frame
    #     full_df$tsn <- tmp1[, 1]
    # }

    # # promote the data types
    full_df$tsn <- as.numeric(full_df$tsn)
    # tsn_df$cnt <- as.numeric(full_df$count)

    # return the result
    return(full_df)
}


# <option value="%20AND%20hierarchy_homonym_string:*\-179913\-*">Mammals (Mammalia)</option>
#     <option value="%20AND%20hierarchy_homonym_string:*\-173747\-*">Reptiles (Reptilia)</option>
#         <option value="%20AND%20hierarchy_homonym_string:*\-174371\-*">Birds (Aves)</option>
#             <option value="%20AND%20hierarchy_homonym_string:*\-161030\-*">Boney Fish (Osteichthyes)</option> 
#                 <option value="%20AND%20hierarchy_homonym_string:*\-173420\-*">Amphibians (Amphibia)</option>
#                     <option value="%20AND%20hierarchy_homonym_string:*\-99208\-*">Insects (Insecta)</option>  
#                         <option value="%20AND%20hierarchy_homonym_string:*\-202423\-*">All Animals (Animalia)</option>
#                             <option value="%20AND%20hierarchy_homonym_string:*\-500009\-*">Conifers (Pinopsida)</option>
#                                 <option value="%20AND%20hierarchy_homonym_string:*\-202422\-*">All Plants (Plantae)</option>
                                    # <option value="%20AND%20hierarchy_homonym_string:*\-555705\-*">Fungi</option>