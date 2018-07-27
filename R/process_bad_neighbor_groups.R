# load the required source code
source("R/export_bad_neighbor_list.R")
source("R/state_all_species_query.R")
source("R/state_bad_neighbor_query.R")

process_bad_neighbor_groups <- function(group_name, taxa) {
    
    # open the state codes
    state_codes <- readr::read_csv("data/state_lookup.csv")
    
    # a list to hold the full result
    full_bn_result <- list()
    
    # build a loop for the taxa
    for (k in 1:nrow(taxa)) {
        
        # get the taxon
        taxon <- taxa[k, ]
        
        # print a status message 
        print(stringr::str_c("Processing Taxon:", taxon$taxon, sep = " "))
        
        # a list to hold results
        bad_neighbor_list <- vector("list", nrow(state_codes))
        
        # a list to hold the all-species result
        state_species_list <- vector("list", nrow(state_codes))
        
        # build a loop for all states
        for(i in 1:nrow(state_codes)) {
            
            # print a status message
            print(stringr::str_c("Processing:", state_codes$state_name[i], "FIPS:", state_codes$state_fips[i],
                                 "Buffer FIPS:", state_codes$buffer_fips[i], sep = " "))
            
            # run the occurrence queries
            state_nn <- state_bad_neighbor_query(fips_list = state_codes$state_fips[i], 
                                                 taxon = taxon$hierarchy_homonym_string)
            
            buffer_nn <- state_bad_neighbor_query(fips_list = state_codes$buffer_fips[i], 
                                                  taxon = taxon$hierarchy_homonym_string)
            
            # run the all-species query for the state
            state_species_list[[i]] <- state_all_species_query(fips_list = state_codes$buffer_fips[i], 
                                                             taxon = taxon$hierarchy_homonym_string,
                                                             state_name = state_codes$state_name[i])
            
            # test for an empty result
            if(!is.null(state_nn) && !is.null(buffer_nn)) {
                # export the results
                bad_neighbor_list[[i]] <- export_bad_neighbor_list(state_list = state_nn, buffer_list = buffer_nn, 
                                                                   taxon = taxon$taxon, 
                                                                   state_name = state_codes$state_name[i],
                                                                   group_name = group_name)
            } 
            
        }
        
        # combine the results into a single tibble
        bad_neighbor_df <- dplyr::bind_rows(bad_neighbor_list)
        # combine the all-species
        all_species_df <- dplyr::bind_rows(state_species_list)
        
        # combine all the data frames
        full_bn_result[[taxon$taxon]] <- list(bad_neighbor = bad_neighbor_df,
                                           all_species = all_species_df)
    }
    # return the combined list of all results
    return(full_bn_result)
}