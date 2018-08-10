# load the required source code
source("R/export_bad_neighbor_list.R")
source("R/state_all_species_query.R")
source("R/state_bad_neighbor_query.R")

#' process_bad_neighbor_groups
#' 
#' This function is a wrapper function executing all necessary steps in generating the lists of bad neighbors.
#'
#' @param group_name A string. The informal name of the taxon group to process (e.g. "Tree/Shrub")
#' @param taxa A tibble. A table with a the name of the taxon to process along with the associated heirarchy homonym TSN string. The tibbles are typically loded from lookup tables in the "data" directory (see example). The heirarchy homonym TSN string can also be obtained from the [ITIS](https://www.itis.gov) website by searching for the taxon. 
#'
#' @return A list of tibbles (data frames) containing the summary results for the Bad Neighbor Analysis from the export_bad_neighbor_list() function, and a summary tibble for all species currently documented in states from the state_all_species_query() function.
#' 
#' @export
#'
#' @examples
#' 
#' # load lookup tables
#' taxa <- readr::read_csv("data/tree_shrub_heirarchy_strings.csv")
#'
#' trees_results <- process_bad_neighbor_groups("tree_shrub", taxa = taxa)
#' 
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