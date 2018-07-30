

#' export_bad_neighbor_list
#'
#' @param state_list a list of species scientific names with associated TSN for a given state.  This should be the output from the state_bad_nighbor_query function for a single state.
#' @param buffer_list a list of species scientific names with associated TSN for a group of buffer states.  This should be the output from the state_bad_nighbor_query function for a group of buffer states. 
#' @param state_name the name of the state to process
#' @param taxon the name of the taxon to process
#' @param group_name (Optional) name of a group or taxa (e.g. Grasses) to process.
#'
#' @return A tibble (data frame) with the summary of the results for each state, including the total of bad neighbors, the associated total occurrence of non-native species, and the number of non-native species already documented in the state.
#' @export
#'
#' @examples
#' 51 FIPS code for Virginia
#' list of surrounding state FIPS codes
#' buff_states <- "(24 11 37 47 21 54)"
#' "*\\-202422\\-*" hierarchy_homonym_string for all plants
#' va_nn <- state_bad_neighbor_query(fips_list = 51, taxon = "*\\-202422\\-*")
#'
#' buff_nn <- state_bad_neighbor_query(fips_list = buff_states, taxon = "*\\-202422\\-*")
#'
#' export_bad_neighbor_list(state_list = va_nn, buffer_list = va_nn_buffer, taxon = taxon = "*\\-202422\\-*", state_name = "Virginia")
#' 
export_bad_neighbor_list <- function(state_list, buffer_list, state_name, taxon, group_name = NULL) {
    
    # make sure the lists are not empty
    if(!is.null(state_list) && !is.null(buffer_list)) {
    
        # first intersect the two results to find the species not in the state
        species_intersect <- dplyr::anti_join(buffer_list, state_list, by="tsn")
        
        # replace spaces in state names with underscore
        state_name <- stringr::str_replace_all(state_name, "\\s", "_")
        
        # Create a list to hold the results and metadata
        species_output <- list(state_name = state_name,
                             bad_neighbor_count = nrow(species_intersect),
                             nn_abundance = sum(species_intersect$cnt),
                             state_nn_species_count = nrow(state_list),
                             species_list = species_intersect)
        
        # turn the result into json
        species_json <- jsonlite::toJSON(species_output)
        
        # create a file
        if(!is.null(group_name)) {
            out_name <- file.path("result_json", group_name, taxon, stringr::str_c(state_name, taxon, "bad_neighbor.json", sep = "_"))    
        } else {
            out_name <- file.path("result_json", taxon, stringr::str_c(state_name, taxon, "bad_neighbor.json", sep = "_"))
        }
        
        
        # test if the directory exists
        if(!dir.exists(dirname(out_name))) {
            # create the directory and parents if needed
            dir.create(dirname(out_name), recursive = TRUE)
        }
            
        # write the json to file
        readr::write_lines(species_json, path = out_name)
        
        # return just the metadata about the dataset
        t <- tibble::tibble(state_name = state_name,
                            bad_neighbor_count = nrow(species_intersect),
                            nn_abundance = sum(species_intersect$cnt),
                            state_nn_species_count = nrow(state_list))
        return(t)

    }
}