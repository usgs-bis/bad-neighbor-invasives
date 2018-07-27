

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