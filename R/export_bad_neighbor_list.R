

export_bad_neighbor_list <- function(state_list, buffer_list, state_name, taxon) {
    # first intersect the two results to find the species not in the state
    species_intersect <- dplyr::anti_join(buffer_list, state_list, by="tsn")
    
    # turn the result into json
    species_json <- jsonlite::toJSON(species_intersect)
    
    # replace spaces in state names with underscore
    state_name <- str_replace_all(state_name, "\\s", "_")
    
    # create a file
    out_name <- file.path("result_json", taxon, stringr::str_c(state_name, taxon, "bad_neighbor.json", sep = "_"))
    
    # test if the directory exists
    if(!dir.exists(dirname(out_name))) {
        # create the directory if needed
        dir.create(dirname(out_name))
    }
        
    # write the json to file
    readr::write_lines(species_json, path = out_name)
    
}