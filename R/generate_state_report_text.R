# load the required library
require(tidyverse)

generate_state_report_text <- function(target_state, group_name, df) {
    # summarize the data frame
    sum_df <- df %>% 
        filter(state_name == target_state) %>% 
        group_by(state_name) %>% 
        summarize(bad_neighbor_count = sum(bad_neighbor_count), state_nn_species_count = sum(state_nn_species_count))

    
    state_text <- glue::glue("Bad Neighbors pose a threat of {sum_df$bad_neighbor_count} species of {group_name} that are invasive to {target_state}, but are not yet documented within the state.\n\nThere are currently {sum_df$state_nn_species_count} species of invasive {group_name} with a documented presence in the state.  Invasive species within {target_state} may pose a threat to surrounding states.\n\nClick on a name to open a tab with the species full distribution in <a href='https://bison.usgs.gov' target='_blank'>BISON</a>")
    
    # return the text
    return(state_text)
}