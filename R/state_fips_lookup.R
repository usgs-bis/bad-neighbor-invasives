#' state_fips_lookup
#' 
#' A helper function to find FIPS codes and buffers using state name. The function can also works
#' in reverse to find state names and FIPS buffers using the state FIPS code.
#'
#' @param state_string a string with the name of the state to search
#' @param fips an optional 2-digit FIPS code to search
#'
#' @return A tibble with the state FIPS code, the FIPS codes for the states sharing a boundary, and
#'   the name of the state.
#' 
#'
#' @export
#'
#' @examples
#' # look up the data for Maryland
#' state_fips_lookup(state_string = "Maryland")
#' 
#' # the lookup is case insensitive 
#' state_fips_lookup(state_string = "maryland")
#' state_fips_lookup(state_string = "new york")
#' 
#' # the function will execute partial matches, but multiple results are possible.
#' # Multiple results are not necessarily useful as intended
#' state_fips_lookup(state_string = "carolina")
#' 
#' # better to use full names
#' state_fips_lookup(state_string = "North Carolina")
#' 
#' # look up by FIPS code as string
#' state_fips_lookup(fips = "02")
#' 
#' # the function will search single number FIPS
#' state_fips_lookup(fips = 1)
state_fips_lookup <- function(state_string, fips = NULL) {
    # Test for missing arguments
    if(missing(state_string) & missing(fips)) {
        stop("Either a state name or a FIPS code is required")
    }
    
    # Open the lookup table
    state_data <- readr::read_csv("data/state_lookup.csv")
    
    # search for the state name
    if(!missing(state_string)) {
        # make sure the state name is a title
        state_string <- stringr::str_to_title(state_string)
        
        # find results for the state by name
        state_data_filtered <- dplyr::filter(state_data, 
                                    stringr::str_detect(state_name, state_string)) 
    }
    
    # search for state data by FIPS code
    if(!is.null(fips)) {
        # a FIPS code HAS to be two digits (single digits have leading zeros)
        # promote to string 
        fips <- as.character(fips)
        
        # test length
        if(stringr::str_length(fips) == 1) {
            # add leading zero
            fips <- stringr::str_c("0", fips, collapse = "")
        }
        
        # search for the fips
        state_data_filtered <- dplyr::filter(state_data, 
                                    stringr::str_detect(state_fips, fips)) 
    }
    
    # the filter might return multiple matches (e.g. Virginia return both Virginia and West Virginia)
    if (nrow(state_data_filtered > 1)) {
        # see if the original state string has a space
        if(!stringr::str_detect(state_string, "\\s")) {
            # try to match only the result without spaces
            state_data_filtered <- dplyr::filter(state_data_filtered,
                                                 !stringr::str_detect(state_name, "\\s"))
            
            # return null if nothing is matched
            if(nrow(state_data_filtered) == 0) {
                return(NULL)
            }
        }
    }
    # return the result
    return(state_data_filtered)
}