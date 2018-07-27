

generate_bn_count_map <- function(df, taxon) {
    # open the spatial data for US states
    us_states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
    
    # project to albers equal area for the U.S.
    # EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
    # +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
    us_states_alb <- sf::st_transform(us_states, crs = 102003)
    
    # mutate the state_name to match the map data
    df <- dplyr::mutate(df, state_name = stringr::str_to_lower(stringr::str_replace_all(df$state_name, "_", " ")))
    
    # join the data
    us_states_alb <- dplyr::left_join(us_states_alb, df, by = c("ID" = "state_name"))
    
    # build a title, replacing _ as needed
    t <- stringr::str_c("Threat Assessment of Invasion by",
                        stringr::str_replace_all(taxon, "_", " "), "Bad Neighbors",
                        sep = " ") 
        
    # create a plot with states
    p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = us_states_alb, ggplot2::aes( fill = species_count)) +
        ggplot2::ggtitle(t) +
        # ggplot2::scale_fill_gradient(name = "Bad Neighbors", low = "#fff7fb", high = "#014636") +
        # ggplot2::scale_fill_gradient(name = "Bad Neighbors", low = "#fee8c8", high = "#e34a33") +
        ggplot2::scale_fill_distiller(name = "Bad Neighbors", palette = "RdYlGn") +
        # ggthemes::theme_map()+
        ggplot2::scale_y_continuous(breaks = c(25, 35, 45)) +
        ggplot2::scale_x_continuous(breaks = c(-120, -100, -70)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
    
    # return the plot object
    return(p)
}