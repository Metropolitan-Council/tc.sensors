#' Generate spatial lines dataset from sensor configuration
#'
#' @param config data.table, sensor configuration for multiple sensors
#' @param class character, the output class. Default is `"sf"`.
#'  Other option is `"sp"`.
#'
#' @return an `sp` or `sf` object with poly lines for each corridor
#' @export
#'
generate_spatial_lines <- function(config,
                                   class = "sf"){

  config_df <- as.data.table(config)[
    , `:=`(corridor_id = paste0(corridor_route, corridor_dir))][
      , .(detectors = paste(detector_name, collapse = ",")), keyby = .(corridor_id)]

  config_coords <- as.data.table(config)[
    , `:=`(corridor_id = paste0(corridor_route, corridor_dir))][
      , .(corridor_id, r_node_lat, r_node_lon)
    ]

  config_coords_split <- split(config_coords, config_coords$corridor_id)

  # create lines -------------------------------------------------------------------
  created_lines <- map(c(1:length(config_coords_split)), function(i){

   sp::Line(coords = cbind(as.numeric(config_coords_split[[i]]$r_node_lon),
                           as.numeric(config_coords_split[[i]]$r_node_lat)))
  })

  # create line IDS ----------------------------------------------------------------
  config_lines <- vector("list", length(config_coords_split))

  created_lines_id <- map(c(1:length(config_coords_split)), function(i){
    config_lines[[i]] <- sp::Lines(slinelist = created_lines[[i]], ID = config_df[i, corridor_id])
  })

  config_lines_sp <- sp::SpatialLines(created_lines_id)

  if(class == "sf"){
    config_lines_sf <- sf::st_as_sf(config_lines_sp)
    config_lines_sf$corridor_id <- config_df$corridor_id
    return(config_lines_sf)
  }

  return(config_lines_sp)
}
