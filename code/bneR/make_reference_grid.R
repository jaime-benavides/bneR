## ###########################################################
## create reference grid from input base model or config input
## model and store in a R data object
## Last version: 11/13/2023
## Author: Sebastian Rowland, Jaime Benavides
## ###########################################################


# N.1. goal
# The point of this script is to create reference grid:

# config
path_formatted_base_models <- config$path_formatted_base_models
mod_names <- config$input_base_model_names
num_models <- length(mod_names)
poll <- config$pollutant
if(config$pollutant == "nox"){
    poll <- "no2"
    }
mod_file_names <- paste0(mod_names, "_", poll, "_", config$temp_context, "_", config$temp_resolution, ".csv")
mod_names_y <- gsub(".csv","",mod_file_names)

AOI <- config$sp_context

# if user wants to create reference grid from input base model

# else if user wants to create reference grid from spatial resolution
  # 2b. mini function
  create_rectangle_grid <- function(aoi.sf, gridDiameter) {
    # get min and max coordinates
    min.lat <- sf::st_bbox(aoi.sf)$ymin
    max.lat <- sf::st_bbox(aoi.sf)$ymax
    min.lon <- sf::st_bbox(aoi.sf)$xmin
    max.lon <- sf::st_bbox(aoi.sf)$xmax
    # create a dataframe of grid centroids
    dta <- expand_grid(lat = seq(min.lat, max.lat, by = gridDiameter), 
                       lon = seq(min.lon, max.lon, by = gridDiameter))
    # return resulting rectangular grid
    return(dta)
  }

# read spatial context information
sp_context_ids <- readRDS(paste0(config$input_static_path, "sp_context_ids.rds"))


if(is.numeric(config$ref_grid_resolution)){
  
  # 2c. create rectangular grids
  refGrid.rect <- purrr::pmap(list(list(sp_context), config$ref_grid_resolution), 
              create_rectangle_grid) %>% 
    dplyr::bind_rows()
  
  #---------------------------------------------#
  #### 3. restrict centroids to shape of AOI ####
  #---------------------------------------------#
  
  # remember, the original aoi is not a rectangle
  
  # 3a intersect with original shape of aoi
  refGrid.aoi <- refGrid.rect %>% 
      sf::st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326")) %>% 
      sf::st_join(sp_context, join = st_within)
  
  #-----------------------#
  #### 4. save refGrid ####
  #-----------------------#
  
  # 4a. convert refGrid back to dataframe
  # at the end of the day, we only need the lat and lon
  refGrid.aoi <- refGrid.aoi %>%
    dplyr::mutate(lon = st_coordinates(.)[,1], 
           lat = st_coordinates(.)[,2]) %>%
    as.data.frame() %>% 
    dplyr::select(lat, lon)  
  
  # 4b. save the centroids
  refGrid.aoi %>% 
    fst::write_fst(paste0(config$input_static_path,  
                              config$ref_grid_name, '.fst'))
} else {
        j <- config$ref_grid_from_input_base_model
             refGrid.aoi <- sp_context_ids[[mod_names[j]]][c("lat","lon")]
             refGrid.aoi %>% 
               as.data.frame() %>% 
               dplyr::select(lat, lon) %>% 
                  fst::write_fst(paste0(config$input_static_path,  
                              config$ref_grid_name, '.fst'))
           }

