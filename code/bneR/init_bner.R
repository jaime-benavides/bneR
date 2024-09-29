# Title     : init_bner.R
# Objective : Inititate folders and necessary objects across scripts
# Created by: Jaime Benavides 
# Created on: 9/28/23



## load packages
source(paste0(config$preproc_path, "import_packages_set_global_objects.R"))


print("create simulation folders")

mkdirs(config$input_path)
mkdirs(config$output_path)
mkdirs(config$input_static_path) #TODO: add all necessary paths, also geom sp_context...
mkdirs(config$path_keys)
mkdirs(config$path_training)
mkdirs(config$path_prediction)

# config
path_formatted_base_models <- config$path_formatted_base_models
mod_names <- config$input_base_model_names
num_models <- length(mod_names)
poll <- config$pollutant
if(config$pollutant == "nox"){
    poll <- "no2"
    }
mod_file_names <- paste0(mod_names, "_", poll, "_", config$date_begin[1], "_", config$temp_resolution, ".csv")
mod_names_y <- gsub(".csv","",mod_file_names)

config$running_ev_keys <- "on_hold"
config$running_obs_keys <- "on_hold"
config$running_ev_preds <- "on_hold"


if(!file.exists(paste0(config$input_static_path, config$sp_context, '.shp'))){ # this is forced
    system(paste0("ln -s ", config$sp_context_path, config$sp_context, "/*", " ", config$input_static_path))
    if(!file.exists(paste0(config$input_static_path, config$sp_context, '.shp'))){
            print("the shapefile for the spatial context does not exist in the common repository.")
        }
} else {
    print("spatial context already present")
    }

print("set temporal context")
dates <- data_frame_dates(date_begin = config$date_begin,
                               date_end = config$date_end)

dates <- unique(paste0(as.character(strftime(dates[, 1], format="%Y", tz = "UTC")),
                       as.character(strftime(dates[, 1], format="%m", tz = "UTC")),
                       as.character(strftime(dates[, 1], format="%d", tz = "UTC"))))

print("set spatial context")


sp_context  <- sf::st_read(paste0(config$input_static_path, 
                                  paste0(config$sp_context, '.shp'))) %>% 
  sf::st_transform(., crs=st_crs('epsg:4326'))
# 0.b.ii get the bounding box 
bbox.sp_context <- list(xMin = sf::st_bbox(sp_context)$xmin[[1]], 
                        xMax = sf::st_bbox(sp_context)$xmax[[1]], 
                        yMin = sf::st_bbox(sp_context)$ymin[[1]], 
                        yMax = sf::st_bbox(sp_context)$ymax[[1]])

if (!file.exists(paste0(config$input_static_path, "sp_context_ids.rds"))) {
print("running process of identification for data within the spatial context, it can take some minutes, we are intersecting datasets...")

# initialize sp_context_ids list to store reference ids for obs and input base models
sp_context_ids <- list()
# If input base models are larger than spatial_context -> intersect them and get the position in the original dataset for the rows in the subset.

# create list to store the ids within the spatial context for each input dataset (obs and input base models)
# 7.c. reduce to just unique locations


# obs
   obs_all <- read_fst(paste0(config$path_obs, config$obs_dta_name, "/", config$obs_dta_name,"_", config$temp_resolution ,"_curated_", config$date_begin[1], ".fst")) %>%
  dplyr::mutate(obs_in_row_id = 1:nrow(.))

sp_context_ids$obs <- obs_context(obs = obs_all, sp_context)

    if(config$run_keys_ev == TRUE){
   ev_all <- read_fst(paste0(config$path_obs, config$ev_dta_name, "/", config$ev_dta_name,"_", config$temp_resolution ,"_curated_", config$date_begin[1], ".fst")) %>%
  dplyr::mutate(obs_in_row_id = 1:nrow(.))   
    sp_context_ids$ev <- obs_context(obs = ev_all, sp_context)    
}
# input base models
if(config$run_sp_context){
for (j in 1:num_models){
  print(paste0("working on input base model ", mod_names[j]))
  # process models
    # if model files are stored daily
  if(any(grepl("(\\d{8,})", list.files(paste0(path_formatted_base_models, mod_names[j]))))){
  file_ref_name <-  list.files(paste0(path_formatted_base_models, mod_names[j]))[which(grepl("(\\d{8,})", list.files(paste0(path_formatted_base_models, mod_names[j]))))[1]] # reading only first file
         assign(mod_names_y[j], readr::read_csv(paste0(path_formatted_base_models, mod_names[j], "/",file_ref_name)))
           } else {     # if model files are stored by year
             assign(mod_names_y[j], readr::read_csv(paste0(path_formatted_base_models, mod_names[j], "/",
                                                  mod_file_names[j]))) 
            # subset to only distinct lat,lon
      assign(mod_names_y[j], get(mod_names_y[j]) %>% 
          dplyr::select(lat, lon) %>% 
          distinct())
           }
mod <- get(mod_names_y[j])  
mod$cell_id <- 1:nrow(mod)
coordinates(mod) = ~lon+lat
mod.sf = sf::st_as_sf(mod)
sf::st_crs(mod.sf) <- "EPSG:4326"
mod.sf$lon <- mod$lon
mod.sf$lat <- mod$lat    
mod_cntxt_pos <- sapply(sf::st_intersects(mod.sf, sp_context),function(x){length(x)>0})
mod_contxt <- mod.sf[mod_cntxt_pos, ]
mod_contxt_df <- mod_contxt
sf::st_geometry(mod_contxt_df) <- NULL
cntxt_mod_cell_ids <- mod_contxt_df[,c("lon", "lat", "cell_id")]
  sp_context_ids[[mod_names[j]]] <- cntxt_mod_cell_ids 
  rm(list = mod_names_y[j], mod)
  }
saveRDS(sp_context_ids, paste0(config$input_static_path, "sp_context_ids.rds"))
} else {
    print("it is not need to run process of identification for data within the spatial context because sp_context_ids already exists.")
    }
    }




