## ###########################################################
## create keys linking input base models and AQS sites
## model and store in a R data object
## Last version: 09/28/2023
## Author: Sebastian Rowland, Jaime Benavides
## ###########################################################


# N.1. goal
# The point of this script is to create various "key" files:
# 
# The keys map EPA monitor locations to
# their nearest neighbour input base models <lat, lon> coordinate pairs, along with 
# recording the index of the input base models lat-lon pair. In this way, a "key" is made, allowing
# someone to extract only the points of interest from a input base model .rda file using the
# "model_index" column from the key. 
# the keys are stored in the inputs/[pollutant]/keys folder, and follow this naming convention: 
# [aqs/refGrid] _JS_key_nn_ [timeWindow]

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

# read spatial context ids
sp_context_ids <- readRDS(paste0(config$input_static_path, "sp_context_ids.rds"))

#### ------------------------ ####
####  7. create keys for ref grid  ####
#### ------------------------ ####

# 8.a. bring in the refGrid, which is already adapted to the spatial context in create_reference_grid.R
refGrid <- fst::read_fst(paste0(config$input_static_path,  
                              config$ref_grid_name, '.fst'))

# make list 
for (j in 1:num_models){
  print(paste0("working on input base model ", mod_names[j]))
  # set output file name
  out_file_name <- paste0(config$path_keys, "key_nn_", config$ref_grid_name, "_", mod_names_y[j])
# create keys for refGrid
  if(!file.exists(paste0(out_file_name, ".fst"))){
    print(paste0("creating key for input base model ", mod_names[j], " for refGrid ",  config$ref_grid_name))
  createKey(ref.df = refGrid, refName = config$ref_grid_name,
            baseModel.df = sp_context_ids[[mod_names[j]]], baseModelName = mod_names_y[j], 
            poll = config$pollutant, out_file = out_file_name)
  }
}



