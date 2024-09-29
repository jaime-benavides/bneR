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

#### ------------------------------------------- ####
####  0. IMPORT PACKAGES AND SET GLOBAL OBJECTS  ####
#### ------------------------------------------- ####

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

#### ------------------------ ####
####  7. create keys for AQS  ####
#### ------------------------ ####


# since there are repeated locations for AQS, this key is used to identify 
# relevant base model locations, not all locations. 
# this allows us to effectively filter the annual base models before doing the 
# spatial join 

# read spatial context ids
sp_context_ids <- readRDS(paste0(config$input_static_path, "sp_context_ids.rds"))

if(config$running_ev_keys == "doing"){
# subset of aqs locations to spatial context
dt.loc <- sp_context_ids$ev
ref_name <- paste0(config$ev_dta_name, '_', config$temp_resolution) 
out_path <- paste0(config$path_keys, "key_nn_", config$ev_dta_name, "_")
} else if (config$running_obs_keys == "doing"){
dt.loc <- sp_context_ids$obs
ref_name <- paste0(config$obs_dta_name, '_', config$temp_resolution)
out_path <- paste0(config$path_keys, "key_nn_", config$obs_dta_name, "_")
    }


# make list 
for (j in 1:num_models){
  print(paste0("working on input base model ", mod_names[j]))
  # set output file name
  out_file_name <- paste0(out_path, mod_names_y[j])
  # create keys for AQS sites   
  if(!file.exists(out_file_name)){
    print(paste0("creating key for input base model ", mod_names[j], " for AQS sites."))
  createKey(ref.df = dt.loc, refName = ref_name, 
           baseModel.df = sp_context_ids[[mod_names[j]]], baseModelName = mod_names_y[j], 
           poll = config$pollutant, out_file_name) 
  }
  
  }