# Title     : bner_main.R
# Objective : Run BNE preproc over the US or any other area within US during 1 year
# Created by: Jaime Benavides
# Created on: 09/28/2023

### building air quality model ensemble simulation using BNE v2.0
# it is assumed that code has been stored at exp_id/code 
# it is assumed that input base models and AQS data have been formatted according to BNE guidelines
# and that reference grids have been prepared and located at exp_id/inputs

############################################################
#                                                          #
#                Load configuration file                   #
#                                                          #
############################################################  
configFile_path <- "config_file.yml"
#configFile_path <- "/data0/shr/bne/exp/exp_0095/code/bneR/config_file.yml"
args = commandArgs(TRUE) # commenting area of config file provided by command line
if (length(args) > 0) {
  configFile_path <- args[1]
} else {
  cat("\n***WARNING: no ConfigFile was provided. Using default ConfigFile.\n")
}
config <- yaml::yaml.load_file(configFile_path)
rm(configFile_path)
config$sim_folder_path <- paste0(config$exp_folder_path, config$expid, "/")
config$code_path <- paste0(config$sim_folder_path, "code/")
config$preproc_path <- paste0(config$code_path, "bneR/")
config$bne_core_path <- paste0(config$code_path, "bne_core/")
config$input_path <- paste0(config$sim_folder_path, "in/")
config$output_path <- paste0(config$sim_folder_path, "out/")
config$sp_context_path <- paste0(config$data_path, "static/geom/sp_context/")
config$input_static_path <- paste0(config$input_path, "static/")
config$path_formatted_base_models <- paste0(config$data_path, config$pollutant, '/base_models/', config$temp_resolution, "/", 
                                            'formatted', "/")
if(is.numeric(config$ref_grid_resolution)){
    config$ref_grid_name <- paste0("refGrid_", config$sp_context, "_", gsub('\\.', '_', config$ref_grid_resolution), "_deg") 
    }else{
   config$ref_grid_name <- paste0("refGrid_", config$sp_context, "_", config$input_base_model_names[config$ref_grid_from_input_base_model])       
        }
config$path_obs <- paste0(config$data_path, config$pollutant, "/","obs", "/", config$temp_resolution, "/", "formatted", "/") # curated observations need to be saved in this path with the following finle name structure paste0("aqs_", config$temp_resolution ,"_curated_", config$temp_context, ".fst")
config$path_keys <- paste0(config$input_path, config$pollutant, "/", "keys/") # keys will be saved here once generated with file name paste0(config$path_keys, "key_nn_", "aqs", "_", mod_names_y[j], ".fst") for aqs and file 
config$path_training <- paste0(config$input_path, config$pollutant, "/", "training_datasets", "/")
config$path_prediction <- paste0(config$input_path, config$pollutant, "/", "prediction_datasets", "/")

############################################################
#                                                          #
#                init_BNER                                 #
#                                                          #
############################################################  
source(paste0(config$preproc_path, "init_bner.R"))
##################    generating keys  preprocesses   ################

  ############################################################
  #                                                          #
  #                create reference grid                     #
  #                                                          #
  ############################################################  
  if(config$run_create_ref_grid){
  print("create reference grid")
  if (!file.exists(paste0(config$input_static_path, config$ref_grid_name, '.fst'))) {
    source(paste0(config$preproc_path, "make_reference_grid.R"))
  } else {
   print("create reference grid not needed because reference grid already present")
  }
}
  ############################################################
  #                                                          #
  #                create keys at AQS sites                  #
  #                                                          #
  ############################################################  
  if(config$run_keys_obs){
  print("create keys linking input base models and AQS sites")
  # if (file.exists(file_names$roads.shp_file)) {
              config$running_obs_keys <- "doing"  
    source(paste0(config$preproc_path, "make_keys_obs_sites.R"))
              config$running_obs_keys <- "done"  
  # } else {
  #   print("necessary inputs to run process were not found")
  # }
  }

    ############################################################
  #                                                          #
  #                create keys at ev sites                  #
  #                                                          #
  ############################################################  
  if(config$run_keys_ev){
  print("create keys linking input base models and ev sites")
        config$running_ev_keys <- "doing"  
  # if (file.exists(file_names$roads.shp_file)) {
    source(paste0(config$preproc_path, "make_keys_obs_sites.R"))
        config$running_ev_keys <- "done"  
  # } else {
  #   print("necessary inputs to run process were not found")
  # }
  }

############################################################
#                                                          #
#                create keys at reference grid             #
#                                                          #
############################################################  
if(config$run_keys_preds){
print("create keys linking input base models and reference grid")
# if (file.exists(file_names$roads.shp_file)) {
source(paste0(config$preproc_path, "make_keys_ref_grid.R"))
# } else {
#   print("necessary inputs to run process were not found")
# }
}

############################################################
#                                                          #
#                create training dataset                   #
#                                                          #
############################################################  
if(config$run_make_training_dta){
  print("create training dataset")
  # if (file.exists(file_names$roads.shp_file)) {
  source(paste0(config$preproc_path, "make_training_dta.R"))
  print("identifying spatial folds")    
  source(paste0(config$preproc_path, "identify_spatial_folds.R"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}

############################################################
#                                                          #
#                create prediction dataset                 #
#                                                          #
############################################################  
if(config$run_make_preds_dta){
  print("create predictions dataset")
  # if (file.exists(file_names$roads.shp_file)) {
  source(paste0(config$preproc_path, "make_pred_dta.R"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}

############################################################
#                                                          #
#                create prediction dataset EV                #
#                                                          #
############################################################  
if(config$run_make_preds_ev){
  print("create predictions dataset for EV")
  config$running_ev_preds <- "doing"  
  # if (file.exists(file_names$roads.shp_file)) {
  source(paste0(config$preproc_path, "make_pred_dta.R"))
  config$running_ev_preds <- "done"  
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}


############################################################
#                                                          #
#           create BNE configuration file                  #
#                                                          #
############################################################ 

if (any(c(config$run_bne_grid_search, config$run_bne_cv, config$run_bne_predict)  == TRUE)) {
    print("create BNE configuration file")
  source(paste0(config$preproc_path, "config_run_bne.R"))
}

############################################################
#                                                          #
#                Run BNE grid search                       #
#                                                          #
############################################################  

if(config$run_bne_grid_search){
  print("running bne grid search")
  # if (file.exists(file_names$roads.shp_file)) {
matlabr::run_matlab_script(paste0(config$bne_core_path, "grid_search.m"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}

############################################################
#                                                          #
#                Run BNE cross-validation                  #
#                                                          #
############################################################  
if(config$run_bne_cv){
  print("running bne cross-validation")
  # if (file.exists(file_names$roads.shp_file)) {
matlabr::run_matlab_script(paste0(config$bne_core_path, "cv_res.m"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}

############################################################
#                                                          #
#                Run BNE prediction                        #
#                                                          #
############################################################ 

if(config$run_bne_predict_ref_grid){
  print("running bne predict over reference grid")
  # if (file.exists(file_names$roads.shp_file)) {
    config$run_bne_predict_loc <- "ref_grid"
matlabr::run_matlab_script(paste0(config$bne_core_path, "make_predict.m"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}

if(config$run_bne_predict_ev){
  print("running bne predict over external validation sites")
  print("create new BNE configuration file")
  config$run_bne_predict_loc <- "ev"
  source(paste0(config$preproc_path, "config_run_bne.R"))
matlabr::run_matlab_script(paste0(config$bne_core_path, "make_predict.m"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}


if(config$run_postprocess){
  print("create postprocess datasets")
  # if (file.exists(file_names$roads.shp_file)) {
  source(paste0(config$preproc_path, "postprocess.R"))
  # } else {
  #   print("necessary inputs to run process were not found")
  # } 
}

