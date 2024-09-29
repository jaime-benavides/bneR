# Title     : config_run_bne
# Objective : build the configuration file of BNE
# Created by: Jaime Benavides
# Created on: 2/13/24
training_folds <- readr::read_csv(paste0(config$input_path, config$pollutant, "/", "training_datasets", "/", config$training_cvfolds_file_name))
config$nfolds <- length(unique(training_folds$fold))
config_bne(sim_path = config$bne_core_path)

