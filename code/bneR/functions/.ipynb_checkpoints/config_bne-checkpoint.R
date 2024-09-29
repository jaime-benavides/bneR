#' Prepare configuration file for BNE
#'
#' @export config_bne
#'
#########################################################################
config_bne <- function(sim_path = config$bne_core_path, pollutant = config$pollutant, 
                       time_var = config$time_var, 
                       sp_context = config$sp_context,
                       date_begin = config$date_begin,
                       date_end = config$date_end,
                       inp_base_model_names = config$input_base_model_names, 
                       exp_folder_path = config$exp_folder_path,
                       exp_id = config$expid,
                       scale_space_w_list = config$scale_space_w_list,
                       scale_time_w_list = config$scale_time_w_list,
                       scale_space_rp_list = config$scale_space_rp_list,
                       scale_time_rp_list = config$scale_time_rp_list,
                       scale_space_wvar_list = config$scale_space_wvar_list,
                       lambda_list = config$lambda_list,
                       lambda_rp_list = config$lambda_rp_list,
                       seed_list = config$seed_list,
                       n_sample_list = config$n_sample_list,
                       scale_space_w = config$scale_space_w,
                       scale_time_w = config$scale_time_w,
                       scale_space_rp = config$scale_space_rp,
                       scale_time_rp = config$scale_time_rp,
                       scale_space_wvar = config$scale_space_wvar,
                       lambda_w = config$lambda_w,
                       lambda_rp = config$lambda_rp,
                       seed = config$seed,
                       nfolds = config$nfolds,                       
                       bne_mode = config$bne_mode,   
                       sample_n = config$sample_n,
                       day_context = config$day_context,
                       run_bne_predict_loc = config$run_bne_predict_loc,
                       ev_dta_name = config$ev_dta_name){

  sink(paste0(sim_path, "config.m"))
  cat("%%%%%% BNE config file", "\n", "% input base model names (names and order need to match names and order", "\n", 
            "% from training and predictions files", "\n", sep = "")
  cat("pollutant = ","'", pollutant, "';", "\n", sep = "")
  cat("time_var = ","'", time_var, "';", "\n", sep = "")
  cat("sp_context = ","'", sp_context, "';", "\n", sep = "")
  cat(cat('date_begin = ["', sep = ""), cat(date_begin, sep = '","'), '"];', "\n", sep = "")
  cat(cat('date_end = ["', sep = ""), cat(date_end, sep = '","'), '"];', "\n", sep = "")
  cat("exp_desc = append(pollutant, '_', sp_context, '_', time_var, '_')", ";", "\n", sep = "")
  cat(cat('inp_base_model_names = ["', sep = ""), cat(inp_base_model_names, sep = '","'), '"];', "\n", sep = "")
  cat("exp_folder_path = ","'", substr(exp_folder_path, 1, nchar(exp_folder_path)-1), "';", "\n", sep = "")
  cat("expid = ","'", exp_id, "';", "\n", sep = "")
  cat("training = readtable(fullfile(exp_folder_path, expid, 'in', pollutant, 'training_datasets', 'training_cvfolds.csv'));", "\n", sep = "")  
  cat("%% grid-search", "\n", sep = "")
  cat("grid_search_name = exp_desc;", "\n", sep = "")
  cat("%% set parameter values we will consider (only if running grid search)", "\n", sep = "")
  cat(cat("scale_space_w_list = ", sep = ""), cat("[", scale_space_w_list[[1]], ",",scale_space_w_list[[2]],",",scale_space_w_list[[3]],"]", sep = ""), ";", "\n", sep = "")
  cat(cat("scale_time_w_list = ", sep = ""), cat("[", scale_time_w_list[[1]], ",",scale_time_w_list[[2]],",",scale_time_w_list[[3]],"]", sep = ""), ";", "\n", sep = "")
  cat(cat("scale_space_rp_list = ", sep = ""), cat("[", scale_space_rp_list[[1]], ",",scale_space_rp_list[[2]],",",scale_space_rp_list[[3]],"]", sep = ""), ";", "\n", sep = "")
  cat(cat("scale_time_rp_list = ", sep = ""), cat("[", scale_time_rp_list[[1]], ",",scale_time_rp_list[[2]],",",scale_time_rp_list[[3]],"]", sep = ""), ";", "\n", sep = "")
  cat(cat("scale_space_wvar_list = ", sep = ""), cat("[", scale_space_wvar_list[[1]], ",",scale_space_wvar_list[[2]],",",scale_space_wvar_list[[3]],"]", sep = ""), ";", "\n", sep = "")
 cat(cat("lambda_list = ", sep = ""), cat("[", lambda_list[1], ",",lambda_list[2],",",lambda_list[3],",",lambda_list[4],"]", sep = ""), ";", "\n", sep = "")
 cat(cat("lambda_rp_list = ", sep = ""), cat("[", lambda_rp_list[1], ",",lambda_rp_list[2],",",lambda_rp_list[3],",",lambda_rp_list[4],"]", sep = ""), ";", "\n", sep = "")
 cat(cat("seed_list = ", sep = ""), cat("[", seed_list[[1]],"]", sep = ""), ";", "\n", sep = "")
 cat(cat("n_sample_list = ", sep = ""), cat("[", n_sample_list[[1]], ",",n_sample_list[[2]],"]", sep = ""), ";", "\n", sep = "")
  cat("%% Cross-validation", "\n", sep = "")
  cat("% set parameters", "\n", sep = "")
  cat("scale_space_w = ",scale_space_w, ";", "\n", sep = "")
  cat("scale_time_w = ",scale_time_w, ";", "\n", sep = "")
  cat("scale_space_rp = ",scale_space_rp, ";", "\n", sep = "")
  cat("scale_time_rp = ",scale_time_rp, ";", "\n", sep = "")
  cat("scale_space_wvar = ",scale_space_wvar, ";", "\n", sep = "")
  cat("lambda_w = ",lambda_w, ";", "\n", sep = "")
  cat("lambda_rp = ",lambda_rp, ";", "\n", sep = "")
  cat("seed = ",seed, ";", "\n", sep = "")
  cat("nfolds = ",nfolds, ";", "\n", sep = "")    
  cat("bne_mode = ","'", bne_mode, "';", "\n", sep = "")  
  cat("sample_n = ",sample_n, ";", "\n", sep = "")
  cat("%% Prediction", "\n", sep = "")
    if(run_bne_predict_loc == "ev"){
      cat("output_name = append('", ev_dta_name, "_preds_',exp_desc);", "\n", sep = "")
     } else if (run_bne_predict_loc == "ref_grid"){
      cat("output_name = append('ref_grid_preds_',exp_desc);", "\n", sep = "")
    }
  cat("% input predictions are formed by target_name_1 + year (within predict function) + day (withn predict function) + target_name_2", "\n", sep = "")
  cat("target_name_1 = '", exp_folder_path, exp_id,'/', 'in','/', pollutant, '/', 'prediction_datasets', '/', "preds_'", ";", "\n", sep = "")
  if(run_bne_predict_loc == "ev"){
  cat("target_name_2 = '", ev_dta_name,'.csv',";", "\n", sep = "")
      } else if (run_bne_predict_loc == "ref_grid"){
  cat("target_name_2 = 'ref_grid.csv';", "\n", sep = "")
      }
  sink()
}
