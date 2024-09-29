## ###########################################################
## create predictions dataset
## model and store in a R data object
## Last version: 09/28/2023
## Author: Sebastian Rowland, Lawrence Chilrud, Jaime Benavides
## ###########################################################

yyyy_begin <- as.numeric(config$date_begin[1])
sp_context_name <- config$sp_context
path_formatted_base_models <- config$path_formatted_base_models
mod_names <- config$input_base_model_names
num_models <- length(mod_names)
poll <- config$pollutant
if(config$pollutant == "nox"){
    poll <- "no2"
    }
mod_file_names <- paste0(mod_names, "_", poll, "_", yyyy_begin, "_", config$temp_resolution, ".csv")
mod_names_y <- gsub(".csv","",mod_file_names)

# 1.c.ii. refGrid keys
AOI <- config$sp_context



# 0.b small function to arrange by lat and lon
arrange_lat_lon <- function(d) {
  d %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
}

#### ------------------- ####
####  1. general set up  ####
#### ------------------- ####

# read in keys
if(config$running_ev_preds == "doing"){ref_name = config$ev_dta_name} else {ref_name = config$ref_grid_name}
for (j in 1:num_models){
   keys_file_name <- paste0(config$path_keys, "key_nn_", ref_name, "_", mod_names_y[j], ".fst")
  assign(paste0("key.refGrid.",AOI, ".",mod_names[j]), read_fst(keys_file_name) %>% 
    arrange_lat_lon())
}

# 1.d. generate refgrid # todo: adapt for when ref grid is not coming from input base model
preds <- get(paste0("key.refGrid.",AOI, ".",mod_names[config$ref_grid_from_input_base_model])) %>% 
  dplyr::select(ref_lat, ref_lon, ref_id) %>% 
  arrange_lat_lon()

  if(config$temp_resolution == "annual"){ # todo: adapt annual to work as daily does
  
  # 3A.b make year version of preds dataset 
  preds.year <- preds
  
  #### ------------ ####
  ####  3B. add input base models  ####
  #### ------------ ####
  
  for (j in 1:num_models){
    # process models
    assign(mod_names_y[j], readr::read_csv(paste0(path_formatted_base_models, mod_names[j], "/",
                                                  mod_file_names[j]))) 
  # 3B.c assign to prediction data 
  # remember the key is already in order wrt the prediction dataset 
  # because of the arranging we did earlier. 
  if(j == num_models){
    preds.year <- get(mod_names_y[j]) %>% 
      slice(get(paste0("key.refGrid.",AOI, ".",mod_names[j]))$baseModel_id) %>% 
      dplyr::select(-lat, -lon) %>%
      bind_cols(preds.year)
    } else {
      preds.year <- get(mod_names_y[j]) %>% 
        slice(get(paste0("key.refGrid.",AOI, ".",mod_names[j]))$baseModel_id) %>% 
        dplyr::select(paste0("pred_", mod_names[j])) %>%
        bind_cols(preds.year)
    }

  }
  
  
  # 3H.a. save it! 
  preds.year %>% 
    rename(lat = ref_lat, lon = ref_lon) %>%
      mutate(date_local = yyyy) %>%
      dplyr::select(lat, lon, date_local,
                    paste0("pred_", mod_names)) %>% 
    readr::write_csv(paste0(config$input_path, config$pollutant, "/", "prediction_datasets", "/",
                         paste0("preds_", config$temp_resolution,"_", yyyy, '_', AOI, '.csv')))
  } else if (config$temp_resolution == "daily") {

    maxDoY <- 365 
    if (yyyy_begin %in% c(2004, 2008, 2012, 2016, 2020, 2024)) {maxDoY <- 366}
     
     #days = 1:length(dates) # todo: transform to any combination of days
      for(day_loc in 1:length(dates)){
          
# 3A.a determine the date 
    activeDate <- lubridate::ymd(dates[day_loc])
    timeDatelt <- as.POSIXlt(activeDate)
     dayOfYear <-  timeDatelt$yday + 1
  # 3A.b. extract date components
      yyyy <- pad0(lubridate::year(activeDate))
      mm <- pad0(lubridate::month(activeDate))
      dd <- pad0(lubridate::day(activeDate))

  # 3A.c. isolate training to day of interest 
  prediction.day <- preds 
    # for each model, get the training day being flexible with data file content / valid for both day and annual with days
  for (j in 1:num_models){
       if(any(grepl(paste0(yyyy,mm,dd), list.files(paste0(path_formatted_base_models, mod_names[j]))))){
                     
         mod <- readr::read_csv(paste0(path_formatted_base_models, mod_names[j], "/",
                                                  mod_names[j], "_", poll, "_", yyyy,mm,dd,"_", config$temp_resolution, ".csv"))
           # sanity check to avoid duplicates
           if(any(duplicated(mod))){
           mod <- unique(mod)
               }
           } else if (any(grepl(yyyy, list.files(paste0(path_formatted_base_models, mod_names[j]))))){
             mod <- readr::read_csv(paste0(path_formatted_base_models, mod_names[j], "/",
                                                  mod_file_names[j]))
           # subset day
             mod <- mod %>%
                filter(date== activeDate)
           # sanity check to avoid duplicates
           if(any(duplicated(mod))){
           mod <- unique(mod)
               }
           } else {
           print("the period requested is not available for model ", mod_names[j])
           }
      
          colnames(mod)[c(which(colnames(mod)=="lat"), 
                                       which(colnames(mod)=="lon"))] <- c("baseModel_lat", "baseModel_lon")
         prediction.day <- prediction.day %>% 
         inner_join(get(paste0("key.refGrid.",AOI, ".",mod_names[j]))[,c("ref_id", "baseModel_id", "baseModel_lat", "baseModel_lon")], by = 'ref_id') %>% 
         left_join(mod[,c('baseModel_lat', 'baseModel_lon', mod_names[j])], by = c('baseModel_lat', 'baseModel_lon')) %>% 
            dplyr::select(-starts_with('baseModel_'))
      rm(mod)     
}      # format lat,lon,percent_of_year,julian_day,pred_mod1,pred_mod2,pred_mod3,…
       # preds_year_dayofyear.csv / Ex: preds_2005_001.csv     
          

          
      prediction.day <- prediction.day %>% 
      mutate(percent_of_year =  dayOfYear/maxDoY,
        julian_day =  as.numeric(timeDatelt - lubridate::parse_date_time('01/01/2005', 'dmy') )) 
   
    # 3H.b. arrange columns 
    prediction.day <- prediction.day %>% 
      rename(lat = ref_lat, lon = ref_lon) 
    prediction.day <- prediction.day[,c("lat", "lon", "percent_of_year", "julian_day", mod_names)]      
    colnames(prediction.day)[which(colnames(prediction.day) %in% mod_names)] <- paste0("pred_", mod_names)  
          if(config$running_ev_preds == "doing"){add = config$ev_dta_name} else {add = 'ref_grid'}
          readr::write_csv(prediction.day, paste0(config$input_path, config$pollutant, "/", "prediction_datasets", "/",
                 'preds_', yyyy,mm,dd, add, '.csv'))
           # 3H.f report day is done 
      print(activeDate)
      } # end day loop
      } # end if year/day

