## ###########################################################
## create training dataset
## model and store in a R data object
## Last version: 09/28/2023
## Author: Sebastian Rowland, Lawrence Chilrud, Jaime Benavides, Carlos Carrillo-Gallegos
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

# 1.c.ii. refGridConus keys
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


# 1.b. bring in aqs data
training <- read_fst(paste0(config$path_obs, config$obs_dta_name, "/", config$obs_dta_name, "_", config$temp_resolution ,"_curated_", yyyy_begin, ".fst")) 
if(config$temp_resolution == "daily"){
training <- training %>% 
  mutate(ddate = parse_date_time(date_local, 'ymd')) %>% 
  mutate(year = as.character(year(ddate)), month = pad0(month(ddate)), day = pad0(day(ddate)))
    }
# 1.c. read in keys
# 1.c.i. AQS keys

for (j in 1:num_models){
  # process models 
  keys_file_name <- paste0(config$path_keys, "key_nn_", "aqs", "_", mod_names_y[j], ".fst")
  assign(paste0("key.aqs.",mod_names[j]), read_fst(keys_file_name)  %>% 
    arrange_lat_lon())
}

  if(config$temp_resolution == "annual"){ 
  # 3A.a. isolate training to year of interest 
  training_df <- training %>% 
    dplyr::filter(as.numeric(date_local) == as.numeric(yyyy_begin)) # check here how data is structured

  #### ------------ ####
  ####  3B. add input base models  ####
  #### ------------ ####
  
  for (j in 1:num_models){
    # process models
    mod <- readr::read_csv(paste0(path_formatted_base_models, mod_names[j], "/",
                                                  mod_file_names[j]))
  
  # 3B.b. assign to training data 
  # 3B.b.i. attach key to the training data 
               colnames(mod)[c(which(colnames(mod)=="lat"), 
                                       which(colnames(mod)=="lon"))] <- c("baseModel_lat", "baseModel_lon")
  training_df <- training_df %>% 
    inner_join(get(paste0("key.aqs.",mod_names[j])), by = 'ref_id')
               training_df <- training_df %>% 
         left_join(mod[,c('baseModel_lat', 'baseModel_lon', mod_names[j])], by = c('baseModel_lat', 'baseModel_lon')) %>% 
            dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
      rm(mod)
  }
  
#  4. save training dataset 
  training_df %>%  # todo: update method for annual (done but not tested)
#  dplyr::select(-lat, -lon) %>%
#  rename(lat = ref_lat, lon = ref_lon) %>%
  dplyr::select(lat, lon, date_local, obs,
                all_of(mod_names),
                state, ref_id) %>%
    dplyr::rename(yyyy = date_local) %>%
  readr::write_csv(paste0(config$input_path, config$pollutant, "/", "training_datasets", "/",
                       paste0("training_", config$temp_resolution, ".csv")))
  } else if (config$temp_resolution == "daily") {
    training.full <- data.frame()
    maxDoY <- 365
    if (config$date_begin[1] %in% c(2004, 2008, 2012, 2016, 2020, 2024)) {maxDoY <- 366}

      for(day_loc in 1:length(dates)){
 #### ---------------------------- ####
  ####  3A. process date variables  ####
  #### ---------------------------- ####

# 3A.a determine the date 
    activeDate <- lubridate::ymd(dates[day_loc])
    timeDatelt <- as.POSIXlt(activeDate)
     dayOfYear <-  timeDatelt$yday + 1
  # 3A.b. extract date components
      yyyy <- pad0(lubridate::year(activeDate))
      mm <- pad0(lubridate::month(activeDate))
      dd <- pad0(lubridate::day(activeDate))

  # 3A.c. isolate training to day of interest 
  training.day <- training %>%
    dplyr::filter(as.numeric(year) == yyyy &
                    as.character(month) == mm &
                    as.character(day) == dd)
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
         training.day <- training.day %>% 
         inner_join(get(paste0("key.aqs.",mod_names[j])), by = 'ref_id')
               training.day <- training.day %>% 
         left_join(mod[,c('baseModel_lat', 'baseModel_lon', mod_names[j])], by = c('baseModel_lat', 'baseModel_lon')) %>% 
            dplyr::select(-ref_lat, -ref_lon, -starts_with('baseModel_'))
      rm(mod)
}
  training.day <- training.day %>%
    mutate(yyyy = yyyy,
           mm = mm, 
           dd = dd,
        percent_of_year = dayOfYear/maxDoY,
           julian_day =  as.numeric(timeDatelt - lubridate::parse_date_time('01/01/2005', 'dmy') ))
            
    training.day <- training.day %>%
    dplyr::select(lat, lon, yyyy, mm, dd, percent_of_year, julian_day, obs,
                  all_of(mod_names), state, ref_id)
 # 3H.f report day is done 
  print(activeDate)

  # 3H.g return training data 
  training.full <- rbind(training.full,training.day)
      }
      training.full %>%
  readr::write_csv(paste0(config$input_path, config$pollutant, "/", "training_datasets", "/",
                       paste0("training_", config$temp_resolution, ".csv")))
      }

