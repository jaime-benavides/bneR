## ###########################################################
## postprocess bne results
## Last version: 04/13/2024
## Author: Jaime Benavides
## ###########################################################

yyyy <- config$date_begin[1]
sp_context_name <- config$sp_context
path_formatted_base_models <- config$path_formatted_base_models
mod_names <- config$input_base_model_names
num_models <- length(mod_names)
poll <- config$pollutant
if(config$pollutant == "nox"){
    poll <- "no2"
    }
mod_file_names <- paste0(mod_names, "_", poll, "_", config$date_begin[1], "_", config$temp_resolution, ".csv")
mod_names_y <- gsub(".csv","",mod_file_names)
raster_path <- paste0(config$output_path, "raster/")
mkdirs(raster_path) # todo: move to init if postprocess

# create raster file containing average values for the requested period

out_files <- list.files(path = config$output_path)[str_detect(list.files(path = config$output_path) ,"\\d+")]

dates <- gsub("_","", gsub("\\D", "_", as.character(out_files)))
mkdirs(paste0(config$output_path, "partition/")) # todo: move to init

# do partition by month of the year
out_files_month <- numeric()
for(i in 1:length(out_files)){
char_loc <- c(nchar(out_files[i])-6,nchar(out_files[i])-4)
day <- as.numeric(str_sub(out_files[i], char_loc[1], char_loc[2]))
date <- as.Date(day-1, origin=paste0(config$date_begin[1], "-01-01"))
#year <- strftime(date, "%y")
month <- strftime(date, "%m")
out_files_month[i] <- month  
}

if(any(is.na(out_files_month))){
na_pos <- which(is.na(out_files_month))
out_files_month <- out_files_month[-na_pos]
out_files  <- out_files[-na_pos]
}
months <- unique(out_files_month)
for(m in 1:length(months)){
results_m <- arrow::open_dataset( # todo: make sure all users have access to arrow, why 90,91,122 not available?  
sources = paste0(config$output_path, out_files[which(out_files_month == months[m])]), 
col_types = arrow::schema(ISBN = arrow::string()),
format = "csv"
)
results_m |> 
  dplyr::select(lat, lon, time, y_mean, y_sd, starts_with("w_mean"), starts_with("pred_")) |> # todo: add weights and predictions for input base models
  arrow::write_dataset(path = paste0(config$output_path, "partition/", months[m]), format = "parquet")
}

# create monthly summary rasters
postproc_write_raster <- function(dta = dta, vars = c('y_mean_mean', 'y_sd_mean'), temp_aggregation = "monthly"){ # temp_aggregation opts are "month" or "annual"
  xy <- dta[,c('lon','lat')]
  points <- sp::SpatialPointsDataFrame(coords = xy, data = dta, proj4string = sp::CRS("EPSG:4326"))
  pixels <- sp::SpatialPixelsDataFrame(points, tolerance = 0.916421, as.data.frame(dta))
  for(r in 1:length(vars)){
  var <- colnames(dta)[which(colnames(dta) == vars[r])]
  if (var %in% colnames(dta)){
  raster <- raster::raster(pixels[,vars[r]])
# write summary raster/s
      if (temp_aggregation == "monthly"){
  raster::writeRaster(raster, filename=paste0(raster_path, vars[r], "_", config$date_begin[1],"_", months[m], ".tif"), format="GTiff", overwrite=TRUE)
          } else if (temp_aggregation == "annual"){
  raster::writeRaster(raster, filename=paste0(raster_path, vars[r], "_", config$date_begin[1], "_", "annual", ".tif"), format="GTiff", overwrite=TRUE)
              } else {
          print("the temporal aggregation requested is not available.")
          }
} else {
      print("variable ", var, " is not available.") 
      }
      }
    }

# create rasters
for(m in 1:length(months)){
results_pq <- arrow::open_dataset(paste0(config$output_path, "partition/", months[m],"/"))
query <- results_pq |> 
  group_by(lat, lon) |>
  summarize(y_mean_mean = mean(y_mean), y_sd_mean = mean(y_sd),
           across(starts_with("w_mean"), mean, .names = "{.col}_mean"),
           across(starts_with("pred_"), mean, .names = "{.col}_mean")) |>
  arrange(lat, lon)

dta <- query |> collect()
postproc_write_raster(dta = dta, vars = colnames(dta)[-which(colnames(dta) %in% c("lon", "lat"))], temp_aggregation = "monthly")
rm(dta)
    }
# create annual summary rasters (all outputs)
results_pq <- arrow::open_dataset(paste0(config$output_path, "partition/"))
query <- results_pq |> 
  group_by(lat, lon) |>
  summarize(y_mean_mean = mean(y_mean), y_sd_mean = mean(y_sd),
           across(starts_with("w_mean"), mean, .names = "{.col}_mean"),
           across(starts_with("pred_"), mean, .names = "{.col}_mean")) |>
  arrange(lat, lon)

dta <- query |> collect()
postproc_write_raster(dta = dta, vars = colnames(dta)[-which(colnames(dta) %in% c("lon", "lat"))], temp_aggregation = "annual")
rm(dta)



# 0.b small function to arrange by lat and lon # todo integrate once in all code
arrange_lat_lon <- function(d) {
  d %>% 
    arrange(ref_lat) %>% 
    arrange(ref_lon)
}


# extract values at monitoring locations

# 1.b. bring in aqs data
training <- read_fst(paste0(config$path_obs, config$obs_dta_name, "/", config$obs_dta_name, "_", config$temp_resolution ,"_curated_", config$date_begin[1], ".fst")) 
if(config$temp_resolution == "daily"){
training <- training %>% 
  mutate(ddate = parse_date_time(date_local, 'ymd')) %>% 
  mutate(year = as.character(year(ddate)), month = pad0(month(ddate)), day = pad0(day(ddate)))
    }

# read keys reference grid at aqs locations
if(is.null(config$ref_grid_resolution)){ # otherwise ref grid is uninformed by input base models grids
# read keys
      # process models 
keys_file_name <- paste0(config$path_keys, "key_nn_", "aqs", "_", mod_names_y[config$ref_grid_from_input_base_model], ".fst")
  keys_ref_grid <- read_fst(keys_file_name)  %>% 
    arrange_lat_lon()
}

# subset bne results to those over monitoring locations

# rename to lon lat
colnames(keys_ref_grid)[which(colnames(keys_ref_grid) %in% c('baseModel_lat','baseModel_lon'))] <- c("lat", "lon")

keys_ref_grid_arrow <- keys_ref_grid |> 
  arrow::arrow_table()

results_ref_obs <- results_pq|>
  dplyr::left_join(keys_ref_grid_arrow, by = c("lat", "lon")) |> 
  dplyr::filter(!is.na(ref_id)) |>
  dplyr::select(lat, lon, time, y_mean, y_sd, starts_with("w_mean"), starts_with("pred_"), ref_id) |>
  collect()

# transform time column to year, month, day
    maxDoY <- 365
    if (config$date_begin[1] %in% c(2004, 2008, 2012, 2016, 2020, 2024)) {maxDoY <- 366}
results_ref_obs_date <- results_ref_obs %>% 
    mutate(day =  time * maxDoY,
    date= as.Date(day-1, origin=paste0(config$date_begin[1], "-01-01")), 
           year= strftime(date, "%y"),
              month= strftime(date, "%m"), 
              day=strftime(date,"%d"))

results_obs <- training %>% # todo: check why missing data
dplyr::left_join(results_ref_obs_date[,-which(colnames(results_ref_obs_date)%in% c('lon','lat'))], by = c("date", "ref_id")) %>% 
                                 dplyr::select(ref_id, date, lat, lon, obs, y_mean, y_sd, starts_with("pred_"), starts_with("w_mean")) %>% 
dplyr::filter(complete.cases(.)) %>% 
dplyr::mutate_at(c('obs'), as.numeric)

  results_obs %>%
      readr::write_csv(paste0(config$output_path, paste0("aqs_evaluation_", config$temp_resolution, ".csv")))




