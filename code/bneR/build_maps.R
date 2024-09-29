## ###########################################################
## build concentration maps
## Last version: 09/28/2023
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
    conc_title <- 'NO2 concentration (ppb)'
    uncert_title <- 'Uncertainty (ppb)'
    } else if (config$pollutant == "pm25"){
    conc_title <- 'PM2.5 concentration (micrograms / m3)'
    uncert_title <- 'Uncertainty (micrograms / m3)'
    }
mod_file_names <- paste0(mod_names, "_", poll, "_", config$date_begin[1], "_", config$temp_resolution, ".csv")
mod_names_y <- gsub(".csv","",mod_file_names)
mkdirs(paste0(config$output_path, "plots"))
plots_path <- paste0(config$output_path, "plots/")
if(dir.exists(paste0(config$output_path, "/partition/"))){
dir.exists(paste0(config$output_path, "/partition/"))
    months <- list.dirs(paste0(config$output_path, "/partition/"), full.names = FALSE) 
months <- months[which(nchar(months) >0)]
} else {
    print("you need to first create partitions and rasters")
    }

# read supporting data for plotting

# read road infrastructure for plotting
faf5_network <- sf::read_sf(paste0("/data0/shr/bne/data/static/geom/roads/", "FAF5Network.gdb"))
faf5_highways <- faf5_network[which(faf5_network$F_Class %in% c(1,2,3)),]
faf5_highways <- faf5_highways %>%
  sf::st_transform(sf::st_crs(sp_context))

# intersect with spatial context
roads_contxt_id_fhwa <- sapply(sf::st_intersects(faf5_highways, sp_context),function(x){length(x)>0})
roads_contxt_fhwa <- faf5_highways[roads_contxt_id_fhwa, ]
rm(faf5_highways)

# build bne concentration and uncertainty maps
raster_list <- list.files(path=paste0(config$output_path, "raster/"), 
     pattern =".tif$", full.names=TRUE)

raster_list_names <- list.files(path=paste0(config$output_path, "raster/"), 
     pattern =".tif$", full.names=FALSE)

raster_in <- terra::rast(raster_list)

# range concentration maps
# build generic function
postproc_plot_maps <- function(raster_in = raster_in, var_type = "bne_conc", temp_agg = "annual", m = NULL){ # m = NULL for annual and 1 to length(months) for monthly 

# prepare parameters plot
if(var_type == "bne_conc"){
if(temp_agg == "annual"){
var <- paste0(var_conc, "_annual")
title_plot <- paste0("Concentration annual mean for year ", config$date_begin[1])
    } else if (temp_agg == "monthly"){
var <- paste0(var_conc,"_", months[m])    
title_plot <- paste0("Concentration month ", months[m], " year ", config$date_begin[1])
    }
pal <- rev(RColorBrewer::brewer.pal(10, "RdYlBu"))
title_legend <- conc_title
breaks_loc <- varScaleConc
} else if (var_type == "bne_uncert"){
if(temp_agg == "annual"){
var <- paste0(var_uncert, "_annual")
title_plot <- paste0("Uncertainty annual mean for year ", config$date_begin[1])
} else if (temp_agg == "monthly"){
var <- paste0(var_uncert,"_", months[m]) 
title_plot <- paste0("Uncertainty month ", months[m], " year ", config$date_begin[1])
}
pal <- RColorBrewer::brewer.pal(9, "OrRd")
title_legend <- uncert_title
breaks_loc <- varScaleStd
} else if (var_type == "in_mod_conc"){
var_in_mod_conc <- paste0("pred_", mod_names, "_mean_", config$date_begin[1])
if(temp_agg == "annual"){
vars <- paste0(var_in_mod_conc, "_annual")
title_plot <- paste0("Concentration annual mean for year ", config$date_begin[1])
} else if (temp_agg == "monthly"){
vars <- paste0(var_in_mod_conc,"_", months[m])  
title_plot <- paste0("Concentration month ", months[m], " year ", config$date_begin[1])
}
pal <- rev(RColorBrewer::brewer.pal(10, "RdYlBu"))
title_legend <- conc_title
breaks_loc <- varScaleConc
} else if (var_type == "in_mod_weight"){
var_in_mod_weight <- paste0("w_mean_", mod_names, "_mean_", config$date_begin[1])
if(temp_agg == "annual"){
vars <- paste0(var_in_mod_weight, "_annual")
title_plot <- paste0("Weights annual mean for year ", config$date_begin[1])
} else if (temp_agg == "monthly"){
vars <- paste0(var_in_mod_weight,"_", months[m]) 
title_plot <- paste0("Weights month ", months[m], " year ", config$date_begin[1])
}
pal <- RColorBrewer::brewer.pal(9, "YlOrRd")
title_legend <- weight_title
breaks_loc <- varScaleWei
}
# create and save plot
if(var_type %in% c("bne_conc","bne_uncert")){
tm <- tmap::tm_shape(sf::st_make_valid(sp_context)) +
  tmap::tm_borders(alpha = 0.1, col = "black") +
tmap::tm_shape(raster_in[var], raster.downsample = FALSE) +
tmap::tm_raster(paste0(var,".tif"), palette = pal, style = "cont", title=title_legend, breaks = breaks_loc) +
tmap::tm_shape(roads_contxt_fhwa) +
  tmap::tm_lines(alpha = 0.05) +
  tmap::tm_layout(legend.position = c("left", "top"), 
            title= title_plot, 
            title.position = c('left', 'top'))

tmap::tmap_save(tm, paste0(plots_path, "map_", var, ".png"))
} else {
    for(v in 1:length(vars)){
        var <- vars[v]
        md_name <- mod_names[stringr::str_detect(var, mod_names)]
tm <- tmap::tm_shape(sf::st_make_valid(sp_context)) +
  tmap::tm_borders(alpha = 0.1, col = "black") +
tmap::tm_shape(raster_in[var], raster.downsample = FALSE) +
tmap::tm_raster(paste0(var,".tif"), palette = pal, style = "cont", title=title_legend, breaks = breaks_loc) +
tmap::tm_shape(roads_contxt_fhwa) +
  tmap::tm_lines(alpha = 0.05) +
  tmap::tm_layout(legend.position = c("left", "top"), 
            title= paste0(title_plot, " - ", md_name), 
            title.position = c('left', 'top'))

tmap::tmap_save(tm, paste0(plots_path, "map_", var, ".png"))
        }
    }
    }

# get breaks for maps 
var_conc <- paste0("y_mean_mean_", config$date_begin[1])
varVec <- values(raster_in[var_conc])[which(!is.na(values(raster_in[var_conc])))]
varRange <- max(varVec) - min(varVec)
varScaleConc <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))
rm(varVec, varRange)
# range standard deviation maps
var_uncert <- paste0("y_sd_mean_", config$date_begin[1])
varVec <- values(raster_in[var_uncert])[which(!is.na(values(raster_in[var_uncert])))]
varRange <- max(varVec) - min(varVec)
varScaleStd <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))
rm(varVec, varRange)
# range input base model weights maps
weight_title <- "BNE Weights [0-1]"# for plot
var_in_mod_weight <- paste0("w_mean_", mod_names, "_mean_", config$date_begin[1])

varVec <- numeric()
for(i in 1:length(mod_names)){
varVec_loc <- values(raster_in[var_in_mod_weight[i]])[which(!is.na(values(raster_in[var_in_mod_weight[i]])))]
varVec <- append(varVec, varVec_loc)
    }
varRange <- max(varVec) - min(varVec)
varScaleWei <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))
rm(varVec, varRange)

# plot annual mean
postproc_plot_maps(raster_in = raster_in, var_type = "bne_conc", temp_agg = "annual", m = NULL)
postproc_plot_maps(raster_in = raster_in, var_type = "bne_uncert", temp_agg = "annual", m = NULL)
postproc_plot_maps(raster_in = raster_in, var_type = "in_mod_conc", temp_agg = "annual", m = NULL)
postproc_plot_maps(raster_in = raster_in, var_type = "in_mod_weight", temp_agg = "annual", m = NULL)


for(m in 1:length(months)){
postproc_plot_maps(raster_in = raster_in, var_type = "bne_conc", temp_agg = "monthly", m = m)
postproc_plot_maps(raster_in = raster_in, var_type = "bne_uncert", temp_agg = "monthly", m = m)
postproc_plot_maps(raster_in = raster_in, var_type = "in_mod_conc", temp_agg = "monthly", m = m)
postproc_plot_maps(raster_in = raster_in, var_type = "in_mod_weight", temp_agg = "monthly", m = m)
}

# create gifs
## list file names and read in
img_file_names <- c("map_y_sd_mean_2010","map_y_mean_mean_2010","map_w_mean_omi_mean_2010", "map_w_mean_js_mean_2010",
                   "map_w_mean_cams_mean_2010", "map_w_mean_equates_mean_2010", 
                   "map_pred_omi_mean_2010", "map_pred_js_mean_2010",
                   "map_pred_cams_mean_2010", "map_pred_equates_mean_2010")
for(i in 1:length(img_file_names)){
files <- list.files(paste0(config$output_path, "plots/"), full.names = TRUE)
imgs <- files[which(grepl(img_file_names[i], files))]
img_list <- lapply(imgs, magick::image_read)
img_joined <- magick::image_join(img_list)
img_animated <- magick::image_animate(img_joined, fps = 1)

## save to disk
magick::image_write(image = img_animated,
            path = paste0(config$output_path, "plots/",img_file_names[i], ".gif"))
    }



   