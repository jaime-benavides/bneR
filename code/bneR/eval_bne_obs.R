## ###########################################################
## create predictions dataset
## model and store in a R data object
## Last version: 09/28/2023
## Author: Sebastian Rowland, Lawrence Chilrud, Jaime Benavides
## ###########################################################
# todo: move packages to init
library(ggplot2)
library(plotly)
library(leaflet)
library(crosstalk)

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
mkdirs(paste0(config$output_path, "plots"))
plots_path <- paste0(config$output_path, "plots/")
if(dir.exists(paste0(config$output_path, "/partition/"))){
dir.exists(paste0(config$output_path, "/partition/"))
    months <- list.dirs(paste0(config$output_path, "/partition/"), full.names = FALSE) 
months <- months[which(nchar(months) >0)]
} else {
    print("you need to first create partitions and rasters")
    }

# read eval bne obs
dta <- readr::read_csv(paste0(config$output_path, "aqs_evaluation_daily.csv"))

varVec <- dta$y_sd
varRange <- max(varVec) - min(varVec)
varScaleStd <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))
dta$uncertainty_type <- cut(dta$y_sd,
                       breaks=varScaleStd,
                       labels=c('very low', 'low', 'high', 'very high'))
x <- "y_mean"
y <- "obs"
    data_sub <- dta
    x_label <-  expression("Modeled NO"[2]*" (ppm)")
    y_label_abs <- expression("Observed NO"[2]*" (ppm)")
    y_label_rel <- expression("Modeled relative difference NO"[2]*" (%)")
    y_label <- y_label_abs
    title = "test"    
group_by <- "uncertainty_type"
x_limits <- c(0,max(data_sub$y_mean))
y_limits <- c(0,max(data_sub$obs))
png(file = paste0(config$output_path,"plots/", "conc_bne_obs_by_", group_by,".png"), width = 600, height = 540)
p <- ggplot(data_sub, aes_string(x = x, y = y, colour = group_by)) +
      #ggtitle(title) +
      xlab(x_label) +
      theme_bw() +
      theme(text = element_text(size = 20, colour = "black"),
            legend.position = "bottom") +
      scale_y_continuous(name=y_label, expand = c(0, 0), limits = y_limits) +
      scale_x_continuous(expand = c(0, 0), limits = x_limits) +
      geom_point(size = 2.5) +
     #colour_scale +
      geom_smooth(method="lm",se=FALSE) +
      theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
print(p)
    dev.off()

png(file = paste0(config$output_path,"plots/", "conc_bne_obs",".png"), width = 600, height = 540)
p <- ggplot(data_sub, aes_string(x = x, y = y)) +
      #ggtitle(title) +
      xlab(x_label) +
      theme_bw() +
      theme(text = element_text(size = 20, colour = "black"),
            legend.position = "bottom") +
      scale_y_continuous(name=y_label, expand = c(0, 0), limits = y_limits) +
      scale_x_continuous(expand = c(0, 0), limits = x_limits) +
      geom_point(size = 2.5) +
     #colour_scale +
      geom_smooth(method="lm",se=FALSE) +
      theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
print(p)
    dev.off()

# generate interactive plots
# generate interactive plots
dta_conc_vars <- c("ref_id", "date", "lat", "lon", "obs", "y_mean", "pred_cams", "pred_equates", "pred_js", "pred_omi")
dta_weight_vars <- c("ref_id", "date", "lat", "lon", "w_mean_cams", "w_mean_equates", "w_mean_js", "w_mean_omi")
dta_uncert_vars <- c("ref_id", "date", "lat", "lon", "obs", "y_mean", "y_sd")


dta_conc <- tidyr::gather(data_sub[,dta_conc_vars], source, concentration, obs, y_mean, pred_cams, pred_equates, pred_js, pred_omi)
dta_weight <- tidyr::gather(data_sub[,dta_weight_vars], source, weight, w_mean_cams, w_mean_equates, w_mean_js, w_mean_omi)
dta_uncert <- data_sub[,dta_uncert_vars]


stations <- unique(dta_conc[,c("ref_id", "lon", "lat")])
dta_conc_ct <- SharedData$new(dta_conc, key = ~ref_id)
stations_ct <- SharedData$new(stations, key = ~ref_id)
dta_uncert_ct <- SharedData$new(dta_uncert, key = ~ref_id)
dta_weight_ct <- SharedData$new(dta_weight, key = ~ref_id)

widget <- bscols(widths = c(12),
  filter_select("ref_id", "AQS monitoring sites", dta_conc_ct, ~ref_id)
)
# generate plot concentrations
p <- ggplot(dta_conc_ct) + 
  geom_line(aes(date, concentration, color = source)) + 
scale_color_manual(values = c(obs = "black", y_mean = "red",
                             pred_cams = "green", pred_equates = "blue", 
                             pred_js = "orange", pred_omi = "purple")) +
  theme_bw()
p_ly <- plotly::ggplotly(p, dynamicTicks = TRUE) %>%
 plotly::rangeslider() %>%
  plotly::layout(hovermode = "x")

stations_map <- leaflet(stations_ct) %>% addCircles(., lat = ~ lat, lng = ~ lon) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(ref_id), label = ~as.character(ref_id))

# compose output ----
eda_conc_stations <- bscols(widths = c(4, 8), list(stations_map, widget),  p_ly)
eda_conc_stations
# todo save 
rm(p, p_ly)
# generate plot weights
widget <- bscols(widths = c(12),
  filter_select("ref_id", "AQS monitoring sites", dta_weight_ct, ~ref_id)
)

p <- ggplot(dta_weight_ct) + 
  geom_line(aes(date, weight, color = source)) + 
scale_color_manual(values = c(w_mean_cams = "green", w_mean_equates = "blue", 
                             w_mean_js = "orange", w_mean_omi = "purple")) +
  theme_bw()
p_ly <- plotly::ggplotly(p, dynamicTicks = TRUE) %>%
 plotly::rangeslider() %>%
  plotly::layout(hovermode = "x")
eda_weight_stations <- bscols(widths = c(4, 8), list(stations_map, widget),  p_ly)
eda_weight_stations
rm(p, p_ly)

# generate plot uncertainty
widget <- bscols(widths = c(12),
  filter_select("ref_id", "AQS monitoring sites", dta_uncert_ct, ~ref_id)
)
p <- dta_uncert_ct %>% 
  ggplot(aes(date, y_mean)) + 
geom_line(aes(date, obs)) +
  geom_ribbon(aes(ymin = y_mean - y_sd,
                  ymax = y_mean + y_sd),
              fill = "steelblue2") + 
  geom_line(color = "firebrick", size = 1) +
  theme_bw()
p_ly <- plotly::ggplotly(p, dynamicTicks = TRUE) %>%
 plotly::rangeslider() %>%
  plotly::layout(hovermode = "x")
eda_uncert_stations <- bscols(widths = c(4, 8), list(stations_map, widget),  p_ly)
eda_uncert_stations
rm(p, p_ly)
