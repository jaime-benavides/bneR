# load packages
library(ggplot2)
library(plotly)

dta <- fst::read_fst(paste0("/data0/shr/bne/data/nox/obs/daily/formatted/ev/", "calnex_daily_curated_2010.fst"))
p <- ggplot(dta) + 
  geom_line(aes(date, NO2_ppbv)) + 
  theme_bw()
p_ly <- plotly::ggplotly(p) %>%
 plotly::rangeslider() %>%
  plotly::layout(hovermode = "x")


season <- function(datee){
  
  datee <- as.POSIXlt(datee)
  season <- vector()
  normal <- rep(c("Winter","Spring","Summer","Autumn","Winter"), c(59,92,92,91,31))
  leap <- rep(c("Winter","Spring","Summer","Autumn","Winter"), c(60,92,92,91,31))

  
  if(leap_year(year(datee)) == FALSE){
    season <- normal[datee$yday+1]
  } else {
    season <- leap[datee$yday+1]
  }
  return(season)
}
dta$season <-  sapply(dta$date_begin, season)
dta$season <- factor(dta$season, levels = c("Winter", "Spring", "Summer", "Autumn"))


varVec <- dta$y_sd_mean
varRange <- max(varVec) - min(varVec)
varScaleStd <- c(round(min(varVec), 2), round(min(varVec) + 0.25*varRange, 2), 
              round(min(varVec) + 0.5*varRange, 2), round(min(varVec) + 0.75*varRange, 2),
              round(max(varVec), 2))
dta$uncertainty_type <- cut(dta$y_sd_mean,
                       breaks=varScaleStd,
                       labels=c('very low', 'low', 'high', 'very high'))
x <- "y_mean_mean"
y <- "obs"
    data_sub <- dta
    x_label <-  expression("Modeled NO"[2]*" (ppm)")
    y_label_abs <- expression("Observed NO"[2]*" (ppm)")
    y_label_rel <- expression("Modeled relative difference NO"[2]*" (%)")
    y_label <- y_label_abs
    title = "test"    
group_by <- "season"
x_limits <- c(0,max(data_sub$y_mean_mean))
y_limits <- c(0,max(data_sub$obs))

png(file = paste0(config$output_path,"plots/", "conc_bne_ev_by_", group_by,".png"), width = 600, height = 540)
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

colnames(dta)
dta <- dta %>% 
  mutate(se = (obs- y_mean_mean)^2)
rmse.global <- sqrt(mean(dta$se))
r2.global <- cor(dta$y_mean_mean, dta$obs)^2


dta <- dta %>% 
  mutate(se = (obs- y_mean_mean)^2,
        se_js = (obs- pred_js_mean)^2, 
        se_cams = (obs- pred_cams_mean)^2,
        se_omi = (obs- pred_omi_mean)^2,
        se_equates = (obs- pred_equates_mean)^2)

rmse.global.js <- sqrt(mean(dta$se_js))
r2.global.js <- cor(dta$pred_js_mean, dta$obs)^2
rmse.global.cams <- sqrt(mean(dta$se_cams))
r2.global.cams <- cor(dta$pred_cams_mean, dta$obs)^2
rmse.global.omi <- sqrt(mean(dta$se_omi))
r2.global.omi <- cor(dta$pred_omi_mean, dta$obs)^2
rmse.global.equates <- sqrt(mean(dta$se_equates))
r2.global.equates <- cor(dta$pred_equates_mean, dta$obs)^2
rmse.global
r2.global
rmse.global.js
r2.global.js
rmse.global.cams
r2.global.cams
rmse.global.omi
r2.global.omi
rmse.global.equates
r2.global.equates

png(file = paste0(config$output_path,"plots/", "conc_bne_ev_bne.png"), width = 600, height = 540)
TP <- ggplot(dta) + 
  geom_line(aes(x = obs, y = obs), color = 'lightblue') +
  geom_point(aes(x = y_mean_mean, y = obs, colour = season)) + 
  geom_errorbar(aes(xmin = y_mean_mean - y_sd_mean, xmax = y_mean_mean + y_sd_mean, y = obs, colour = season)) + 
  labs(x = 'Estimated Concentration (ppb)',
       y = 'Observed Concentration (ppb)')+
  xlim(c(0, 50)) +  ylim(c(0, 50)) +
theme_bw() + 
annotate(geom="text", x=45, y=50, label="BNE",
              color="black") +
annotate(geom="text", x=45, y=48, label=paste0("RMSE: ", round(rmse.global,2)),
              color="black") + 
annotate(geom="text", x=45, y=46, label=paste0("R2: ", round(r2.global,2)),
              color="black")
print(TP )
    dev.off()

png(file = paste0(config$output_path,"plots/", "conc_bne_ev_cams.png"), width = 600, height = 540)
TP <- ggplot(dta) + 
  geom_line(aes(x = obs, y = obs), color = 'lightblue') +
  geom_point(aes(x = pred_cams_mean, y = obs, colour = season)) + 
  labs(x = 'Estimated Concentration (ppb)',
       y = 'Observed Concentration (ppb)')+
  xlim(c(0, 50)) +  ylim(c(0, 50)) +
theme_bw() + 
annotate(geom="text", x=45, y=50, label="CAMS",
              color="black") +
annotate(geom="text", x=45, y=48, label=paste0("RMSE: ", round(rmse.global.cams,2)),
              color="black") + 
annotate(geom="text", x=45, y=46, label=paste0("R2: ", round(r2.global.cams,2)),
              color="black")
print(TP )
   dev.off()

png(file = paste0(config$output_path,"plots/", "conc_bne_ev_equates.png"), width = 600, height = 540)
TP <- ggplot(dta) + 
  geom_line(aes(x = obs, y = obs), color = 'lightblue') +
  geom_point(aes(x = pred_equates_mean, y = obs, colour = season)) + 
  labs(x = 'Estimated Concentration (ppb)',
       y = 'Observed Concentration (ppb)')+
  xlim(c(0, 50)) +  ylim(c(0, 50)) +
theme_bw() + 
annotate(geom="text", x=45, y=50, label="EQUATES",
              color="black") +
annotate(geom="text", x=45, y=48, label=paste0("RMSE: ", round(rmse.global.equates,2)),
              color="black") + 
annotate(geom="text", x=45, y=46, label=paste0("R2: ", round(r2.global.equates,2)),
              color="black")
print(TP )
   dev.off()

png(file = paste0(config$output_path,"plots/", "conc_bne_ev_js.png"), width = 600, height = 540)
TP <- ggplot(dta) + 
  geom_line(aes(x = obs, y = obs), color = 'lightblue') +
  geom_point(aes(x = pred_equates_mean, y = obs, colour = season)) + 
  labs(x = 'Estimated Concentration (ppb)',
       y = 'Observed Concentration (ppb)')+
  xlim(c(0, 50)) +  ylim(c(0, 50)) +
theme_bw() + 
annotate(geom="text", x=45, y=50, label="JS",
              color="black") +
annotate(geom="text", x=45, y=48, label=paste0("RMSE: ", round(rmse.global.js,2)),
              color="black") + 
annotate(geom="text", x=45, y=46, label=paste0("R2: ", round(r2.global.js,2)),
              color="black")
print(TP )
   dev.off()

png(file = paste0(config$output_path,"plots/", "conc_bne_ev_omi.png"), width = 600, height = 540)
TP <- ggplot(dta) + 
  geom_line(aes(x = obs, y = obs), color = 'lightblue') +
  geom_point(aes(x = pred_equates_mean, y = obs, colour = season)) + 
  labs(x = 'Estimated Concentration (ppb)',
       y = 'Observed Concentration (ppb)')+
  xlim(c(0, 50)) +  ylim(c(0, 50)) +
theme_bw() + 
annotate(geom="text", x=45, y=50, label="OMI",
              color="black") +
annotate(geom="text", x=45, y=48, label=paste0("RMSE: ", round(rmse.global.omi,2)),
              color="black") + 
annotate(geom="text", x=45, y=46, label=paste0("R2: ", round(r2.global.omi,2)),
              color="black")
print(TP )
   dev.off()



