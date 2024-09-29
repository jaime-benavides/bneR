obs_context <- function(obs_all = obs_all, sp_context = sp_context){
        obs.unique <- obs_all %>% 
  dplyr::select(lat, lon, ref_id) %>% 
  distinct() 

coordinates(obs.unique) = ~lon+lat
obs.unique.sf = sf::st_as_sf(obs.unique)
sf::st_crs(obs.unique.sf) <- "EPSG:4326"
obs.unique.sf$lon <- obs.unique$lon
obs.unique.sf$lat <- obs.unique$lat 
obs.unique_cntxt_pos <- sapply(sf::st_intersects(obs.unique.sf, sp_context),function(x){length(x)>0})

obs.unique_contxt <- obs.unique.sf[obs.unique_cntxt_pos, ]
obs.unique_contxt_df <- obs.unique_contxt
sf::st_geometry(obs.unique_contxt_df) <- NULL
    return(obs.unique_contxt_df[,c("lon", "lat", "ref_id")])
}