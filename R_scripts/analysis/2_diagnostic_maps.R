###############################################################################
#project: BRAC gps data
#2. map raw track data for each bird
#date: Nov 09, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)
library(lubridate)

#source functions
source("R_scripts/functions/set_ggplot_custom_theme.R")
source("R_scripts/functions/outputs_loc.R")
source("R_scripts/functions/localgd_loc.R")

#set data output base path to the projects google drive output folder
outputbasepath <- outputs_loc("BRACgps_outputs")

#set path to map_data folder
mapdatapath <- localgd_loc("map_data")

#load gps location data and deployment info
gps <- readRDS(file.path(outputbasepath, "data_working", "brac_gps_and_sensor_data.rds"))
deploys <- readRDS(file.path(outputbasepath, "data_working", "brac_deployments.rds"))

names(gps)
names(deploys)

#check num of unique birds, tags, and deployments
length(unique(deploys$metalBand)) #30
length(unique(deploys$tagID)) #30
length(unique(deploys$deployID)) #30

#list of deployment ids
dlist <- deploys %>% dplyr::select(deployID) %>% distinct() %>% pull()

#setup basic things for map 
# land <- st_read(file.path(mapdatapath, "land_polygon", "ne_10m_land.shp")) #not detailed enough
land <- st_read(file.path(mapdatapath, "world_coastline", "GSHHS_shp", "h", "GSHHS_h_L1.shp")) 
eez <- st_read(file.path(mapdatapath, "eez", "eez.shp"))

#filter out extreme outliers
gps <- gps %>% dplyr::filter(lon < -100 | is.na(lon))


#plot each bird separately
for(i in 1:length(dlist)){
  
  bird <- gps %>% filter(deployID == dlist[i]) 
  dep <- deploys %>% filter(deployID == dlist[i]) 
  
  depyear <- year(dep$deployTime)
  
  stime <- dep$deployTime - hours(4)
  etime <- max(bird$ts) + hours(4)
  
  lims <- c(stime, etime)
  
  xmin <- min(min(bird$lon, na.rm = TRUE), min(dep$deployLon, na.rm = TRUE)) - 0.1
  xmax <- max(max(bird$lon, na.rm = TRUE), max(dep$deployLon, na.rm = TRUE)) + 0.1
  ymin <- min(min(bird$lat, na.rm = TRUE), min(dep$deployLat, na.rm = TRUE)) - 0.1
  ymax <- max(max(bird$lat, na.rm = TRUE), max(dep$deployLat, na.rm = TRUE)) + 0.1
  
  bird_gps <- bird %>% dplyr::filter(!is.na(lat))
  bird_dive <- bird %>% dplyr::filter(!is.na(barometricDepth))
  
  #map
  p1 <- ggplot(data = eez) +
    geom_sf(fill = "grey90", color = "grey90") +
    geom_sf(data = land, color = "black", fill = "white") +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    geom_point(data = bird_gps,
               aes(lon, lat, color = ts), alpha = 0.8, size = 1) +
    geom_path(data = bird_gps,
              aes(lon, lat, color = ts), alpha = 0.8, linewidth = 0.75) +
    geom_point(data = dep,
               aes(deployLon, deployLat), color = "red", size = 2) +
    scale_color_gradientn(name = "timestamp",
                          colors = viridis(40, direction = -1, begin = 0, end = 1),
                          trans = "time") +
    theme(legend.position = "top") +
    guides(colour = guide_colourbar(barwidth=25)) +
    ylab("Latitude") + xlab("Longitude") +
    ggtitle(paste0(dep$species, " ", dep$studySite,": tag",
                   dep$tagID, " band", dep$metalBand,
                   " year:", depyear))
  
  #dives 
  p2 <- ggplot(data = bird_dive, aes(x = ts, y = as.numeric(barometricDepth))) +
    geom_point(color = "black", alpha = 0.5) +
    scale_x_datetime(date_breaks = "2 weeks",
                     date_minor_breaks = "1 week",
                     date_labels = "%d-%b",
                     limits = lims,
                     timezone = "America/Vancouver",
                     expand = c(0, 0)) +
    geom_point(data = dep, aes(x = deployTime, y = 0), color = "red", size = 2) +
    geom_hline(yintercept = 0, color = "red") +
    theme(legend.position = "none",
          panel.grid.minor.x = element_line(colour="lightgrey", linewidth = 0.5),
          panel.grid.major.x = element_line(colour="lightgrey", linewidth = 0.5)) +
    ylab("Depth (meters)") + xlab("Time") +
    scale_y_reverse()
  
  
  png(filename = file.path(outputbasepath, "figs", "diagnostic-individual-bird-plots",
                           paste0(dep$studySite,"-tag",
                                  dep$tagID, "-", depyear, ".png")),
      width=10, height=9, units="in", res=300,
      type = "cairo") 
  
  print(p1/p2 +
          plot_layout(heights = c(2, 1)))
  
  dev.off()
  
  
}
rm(i)



