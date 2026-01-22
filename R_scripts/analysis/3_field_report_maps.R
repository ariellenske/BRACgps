###############################################################################
#project: BRAC gps data
#5. field report maps
#also contains code for some other maps e.g. for presentations
#date: Nov 9, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(sf)
library(viridis)
library(terra)
library(tidyterra)
library(marmap)
library(patchwork)
# library(moveVis)

#source functions
source("R_scripts/functions/outputs_loc.R")
source("R_scripts/functions/localgd_loc.R")
source("R_scripts/functions/set_ggplot_custom_theme.R")

#set data output base path to the projects google drive output folder
outputbasepath <- outputs_loc("BRACgps_outputs")

#set path to map_data folder
mapdatapath <- localgd_loc("map_data")

#Section 1: field report maps####

#1.0 load gps location data and deployment info####
gps <- readRDS(file.path(outputbasepath, "data_working", "brac_gps_and_sensor_data.rds"))
deploys <- readRDS(file.path(outputbasepath, "data_working", "brac_deployments.rds"))

names(gps)
names(deploys)

#2.0 setup basic things for mapping#### 

#load map data
bccoast = st_read(file.path(mapdatapath, "coastline_BC", "british_columbia_coastline.shp")) 
eez = st_read(file.path(mapdatapath, "eez", "eez.shp"))
bathy <- getNOAA.bathy(-125, -122, 50, 45, res = 0.25, keep = TRUE)
coast <- sf::read_sf(file.path(mapdatapath, "world_coastline", "GSHHS_shp", "f", "GSHHS_f_L1.shp"))


#2.0 set up stuff for plotting bathymetry layer####
#Switch to raster
bathyr <- marmap::as.raster(bathy)
bathyr <- rast(bathyr)
plot(bathyr)

#create color scheme for raster bathymetry map background
# Function to calculate colour break points
# x = raster, b1 & b2 = number of divisions for each sequence, r1 & r2 = rounding value
colbr <- function(x, b1=50, b2=50, r1=-2, r2=-2) {
  # Min/max values of the raster (x)
  mi <- as.numeric(global(x, fun = "min")) - 100
  ma <- as.numeric(global(x, fun = "max")) + 100
  # Create sequences, but only use unique numbers
  s1 <- unique(round(seq(mi, 0, 0-mi/b1),r1))
  s2 <- unique(round(seq(0, ma, ma/b2),r2))
  # Combine sequence for our break points, removing duplicate 0
  s3 <- c(s1, s2[-1])
  # Create a list with the outputs
  # [[1]] = length of the first sequence minus 1 (water)
  # [[2]] = length of the second sequence minus 1 (land)
  # [[3]] = The break points
  x <- list(length(s1)-1, length(s2)-1, s3)
}

mapbr <- colbr(x = bathyr, b1 = 12, b2 = 12)

#colors
ocean.col <- colorRampPalette(c("dodgerblue", "lightblue1"))
land.col <- colorRampPalette(c("white"))
land.col2 <- colorRampPalette(c("grey", "black"))
land.col3 <- colorRampPalette(c("lightblue1"))

#plot to check
ggplot() +
  geom_spatraster(data = bathyr) +
  scale_fill_stepsn(breaks = mapbr[[3]], 
                    colors = c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])), 
                    values = scales::rescale(mapbr[[3]])) 


#3.0 plot all birds together####
site <- data.frame(studySite = deploys$studySite,
                   deployLon = deploys$deployLon,
                   deployLat = deploys$deployLat) %>%
  distinct()

#plot location data
#pull out gps data (remove records with only sensor data)
gpso <- gps %>% dplyr::filter(!is.na(lat)) %>% dplyr::select(deployID, ts, tagID, lat, lon)

#################################
#segment tracks so that don't get lines between fixes that are far apart in time (i.e. accross large gaps btw fixes)
gpso <- gpso %>% 
  group_by(deployID) %>%
  arrange(ts) %>%
  mutate(lagts = lag(ts),
         timediff = signif(difftime(ts, lagts, units = "hours"), digits = 2),
         gap = ifelse(timediff <= 3, 0, 1) #gap defined as > 3 hours
  ) %>%
  ungroup()


#trackseg ID function
add_tracksegID <- function(bird){
  
  bird <- bird[with(bird, order(ts)), ]
  
  gap <- bird$gap
  tracksegID <- rep(0, nrow(bird))
  tracksegID[1] <- 1
  
  for(i in 2:nrow(bird)){
    
    if(gap[i] == 1){
      
      tracksegID[i] <- tracksegID[i-1] + 1
      
    } else if(gap[i] == 0){
      
      tracksegID[i] <- tracksegID[i-1]
    }}
  
  bird <- bird %>% mutate(tracksegID = tracksegID)
  
}

#add segmentIDs using above function
gpso <- gpso %>%
  group_by(deployID) %>%
  do(add_tracksegID(.)) %>%
  ungroup() 

#remove extra columns
gpso <- gpso %>% dplyr::select(-lagts, -timediff, -gap)

#make tripsegID unique
gpso <- gpso %>% mutate(tracksegID = paste0(deployID, "-", tracksegID))

#get summary of number of fixes per track segment
tab <- gpso %>%
  group_by(tracksegID) %>%
  summarise(n = n())
#################################

#subset gps data to with and without bird that went to Oregon
gpso1 <- gpso %>% dplyr::filter(tagID == "210424")
gpso0 <- gpso %>% dplyr::filter(tagID != "210424")

#plot all birds except for "210424"
#set plot boundaries
xmin <- min(min(gpso0$lon, na.rm = TRUE)) - 0.1
xmax <- max(max(gpso0$lon, na.rm = TRUE)) + 0.1
ymin <- min(min(gpso0$lat, na.rm = TRUE)) - 0.1
ymax <- max(max(gpso0$lat, na.rm = TRUE)) + 0.1

p1 <- ggplot() +
  geom_spatraster(data = bathyr) +
  scale_fill_stepsn(breaks = mapbr[[3]],
                    colors = c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])),
                    values = scales::rescale(mapbr[[3]])) +
  geom_sf(data = eez, fill = "grey35", color = "grey35", size = 0.2, alpha = 0.1) +
  geom_point(data = gpso0, aes(lon, lat, color = ts), alpha = 0.6, size = 0.3) +
  geom_path(data = gpso0, aes(lon, lat, group = tracksegID, color = ts), alpha = 0.6, linewidth = 0.3) +
  geom_point(data = site,
             aes(deployLon, deployLat), color = "red", size = 1) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_color_gradientn(name = "timestamp",
                        colors = viridis(40, direction = -1, begin = 0, end = 1),
                        trans = "time") +
  theme(legend.position = "top") +
  guides(colour = guide_colourbar(barwidth = 25),
         fill = FALSE)

png(filename = file.path(outputbasepath, "figs", "reports", "brac_gps_all_local.png"),
    width=7, height=7, units="in", res=300,
    type = "cairo") 

print(p1)

dev.off()

#plot "210424"
#set plot boundaries
xmin <- min(min(gpso1$lon, na.rm = TRUE)) - 0.1
xmax <- max(max(gpso1$lon, na.rm = TRUE)) + 0.1
ymin <- min(min(gpso1$lat, na.rm = TRUE)) - 0.1
ymax <- max(max(gpso1$lat, na.rm = TRUE)) + 0.1

p2 <- ggplot() +
  geom_spatraster(data = bathyr) +
  scale_fill_stepsn(breaks = mapbr[[3]],
                    colors = c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])),
                    values = scales::rescale(mapbr[[3]])) +
  geom_sf(data = eez, fill = "grey35", color = "grey35", size = 0.2, alpha = 0.1) +
  geom_point(data = gpso1, aes(lon, lat), alpha = 0.6, size = 0.3, color = "black") +
  geom_path(data = gpso1, aes(lon, lat, group = tracksegID), alpha = 0.6, linewidth = 0.3, color = "black") +
  geom_point(data = site,
             aes(deployLon, deployLat), color = "red", size = 1) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_color_gradientn(name = "timestamp",
                        colors = viridis(40, direction = -1, begin = 0, end = 1),
                        trans = "time") +
  theme(legend.position = "none") 

png(filename = file.path(outputbasepath, "figs", "reports", "brac_gps_210424.png"),
    width=7, height=14, units="in", res=300,
    type = "cairo") 

print(p1)

dev.off()


#4.0 plot dive data
dives <- gps %>% dplyr::filter(!is.na(barometricDepth)) %>%
  mutate(date = as.Date(ts),
         barometricDepth2 = as.numeric(barometricDepth)) 

#plot data from all birds
png(filename = file.path(outputbasepath, "figs", "reports", "brac_dives_all_v2.png"),
    width=10, height=4, units="in", res=300,
    type = "cairo") 

ggplot() +
  geom_point(data = dives,
             aes(ts, barometricDepth2, color = barometricDepth2), alpha = 0.2, size = 0.3) +
  scale_color_viridis(option = "B") +
  theme(legend.position = "none") +
  scale_y_reverse()

dev.off()

#plot dive data for one bird over one day as example

#list of deployment ids
dlist <- deploys %>% dplyr::select(deployID) %>% distinct() %>% pull()

dives.sub <- dives %>% 
  dplyr::filter(deployID == dlist[1] & ts > as.POSIXct("2023-10-16 8:30") & ts < as.POSIXct("2023-10-16 9:00"))

p3 <- ggplot() +
  geom_point(data = dives.sub,
             aes(ts, barometricDepth2, color = barometricDepth2), alpha = 0.6, size = 0.3) +
  ylab("depth (m)") + xlab("time") +
  scale_color_viridis(option = "B", direction = -1) +
  scale_y_reverse() +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey")

png(filename = file.path(outputbasepath, "figs", "reports", "brac_dive_example.png"),
    width=10, height=4, units="in", res=300,
    type = "cairo") 

print(p3)

dev.off()

p4 <- ggplot() +
  geom_point(data = dives.sub,
             aes(ts, barometricDepth2), alpha = 0.6, size = 0.3, color = "grey30") +
  ylab("depth (m)") + xlab("time") +
  scale_color_viridis(option = "B", direction = -1) +
  scale_y_reverse() +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey")

png(filename = file.path(outputbasepath, "figs", "reports", "brac_dive_example_grey.png"),
    width=10, height=4, units="in", res=300,
    type = "cairo") 

print(p4)

dev.off()


#multi plot figure
png(filename = file.path(outputbasepath, "figs", "reports", "brac_overall_plot.png"),
    width=13, height=10, units="in", res=300,
    type = "cairo") 

print((p1 / p4 + plot_layout(heights = c(2, 1))) | p2) +
  plot_annotation(tag_levels = "A")

dev.off()



#5.0 animate GPS tracks####

#5.1 make base map raster
#bathymetry layer

#disaggregate bathyr
bathyrd <- bathyr
bathyrd <- disagg(bathyrd, fact = 2)

# convert bathyr spatraster to raster RGB 
bathyrRGB <- raster::RGB(raster::raster(bathyrd), col = c(ocean.col(mapbr[[1]]), land.col3(mapbr[[2]])), breaks = mapbr[[3]])
plotRGB(bathyrRGB)

#coastal layer
#crop coast polygons
sf_use_s2(FALSE)
coastcrop <- st_crop(coast, y = c(xmin = -125, ymin = 48, xmax = -121, ymax = 50))
plot(coastcrop["id"])

#convert proj so matches bathymetry raster
coastcrop <- st_transform(coastcrop, crs = crs(bathyrRGB))

#rasterize the coastal polygon
coastcrop <- rasterize(vect(coastcrop), bathyrd)
plot(coastcrop)

#convert to a RGB raster
coastcropRGB <- RGB(raster::raster(coastcrop), col = "black")
plotRGB(coastcropRGB)

#combine bathymetry and coastal rasters into one raster
rbasemap <- raster::mosaic(coastcropRGB, bathyrRGB, fun = min)
plotRGB(rbasemap)

#5.2 set time zone to local time
Sys.setenv(TZ = "America/Toronto")

#5.3 format gps data for moveVis
gpsa <- gps %>%
  dplyr::select(studySite, species, tagID,
                ts, lat, lon) %>%
  mutate(id = tagID,
         legend = tagID)

#5.4 set track colour pallet
idlist <- gpsa %>% dplyr::select(legend) %>% distinct() %>% pull()

colpal <- data.frame(colour = viridis_pal()(length(idlist)),
                     legend = idlist)

gpsa <- left_join(gpsa, colpal)
tcols <- gpsa %>% dplyr::select(id, legend) %>%
  distinct() %>% left_join(., colpal) %>% dplyr::select(colour) %>%
  pull()

#5.5 convert to move format
gpsamove <- df2move(gpsa %>% dplyr::filter(!is.na(lat)),
                    proj = crs(rbasemap), 
                    x = "lon", y = "lat", time = "ts", track_id = "id") %>%
  align_move(res = 1, unit = "hours") 

#5.6 make lists for frames_spatial function
#time list
timelist <- list()
timelist[[1]] <- gpsamove@timestamps[1]

#raster list
rastlist <- list()
rastlist[[1]] <- rbasemap

#5.7 create spatial frames 
frames <- frames_spatial(gpsamove,
                         path_colours = tcols,
                         r_list = rastlist[[1]],
                         r_type = "RGB",
                         r_times = timelist[[1]],
                         crop_raster = TRUE,
                         path_size = 2,
                         fade_raster = FALSE,
                         path_legend = FALSE,
                         tail_length = 5,
                         tail_size = 0.8,
                         trace_show = TRUE,
                         trace_colour = "white"
                         ) %>%
  add_labels(x = "Longitude", y = "Latitude", 
             title = "BRAC GPS tracks, Salish Sea") %>%
  add_timestamps(type = "label", x = -123.6, y = 48.6, size = 3) 

frames[[40]] # preview one of the frames

# animate frames (slow ~20min)
animate_frames(frames, 
               out_file = "animated_BRAC_Salish_Sea_2.gif",
               overwrite = TRUE,
               fps = 100)
