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
library(moveVis)

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
deploys <- readRDS(file.path(outputbasepath, "data_working", "brac_gps_deployments.rds"))

names(gps)
names(deploys)

#2.0 setup basic things for mapping#### 

#load map data
bccoast = st_read(file.path(mapdatapath, "coastline_BC", "british_columbia_coastline.shp")) 
eez = st_read(file.path(mapdatapath, "eez", "eez.shp"))
bathy <- getNOAA.bathy(-124, -122, 50, 47, res = 0.25, keep = TRUE)
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


xmin <- min(min(gps$lon, na.rm = TRUE)) - 0.1
xmax <- max(max(gps$lon, na.rm = TRUE)) + 0.1
ymin <- min(min(gps$lat, na.rm = TRUE)) - 0.1
ymax <- max(max(gps$lat, na.rm = TRUE)) + 0.1

#split gps data into alive and presumed or confirmed dead birds
gps.a <- gps %>% 
  dplyr::filter(!deployID %in% c("band112802265-tag210419-2023",
                                 "band112802266-tag210418-2023",
                                 "band112802269-tag220557-2023",
                                 "band112802277-tag220552-2023",
                                 "band112802270-tag210421-2023",
                                 "band112802257-tag210551-2023",
                                 "band112802268-tag210552-2023"))

gps.d <- gps %>% 
  dplyr::filter(deployID %in% c("band112802265-tag210419-2023",
                                "band112802266-tag210418-2023",
                                "band112802269-tag220557-2023",
                                "band112802277-tag220552-2023",
                                "band112802270-tag210421-2023",
                                "band112802257-tag210551-2023",
                                "band112802268-tag210552-2023"))

#plot location data
png(filename = file.path(outputbasepath, "figs", "reports", "brac_gps_all.png"),
    width=7, height=7, units="in", res=300,
    type = "cairo") 

ggplot() +
  geom_spatraster(data = bathyr) +
  scale_fill_stepsn(breaks = mapbr[[3]],
                    colors = c(ocean.col(mapbr[[1]]), land.col(mapbr[[2]])),
                    values = scales::rescale(mapbr[[3]])) +
  geom_sf(data = eez, fill = "grey35", color = "grey35", size = 0.2, alpha = 0.1) +
  geom_point(data = gps.a, aes(lon, lat, color = ts), alpha = 0.6, size = 0.3) +
  geom_path(data = gps.a, aes(lon, lat, group = deployID, color = ts), alpha = 0.6, size = 0.3) +
  geom_path(data = gps.d, aes(lon, lat, group = deployID),
            color = "grey25", alpha = 0.6, size = 0.3, linetype = "dotted") +
  geom_point(data = site,
             aes(deployLon, deployLat), color = "red", size = 1) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_color_gradientn(name = "timestamp",
                        colors = viridis(40, direction = -1, begin = 0, end = 1),
                        trans = "time") +
  theme(legend.position = "top") +
  guides(colour = guide_colourbar(barwidth=25),
         fill = FALSE)

dev.off()

#4.0 plot dive data

dives <-gps.a %>% dplyr::filter(!is.na(barometricDepth)) %>%
  mutate(date = as.Date(ts))

#plot data from all birds
png(filename = file.path(outputbasepath, "figs", "reports", "brac_dives_all.png"),
    width=10, height=4, units="in", res=300,
    type = "cairo") 

ggplot() +
  geom_point(data = dives,
             aes(ts, barometricDepth, color = barometricDepth), alpha = 0.2, size = 0.3) +
  scale_color_viridis(option = "B") +
  theme(legend.position = "none") +
  scale_y_reverse()

dev.off()

#plot dive data for one bird over one day as example

#list of deployment ids
dlist <- deploys %>% dplyr::select(deployID) %>% distinct() %>% pull()

dives.sub <- dives %>% 
  dplyr::filter(deployID == dlist[1] & ts > as.POSIXct("2023-10-16 8:30") & ts < as.POSIXct("2023-10-16 9:00"))


png(filename = file.path(outputbasepath, "figs", "reports", "brac_dive_example.png"),
    width=10, height=4, units="in", res=300,
    type = "cairo") 

ggplot() +
  geom_point(data = dives.sub,
             aes(ts, barometricDepth, color = barometricDepth), alpha = 0.6, size = 0.3) +
  ylab("depth (m)") + xlab("time") +
  scale_color_viridis(option = "B", direction = -1) +
  scale_y_reverse() +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey")

dev.off()

#5.0 animate GPS tracks####

#5.1 make base map raster
plot(bathyr)

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
  add_timestamps(type = "label", x = -123.6, y = 48.55, size = 3) 

frames[[40]] # preview one of the frames

# animate frames (slow ~20min)
animate_frames(frames, 
               out_file = "animated_BRAC_Salish_Sea.mp4",
               overwrite = TRUE,
               fps = 25)
