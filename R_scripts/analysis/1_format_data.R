###############################################################################
#project: BRAC gps data
#1. load data from movebank and format
#date: Oct 26, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(move2)
library(sf)

#source functions
source("R_scripts/functions/outputs_loc.R")

#set data output base path to the projects google drive output folder
outputbasepath <- outputs_loc("BRACgps_outputs")

#load data from movebank

#store movebank password
movebank_store_credentials(username = "ariellenske")

#load data from BRAC gps movebank project 
#3066962629 - Brandt's Cormorant BRAC, Salish Sea BC Canada (GPS)

#get the animal names of the study
birdIDs <- as.character(unique(movebank_download_deployment(study = 3066962629)$individual_local_identifier))

#get list of study sensor type ids
movebank_download_study_info(study_id = 3066962629)$sensor_type_ids

#get entity type list (gives you the correct formatting for sensor_type_id)
movebank_retrieve(entity_type = "tag_type")

#gps and sensor data
#download each individual's data and combine in one df
locs <- list()

for(i in 1:length(birdIDs)) {
  
  # i <- 1
  
  print(paste0(birdIDs[i]," (", i, " of ", length(birdIDs),")"))
  
  temp <- movebank_download_study(study_id = 3066962629,
                                  sensor_type_id = c("gps", "accessory-measurements"),
                                  individual_local_identifier = birdIDs[i], 
                                  remove_movebank_outliers = TRUE)

  
  #convert to flat table with all fields
  temp <- mt_as_event_attribute(temp, names(mt_track_data(temp)))
  
  #pull out spatial data
  temp <- temp %>% dplyr::mutate(deploy_on_longitude = sf::st_coordinates(temp$deploy_on_location)[,1],
                                 deploy_on_latitude = sf::st_coordinates(temp$deploy_on_location)[,2],
                                 location_long = sf::st_coordinates(temp)[,1],
                                 location_lat = sf::st_coordinates(temp)[,2])
  #drop geometry
  temp <- sf::st_drop_geometry(temp)
  
  #remove attributes from df
  temp <- data.frame(temp)
  
  locs[[i]] <- temp
  
}
rm(i, temp)

#convert list to df (spatial point sfc)
db <- bind_rows(locs)

#clean up dataset

#add species 4-letter code column
sciname.df <- data.frame(species = c("BRAC"),
                         taxon_canonical_name = c("Phalacrocorax penicillatus"))

db <- left_join(db, sciname.df)

#list of cols want to keep
cols <- c("study_site",
          "event_id",
          "timestamp",
          "location_lat", "location_long",
          "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z",
          "magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z",
          "ground_speed",
          "heading",
          "height_above_msl",
          "barometric_height",
          "barometric_depth",
          "external_temperature",
          "light_level",
          "tag_mass_total",
          "tag_readout_method",
          "tag_voltage", "battery_charge_percent","battery_charging_current",
          "gps_satellite_count",
          "gps_time_to_fix",
          "gps_hdop",
          "ornitela_transmission_protocol",
          "deployment_local_identifier",
          "tag_local_identifier",
          "manufacturer_name",
          "model",
          "individual_local_identifier",
          "species", "taxon_canonical_name",
          "animal_mass",
          "animal_life_stage",
          "animal_reproductive_condition",
          "sex",
          "attachment_type",
          "deploy_on_person",
          "deploy_on_timestamp",
          "deploy_on_latitude", "deploy_on_longitude",
          "duty_cycle",
          "deployment_comments",
          "deploy_off_timestamp",
          "deployment_end_type",
          "deployment_end_comments")

                               
db <- db %>%
  dplyr::select(!!!rlang::syms(cols)) %>%
  dplyr::mutate(
    timestamp = with_tz(as.POSIXct(timestamp, tz = "UTC"), tzone = "America/Vancouver"), # convert times to PT
    deploy_on_timestamp = with_tz(as.POSIXct(deploy_on_timestamp, tz = "UTC"), tzone = "America/Vancouver"),
    deploy_off_timestamp = with_tz(as.POSIXct(deploy_off_timestamp, tz = "UTC"), tzone = "America/Vancouver"),
    year = as.numeric(strftime(timestamp, '%Y')), #add year field
    month = as.numeric(strftime(timestamp, '%m')) # add numeric month field
  ) 


#rename columns for easier coding
db <- db %>%
  rename(tagID = tag_local_identifier,
         metalBand = individual_local_identifier,
         speciesSciName = taxon_canonical_name,
         deployTime = deploy_on_timestamp,
         deployEndTime = deploy_off_timestamp,
         deployMass = animal_mass,
         attachmentType = attachment_type,
         deployLat = deploy_on_latitude,
         deployLon = deploy_on_longitude,
         deployComments = deployment_comments,
         deployEndComments = deployment_end_comments,
         deployEndType = deployment_end_type,
         deployID = deployment_local_identifier,
         fixRate = duty_cycle,
         studySite = study_site,
         ts = timestamp,
         lat = location_lat,
         lon = location_long,
         tagVoltage = tag_voltage,
         num_satellites = gps_satellite_count,
         acceleration_x = acceleration_raw_x, 
         acceleration_y = acceleration_raw_y, 
         acceleration_z = acceleration_raw_z, 
         magnetic_x = magnetic_field_raw_x,
         magnetic_y = magnetic_field_raw_y,
         magnetic_z = magnetic_field_raw_z,
         groundSpeed = ground_speed,        
         barometricHeight = barometric_height,
         barometricDepth = barometric_depth,
         temperature = external_temperature,
         lightLevel = light_level)

#split into gps/sensor data and deployment info
gps <- db %>% 
  dplyr::select(studySite, species, deployID, tagID, metalBand, event_id, ts, lat, lon, 
                acceleration_x, acceleration_y, acceleration_z,
                magnetic_x, magnetic_y, magnetic_z, 
                groundSpeed, heading,                 
                height_above_msl, barometricHeight, barometricDepth,
                temperature, lightLevel, 
                tagVoltage, battery_charge_percent, battery_charging_current,
                num_satellites, gps_time_to_fix, gps_hdop, ornitela_transmission_protocol,
                year, month) %>%
  group_by(deployID) %>%
  arrange(ts) %>%
  ungroup()

deploys <- db %>% 
  dplyr::select(studySite, deployLat, deployLon, deployID, deployTime, deployEndTime, deployEndType,
                tagID, manufacturer_name, model, metalBand, species, speciesSciName, sex, animal_reproductive_condition,
                deployMass, attachmentType, tag_mass_total, tag_readout_method, deploy_on_person,
                deployComments, deployEndComments) %>%
  distinct() 
 
  
#save dataframes to data_working folder on google drive
saveRDS(gps, file.path(outputbasepath, "data_working", "brac_gps_and_sensor_data.rds"))
saveRDS(deploys, file.path(outputbasepath, "data_working", "brac_deployments.rds"))


