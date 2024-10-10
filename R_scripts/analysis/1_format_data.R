###############################################################################
#project: BRAC gps data
#1. load data from movebank and format
#date: Oct 26, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(move)
library(keyring)

#source functions
source("R_scripts/functions/outputs_loc.R")

#set data output base path to the projects google drive output folder
outputbasepath <- outputs_loc("BRACgps_outputs")

#load data from movebank

#store movebank password
key_set(service = "Movebank",
        username = "ariellenske",
        prompt = "Movebank password: ")

#set movebank login
mb_user <- keyring::key_list(service = "Movebank")$username
mb_pass <- keyring::key_get("Movebank", username = mb_user)
login <- move::movebankLogin(username = mb_user, password = mb_pass)

#load data from BRAC gps movebank project 
#3066962629 - Brandt's Cormorant BRAC, Salish Sea BC Canada (GPS)

#get the animal names of the study
birdIDs <- as.character(unique(getMovebankAnimals(study = 3066962629, login = login)$local_identifier))

#gps and associated sensor data
#download each individual's data and create a MoveStack
locs <- lapply(birdIDs, function(x){
  
  print(paste0(x," (", match(x, birdIDs), " of ", length(birdIDs),")"))
  getMovebankData(study = 3066962629,
                  login = login, 
                  animalName = x, 
                  removeDuplicatedTimestamps = TRUE,
                  includeExtraSensors = FALSE,
                  includeOutliers = FALSE)

  })

locs <- moveStack(locs, forceTz = "UTC")

#convert to df
locs.df <- as(locs, 'data.frame') 


#sensor data with no location
#download each individual's data and create a MoveStack
sensors <- lapply(birdIDs, function(x){
  
  print(paste0(x," (", match(x, birdIDs), " of ", length(birdIDs),")"))
  getMovebankNonLocationData(study = 3066962629,
                             login = login, 
                             animalName = x)
  
})


# sensors <- getMovebankNonLocationData(study = 3066962629, login = login)

#add deployId to sensor records
deployIDs <- locs.df %>% 
  dplyr::select(study_site, deployment_local_identifier, tag_local_identifier,
                individual_local_identifier = ring_id) %>%
  distinct()

sensors <- left_join(sensors, deployIDs)

#combine gps and sensor data into one dataframe
db <- bind_rows(locs.df %>% rename(individual_local_identifier = ring_id),
                sensors)


#clean up dataset

#add species 4-letter code column
sciname.df <- data.frame(species = c("BRAC"),
                         taxon_canonical_name = c("Phalacrocorax penicillatus"))

db <- left_join(db, sciname.df)

#list of cols want to keep
cols <- c("study_site", "timestamp", "location_lat", "location_long",
          "acceleration_raw_x", "acceleration_raw_y","acceleration_raw_z",
          "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z",         
          "ground_speed", "heading","height_above_msl",
          "barometric_height","barometric_depth", 
          "external_temperature", "light_level",
          "tag_mass_total", "tag_readout_method",
          "tag_voltage", "battery_charge_percent","battery_charging_current",
          "gps_satellite_count", "gps_time_to_fix", "gps_hdop", 
          "deployment_local_identifier",
          "tag_local_identifier", "individual_local_identifier", "species", "taxon_canonical_name",
          "animal_mass", "animal_reproductive_condition", "sex",
          "attachment_type", 
          "deploy_on_person", "deploy_on_timestamp", 
          "deploy_on_latitude", "deploy_on_longitude",
          "duty_cycle", "comments.y")


                                
db <- db %>%
  dplyr::select(!!!rlang::syms(cols)) %>%
  dplyr::mutate(
    timestamp = with_tz(as.POSIXct(timestamp, tz = "UTC"), tzone = "America/Vancouver"), # convert times to PDT
    deploy_on_timestamp = with_tz(as.POSIXct(deploy_on_timestamp, tz = "UTC"), tzone = "America/Vancouver"),
    year = as.numeric(strftime(timestamp, '%Y')), #add year field
    month = as.numeric(strftime(timestamp, '%m')) # add numeric month field
  ) 


#rename columns for easier coding
db <- db %>%
  rename(tagID = tag_local_identifier,
         metalBand = individual_local_identifier,
         speciesSciName = taxon_canonical_name,
         deployTime = deploy_on_timestamp,
         deployMass = animal_mass,
         attachmentType = attachment_type,
         deployLat = deploy_on_latitude,
         deployLon = deploy_on_longitude,
         deployComments = comments.y,
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
         magneticField_x = magnetic_field_raw_x,
         magneticField_y = magnetic_field_raw_y,
         magneticField_z = magnetic_field_raw_z,
         groundSpeed = ground_speed,        
         barometricHeight = barometric_height,
         barometricDepth = barometric_depth,
         temperature = external_temperature,
         lightLevel = light_level)


#split into gps/sensor data and deployment info
gps <- db %>% 
  dplyr::select(studySite, species, deployID, tagID, metalBand, ts, lat, lon, 
                acceleration_x, acceleration_y, acceleration_z,
                magneticField_x, magneticField_y, magneticField_z, groundSpeed, heading,                 
                height_above_msl, barometricHeight, barometricDepth,
                temperature, lightLevel, tagVoltage, battery_charge_percent, battery_charging_current,
                num_satellites, gps_time_to_fix, gps_hdop,
                year, month) %>%
  arrange(studySite, year, species, deployID, ts)

deploys <- db %>% 
  dplyr::select(studySite, deployLat, deployLon, year, deployID, deployTime, 
                tagID, metalBand, species, speciesSciName, animal_reproductive_condition,
                deployMass, attachmentType, tag_mass_total, tag_readout_method, deploy_on_person,
                deployComments) %>%
  distinct() %>%
  dplyr::filter(!is.na(deployID)) %>%
  arrange(studySite, year, species, deployID)
 
  
#save dataframes to data_working folder on google drive
saveRDS(gps, file.path(outputbasepath, "data_working", "brac_gps_and_sensor_data.rds"))
saveRDS(deploys, file.path(outputbasepath, "data_working", "brac_gps_deployments.rds"))


