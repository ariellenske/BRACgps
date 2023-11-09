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

#gps and associated sensor data
locs <- getMovebankData(study = 3066962629, login = login,
                        removeDuplicatedTimestamps = TRUE,
                        includeExtraSensors = TRUE,
                        deploymentAsIndividuals = TRUE,
                        includeOutliers = FALSE)
#convert to df
locs.df <- as(brac, 'data.frame') 


#sensor data with no location
sensors <- getMovebankNonLocationData(study = 3066962629, login = login)


#combine gps and sensor data into one dataframe
db <- bind_rows(locs.df, sensors)


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
          "tag_local_identifier", "ring_id", "species", "taxon_canonical_name",
          "animal_mass", "animal_reproductive_condition", "sex",
          "attachment_type", 
          "deploy_on_person", "deploy_on_timestamp", 
          "deploy_on_latitude", "deploy_on_longitude",
          "duty_cycle", "comments.y")


                                
test <- db %>%
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
         metalBand = ring_id,
         speciesSciName = taxon_canonical_name,
         deployTime = deploy_on_timestamp,
         retrievalTime = deploy_off_timestamp,
         deployMass = animal_mass,
         nestStage = animal_reproductive_condition,
         attachmentType = attachment_type,
         colonyLat = deploy_on_latitude,
         colonyLon = deploy_on_longitude,
         deployComments = comments.y,
         retrievalComments = deployment_end_comments,
         deployID = deployment_local_identifier,
         fixRate = duty_cycle,
         studySite = study_site,
         ts = timestamp,
         lat = location_lat,
         lon = location_long,
         tagVoltage = tag_voltage,
         num_satellites = gps_satellite_count)

#split into gps data and deployment info
gps <- db %>% 
  dplyr::select(studySite, species, deployID, ts, lat, lon, tatVoltage, num_satellites,
                nestStage, year, month, fixRate) %>%
  arrange(studySite, year, species, deployID, ts)

deploys <- db %>% 
  dplyr::select(studySite, colonyLat, colonyLon, year, deployID, deployTime, retrievalTime, fixRate,
                tagID, metalBand, species, speciesSciName,
                deployMass, nestStage,
                attachmentType, 
                deployComments, retrievalComments) %>%
  distinct() %>%
  arrange(studySite, year, species, deployID)





