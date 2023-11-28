###############################################################################
#project: BRAC gps data
#0.5. format data for movebank upload
#date: Oct 16, 2023
#author: Ariel Lenske
###############################################################################

#packages
library(tidyverse)
library(lubridate)
library(readxl)

#source functions
source("R_scripts/functions/outputs_loc.R")

#0.1 set data output base path to the projects google drive output folder####
outputbasepath <- outputs_loc("BRACgps_outputs")

#1. gps tagging metadata

#1.01 copy the brac tracking database from INGEO-DEL and read the relevant sheets into R####
file.copy(from = "Z:/USERS/LenskeA/CWS_OceanProtectionPlan/Cormorants/BRAC_tracking/Data/cormorant_tracking_database.xlsx",
          to = "data_raw/cormorant_tracking_database_copy.xlsx", overwrite = TRUE)

#locations
locations <- read_excel("data_raw/cormorant_tracking_database_copy.xlsx", sheet = "locations",
                        na = "-") 

#tags
tags <- read_excel("data_raw/cormorant_tracking_database_copy.xlsx", sheet = "tags",
                        na = "-") 

#captures
captures <- read_excel("data_raw/cormorant_tracking_database_copy.xlsx", sheet = "captures",
                       col_types = c(rep("text", 8), 
                                     rep("numeric", 1), 
                                     rep("text", 3),
                                     rep("guess", 3),
                                     rep("numeric", 6),
                                     rep("text", 1)),
                       na = "-") %>%
  dplyr::filter(!is.na(tagID))


#fix dates
captures <- captures %>%
  mutate(captureDate = as.Date(as.numeric(captureDate), origin = "1899-12-30"))

#fix times
captures <- captures %>%
  mutate(captureTime = as.character(gsub(".* ","", captureTime)),
         captureTime = str_c(captureDate, captureTime, sep = " "),
         releaseTime = as.character(gsub(".* ","", releaseTime)),
         releaseTime = str_c(captureDate, releaseTime, sep = " "),
         handlingTime = difftime(releaseTime, captureTime, units = "mins"))


#check dfs
head(as.data.frame(captures))
str(captures)
head(as.data.frame(locations))
str(locations)

#1.02. join deployment site info with gps tag metadata and tag info####
gpsmeta <- left_join(captures, locations %>% 
                       dplyr::select(location, deploy_lat = latitude, deploy_lon = longitude)) %>%
  left_join(., tags)

#1.03. add scientific names####
sciname.df <- data.frame(species = c("BRAC"),
                         animal_taxon = c("Phalacrocorax penicillatus"))

gpsmeta <- left_join(gpsmeta, sciname.df)

#1.04. remove dash from metalBand for merging with gpsdata files####
gpsmeta <- gpsmeta %>%
  mutate(metalBand = as.numeric(str_replace(metalBand, "-","")))

#1.05. fix tag deploy and retrieval times####

#a. set correct timezone, times were recorded in local time so timezone = "America/Vancouver"
gpsmeta <- gpsmeta %>%
  mutate(deploy_on_timestamp = as.POSIXct(releaseTime, tz = "America/Vancouver"))

#c. convert deploy times to UTC
gpsmeta <- gpsmeta %>%
  mutate(deploy_on_timestamp = with_tz(deploy_on_timestamp, tzone = "UTC"))

#1.07 add deployment id####
gpsmeta <- gpsmeta %>% mutate(deployID = paste0("band", metalBand, "-tag", tagID, "-", captureYear))

#1.08 create new columns needed for movebank####
gpsmeta <- gpsmeta %>% 
  mutate(animal_life_stage = "adult",
         animal_reproductive_condition = "non-breeding",
         deployment_end_type = NA,
         duty_cycle = "varible",
         tag_manufacturer_name = "Ornitela",
         tag_readout_method = "phone-network") 

#1.09 pull out all relevant columns####
gpsmeta <- gpsmeta %>%
  dplyr::select(tag_id = tagID,
                animal_id = metalBand,
                animal_taxon,
                deploy_on_timestamp,
                animal_life_stage,
                animal_mass = mass,
                animal_reproductive_condition,
                animal_ring_id = metalBand,
                attachment_type = tagAttachment,
                deploy_on_latitude = deploy_lat,
                deploy_on_longitude = deploy_lon,
                deploy_on_person = tagger,
                deploy_comments = comments,
                deployment_end_type,
                deployment_id = deployID,
                duty_cycle,
                study_site = location,
                tag_manufacturer_name,
                tag_mass = tagMass,
                tag_model = tagModel,
                tag_readout_method,
                species) 


#1.10 check gpsmeta df####
head(as.data.frame(gpsmeta))
str(gpsmeta)

# #1.11 save dataframe movebank upload####

write.csv(gpsmeta, file.path(outputbasepath,"data_processed", "movebank_upload",
                             paste0("reference-data-BRAC-Tsawwassen.csv")),
          row.names = FALSE)

###############################################################################
#2. sensor data

#2.01 download cormorant sensor and gps data from ingeodel and read into R####

# #create a new folder in "data_raw" to hold all the raw gps data
# dir.create(path = file.path("data_raw", "cormie_sensor_data"))

#list cormie files on INGEO-DEL
files <- list.files(path = "Z:/USERS/LenskeA/CWS_OceanProtectionPlan/Cormorants/BRAC_tracking/Data/BRAC gps and sensor data/", 
                    full.names = TRUE)

#copy files over from INGEO-DEL 
file.copy(from = files, to = file.path("data_raw", "cormie_sensor_data"), overwrite = TRUE)

#list cormie files in data_raw folder
files <- list.files(path = file.path("data_raw", "cormie_sensor_data"), 
                    full.names = TRUE)

#read data into r
sdata <- lapply(files, read_csv) %>%
  bind_rows()

#2.02 format for movebank####

# dive data (with other sensor data)
ddata <- sdata %>%
  dplyr::filter(!is.na(depth_m) & !is.na(light)) %>%
  dplyr::select(timestamp = UTC_datetime, 
                tag_id = device_id,
                depth_m,
                light,
                acc_x,
                acc_y,
                acc_z,
                mag_x,
                mag_y,
                mag_z,
                int_temperature_C) 

# dive data (without other sensor data)
ddata0 <- sdata %>%
  dplyr::filter(!is.na(depth_m) & is.na(light)) %>%
  dplyr::select(timestamp = UTC_datetime, 
                tag_id = device_id,
                depth_m) 


# acceleration only data
adata <- sdata %>%
  dplyr::filter(!is.na(acc_x) & is.na(depth_m)) %>%
  dplyr::select(timestamp = UTC_datetime, 
                tag_id = device_id,
                acc_x,
                acc_y,
                acc_z) %>%
  dplyr::filter(!is.na(acc_x))


#2.03 save dive and acceleration dataframes for movebank upload####

write.csv(ddata, file.path(outputbasepath,"data_processed", "movebank_upload",
                             paste0("dive-and-sensor-data-BRAC-Tsawwassen.csv")),
          row.names = FALSE)

write.csv(ddata0, file.path(outputbasepath,"data_processed", "movebank_upload",
                           paste0("dive-only-data-BRAC-Tsawwassen.csv")),
          row.names = FALSE)

write.csv(adata, file.path(outputbasepath,"data_processed", "movebank_upload",
                           paste0("acceleration-only-data-BRAC-Tsawwassen.csv")),
          row.names = FALSE)

