### ---------------------------------------------

## Title: Import Data

## Author: Michaela L. Gustafson

## Description: Importing 2019 and 2021 prey survey data

## Date created: 08 September 2021

## ----------------------------------------------

### LIBRARY -------------------------------------

library(here)
library(dplyr)
library(tidyr)

### END LIBRARY ---------------------------------

### IMPORT DATA ---------------------------------

## Importing and initial cleaning of observation and environmental .csv for 2019

here()

obs_main_19 <- read.csv(here("data/Point_Count_Obs_2019_MG_cleaned.csv"))

env_main_19 <- read.csv(here("data/Point_Count_Env_2019_MG_cleaned.csv"))

obs_main_21 <- read.csv(here("data/Point_Count_Obs_2021_MG.csv"))

env_main_21 <- read.csv(here("data/Point_Count_Env_2021_MG.csv"))

# We can see in the Global Environment that we've ended up with thousands of extra rows
# This was some sort of formatting error in the Excel sheets that I couldn't fix
# We will do an intial clean of these datasets to slim down the tables

### MINOR CLEANING ------------------------------

## Clean up table size by dropping all rows in which the Road_ID column contains NA

# Environmental data 2019:

env_main_19_short <- env_main_19 %>% dplyr::filter(!(Road_ID==""))

# Observation data 2019 & 2021:

obs_main_19_short <- obs_main_19 %>% 
  dplyr::filter(!(Road_ID==""))

obs_main_21_short <- obs_main_21 %>%
  dplyr::filter(!(ROAD_ID==""))

### END MINOR CLEANING --------------------------

### WRITE NEW DATA CSV --------------------------

# I will write .csv files of these shorter data frames to import into my data cleaning module
# and so I will have Excel copies without all the extra blank rows and so I can manipulate my
# data without touching the raw data

write.csv(env_main_19_short, "data/env_main_19_short.csv")
write.csv(obs_main_19_short, "data/obs_main_19_short.csv")
write.csv(obs_main_21_short, "data/obs_main_21_short.csv")
write.csv(env_main_21, "data/env_main_21_short.csv") # even though there were no excess
# rows in the 2021 environmental data, rewrote a .csv to be consistent with imports for 
# the cleaning module

### END WRITE CSV -------------------------------


### END SCRIPT ----------------------------------

