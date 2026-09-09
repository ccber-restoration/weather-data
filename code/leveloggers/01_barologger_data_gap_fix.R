# =============================================================================
# Name:           venoco_levelogger_combine.R
# Description:    Uses SBA air pressure data to manually compensate levelogger data


# Author(s):      Claire WS

# Inputs:         SBA altimeter data (units = inches of mercury)             
# Outputs:        Individual compensated levelogger csvs
# 
# Notes:          Revised on 3/3/26 to manually compensate Campus Lagoon Data.
#                 
# =============================================================================

library(dplyr)
library(janitor)
library(tidyverse)
library(lubridate)
library(hms)
library(measurements)

# 1. Santa Barbara Airport pressure data ----

## Read in data from Santa Barbara Airport weather station. We are missing
## barologger data from 5/10 through 5/20 and from 8/29 through the rest of the WY.
## source: https://mesonet.agron.iastate.edu/request/download.phtml?network=CA_ASOS

sba<-read_csv("data/SBA/SantaBarbaraAirport_altimeter_2025.05.10_2025.11.26.csv") %>% 
  mutate(
    valid = mdy_hm(valid),
    Date = date(valid),
    Time = as_hms(valid)
  )
sba

# read in data from 9/24/25 to 1/27/26 to compensate the campus lagoon data.
sba2<-read_csv("data/SBA/SBA_altimeter_09.24.25_01.27.26.csv") %>% 
  mutate(
    valid = mdy_hm(valid),
    Date = date(valid),
    Time = as_hms(valid)
  )
sba2

## We only care about the 15 and 10 minute interval data. Add a column called 
## interval and omit NA values.
sba<-sba %>%
  mutate(interval = case_when(
    grepl(pattern = "00:00", x = Time) ~ "00",
    grepl(pattern = "10:00", x = Time) ~ "10",
    grepl(pattern = "15:00", x = Time) ~ "15",
    grepl(pattern = "20:00", x = Time) ~ "20",
    grepl(pattern = "30:00", x = Time) ~ "30",
    grepl(pattern = "40:00", x = Time) ~ "40",
    grepl(pattern = "45:00", x = Time) ~ "45",
    grepl(pattern = "50:00", x = Time) ~ "50",
  )) %>%
  filter(!is.na(interval))

# for the campus lagoon data, we want every 10 min interval
sba2<-sba2 %>%
  mutate(interval = case_when(
    grepl(pattern = "00:00", x = Time) ~ "00",
    grepl(pattern = "10:00", x = Time) ~ "10",
    grepl(pattern = "20:00", x = Time) ~ "20",
    grepl(pattern = "30:00", x = Time) ~ "30",
    grepl(pattern = "40:00", x = Time) ~ "40",
    grepl(pattern = "50:00", x = Time) ~ "50"
  )) %>%
  filter(!is.na(interval))

## Altimeter is measured in inHg and is not equal to barometric pressure. We
## need to calculate baropressure using the elevation of the station (3 m
## according to the station metadata) and convert to kPa. Multiply altimeter by
## a constant equal to ((288 - 0.0065 x h)/288)^2 where h is the elevation in m.

#constant:
constant <- ((288 - 0.0065*3)/288)^2
constant

sba <- sba %>%
  mutate(baropressure_inHg = altimeter*constant,
         ## Since the station is at sea level, the difference is negligible.
         ## Convert from inches of mercury (inHg) to kilopascals (kPa).
         baropressure_kPa = conv_unit(baropressure_inHg,"inHg","kPa"),
         ## Levellogger manual provides conversion factor for kPa to water column equivalent
         ## feet and meters.
         equivalent_ft = baropressure_kPa*0.334553,
         equivalent_m = baropressure_kPa*0.101972
         )

sba2 <- sba2 %>%
  mutate(baropressure_inHg = alti*constant,
         ## Since the station is at sea level, the difference is negligible.
         ## Convert from inches of mercury (inHg) to kilopascals (kPa).
         baropressure_kPa = conv_unit(baropressure_inHg,"inHg","kPa"),
         ## Levellogger manual provides conversion factor for kPa to water column equivalent
         ## feet and meters.
         equivalent_ft = baropressure_kPa*0.334553,
         equivalent_m = baropressure_kPa*0.101972
  )

#write to file
write_csv(sba,"data/SBA/SBA_baropressure_2025.05.10_2025.11.26.csv")
write_csv(sba2,"data/SBA/SBA_baropressure_2025.09.24_2026.01.26.csv")

# 2. Manual Barometric Compensation ----

## Conduct manual compensation for data gaps.
## Step 1: Calculate elevation difference between elevation of levelogger and 
## elevation of weather station.
## According to Solinst guide: (elevation of levelogger - elevation of weather
## station) divided by 826. This is because as elevation increases, barometric 
## pressure decreases at a rate of approximately 1.21/1000 ft or meters.

logger_elev<-read_csv("data/leveloggers/logger_elevations_2025wy.csv")
# subtract elevation of weather station 3 meters ~ 9.84252 ft
logger_elev$elev_diff_ft <- (logger_elev$elevation_ft-9.84252)/826
logger_elev

# 3. Venoco Bridge -----

## Read in Venoco Bridge data from 8/29-10/22 2025. Level is in meters.
venoco<-read_csv("data/leveloggers/Venoco_Bridge/Venoco_08.29.25_10.22.25_Uncompensated.csv", skip = 11) %>%
  clean_names() %>%
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time
  ) 

## merge with Santa Barbara Airport baropressure df
venoco_comp<-left_join(venoco,sba,by=join_by(datetime == valid)) %>% 
  # subtract elevation difference coefficient (see logger_elev) and water column equivalent
  mutate(comp_level_ft = level_ft - 0.0084800484 - equivalent_ft) %>% 
  #keep key columns
  select(datetime,comp_level_ft,temperature)

#FHJ- write_csv() to not write rownames (numbers)
write_csv(venoco_comp,"data/leveloggers/Venoco_Bridge/Venoco_08.29.25_10.22.25_Compensated.csv")

## Read in uncompensated Venoco Bridge data from 5/10-5/20. Level is in meters.
venoco_may<-read_csv("data/leveloggers/Venoco_Bridge/Venoco_11.13.24_8.29.25_Uncompensated.csv", skip = 11) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  #round datetime down to nearest minute
  #FIXME- maybe use round_date() instead, unless there is a reason to round down??
  mutate(datetime = floor_date(datetime, unit="minute")) %>% 
  ## Filter for missing data between 5/10/25 at 3:45 am and 5/20/25 at 12:45 pm.
  filter(datetime > ymd_hms("2025-05-10 03:45:00") & datetime < ymd_hms("2025-05-20 12:45:00"))

## merge with Santa Barbara Airport baropressure df
vnc_may_comp<-left_join(venoco_may,sba,by=join_by(datetime == valid)) %>% 
  ## subtract elevation difference coefficient (see logger_elev) and water column equivalent
  mutate(comp_level_ft = level_ft- 0.0084800484- equivalent_ft) %>%
  # keep relevant columns
  select(datetime,comp_level_ft,temperature)

write_csv(vnc_may_comp,"data/leveloggers/Venoco_Bridge/Venoco_05.10.25_05.20.25_Compensated.csv")

# 4. Phelps Creek ----

## Read in Phelps data from 8/29-11/18. Level is in meters.
phlp<-read_csv("data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_2025.08.29_2026.11.18_Uncompensated.csv", skip = 11) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) 

## merge with Santa Barbara Airport baropressure df
phlp_comp<-left_join(phlp,sba,by=join_by(datetime == valid)) %>% 
  ## add elevation difference coefficient (see logger_elev) and water column equivalent
  mutate(comp_level_ft = level_ft+ 0.0001785472- equivalent_ft ) %>% 
  # keep relevant columns
  select(datetime,comp_level_ft,temperature)

write_csv(phlp_comp,"data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_08.29.25_11.18.25_Compensated.csv")

## Read in uncompensated Phelps Creek data from 5/10-5/20. Level is in meters.
phlp_may<-read_csv("data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_02.20.24_08.29.25_Uncompensated.csv", skip = 11) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #new water level column in ft
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  #round datetime to nearest 15 min
  mutate(datetime = round_date(datetime,unit="15 mins")) %>% 
  # Filter for missing data between 5/10/25 at 3:45 am and 5/20/25 at 1 pm.
  filter(datetime > ymd_hms("2025-05-10 03:30:00") & datetime < ymd_hms("2025-05-20 13:15:00"))

## merge with Santa Barbara Airport baropressure df
phlp_may_comp<-left_join(phlp_may,sba,by=join_by(datetime == valid)) %>% 
  ## add/subtract elevation difference coefficient (see logger_elev) and water column equivalent
  mutate(comp_level_ft = level_ft+0.0001785472-equivalent_ft) %>% 
  # keep relevant columns
  select(datetime,comp_level_ft,temperature)

#write to file
write_csv(phlp_may_comp,"data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_05.10.25_05.20.25_Compensated.csv")

# 5. Pier ----

## Read in Pier data from 8/29-11/13. Level is in meters.
pier<-read_csv("data/leveloggers/Pier/PIER_08.29.25_11.13.25_Uncompensated.csv", skip = 13) %>% 
  clean_names() %>%
  #fix conductivity column spelling
  rename(conductivity = con_uctivity) %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) 

## merge with Santa Barbara Airport baropressure df
pier_comp<-left_join(pier,sba,by=join_by(datetime == valid)) %>% 
  # add/subtract elevation difference coefficient (see logger_elev) and water column equivalent
  mutate(comp_level_ft = level_ft- 0.0106570581- equivalent_ft) %>% 
  # keep relevant columns
  select(datetime,comp_level_ft,temperature,conductivity)

write_csv(pier_comp,"data/leveloggers/Pier/PIER_08.29.25_11.13.25_Compensated.csv")

## Read in uncompensated Pier data from 5/10-5/20. Level is in meters.
pier_may<-read_csv("data/leveloggers/Pier/PIER_10.10.23_08.29.25_Uncompensated.csv", skip = 13) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  #round datetime to nearest 15 min  
  mutate(datetime = round_date(datetime,unit="15 mins")) %>% 
  ## Filter for missing data between 5/10/25 at 3:45 am and 5/20/25 at 1 pm.
  filter(datetime > ymd_hms("2025-05-10 03:30:00") & datetime < ymd_hms("2025-05-20 13:15:00"))

## merge with Santa Barbara Airport baropressure df
pier_may_comp<-left_join(pier_may,sba,by=join_by(datetime == valid)) %>%
  ## add/subtract elevation difference coefficient (see logger_elev) and water column equivalent
  mutate(comp_level_ft = level_ft-0.0106570581-equivalent_ft) %>%
  # keep relevant columns
  select(datetime,comp_level_ft,temperature,conductivity)

# 5. Dune Pond ----

## Read in Dune Pond data from 4/20/23-09/02/25. Level is in meters.
dp<-read_csv("data/leveloggers/Dune_Pond/2171471_Dune_Pond_23.04.20_25.09.02_Uncompensated.csv", skip = 11) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  #round datetime down to nearest minute
  mutate(datetime = floor_date(datetime, unit="minute")) %>% 
  ## Filter for missing data between 5/10/25 at 3:45 am and 5/20/25 at 12:45 pm.
  filter(datetime > ymd_hms("2025-05-10 03:45:00") & datetime < ymd_hms("2025-05-20 12:45:00"))

## merge with Santa Barbara Airport baropressure df
dp_comp<-left_join(dp,sba,by=join_by(datetime == valid)) %>%
  ## add/subtract elevation difference coefficient (see logger_elev) and water 
  ## column equivalent
  mutate(comp_level_ft = level_ft-0.0050332688-equivalent_ft) %>%
  # keep relevant columns
  select(datetime,comp_level_ft,temperature)

# 6. Campus Lagoon ----

## We have barometric pressure data from the Goleta Slough barometer starting
## 11/18/2025. We will manually compensate using this script from 9/24 to 11/18.
## Read in uncompensated Campus Lagoon data from 9/24/2025-12/23/2025. Level is
## in feet.

lagoon<-read_csv(
  "data/leveloggers/Campus_Lagoon/CampusLagoon_2025.09.24_2025.12.23_Uncompensated.csv",
  skip = 11) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  #round datetime down to nearest minute
  mutate(datetime = floor_date(datetime, unit="minute")) %>% 
  ## Filter for missing data between 5/10/25 at 3:45 am and 5/20/25 at 12:45 pm.
  filter(datetime > ymd_hms("2025-09-24 08:40:00") & datetime < 
           ymd_hms("2025-11-18 06:10:00"))

## merge with Santa Barbara Airport baropressure df
lagoon_comp<-left_join(lagoon, sba2, by=join_by(datetime == valid)) %>%
  ## add/subtract elevation difference coefficient (see logger_elev) and water 
  ## column equivalent
  mutate(comp_level_ft = level - 0.0021398063 - equivalent_ft) %>%
  # keep relevant columns
  select(datetime, comp_level_ft, temperature)

write_csv(lagoon_comp,
          "data/leveloggers/Campus_Lagoon/CampusLagoon_09.24.25_11.18.25_Compensated.csv")

