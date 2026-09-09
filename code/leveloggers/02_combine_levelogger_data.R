# =============================================================================
# Name:           venoco_levelogger_combine.R
# Description:    combines compensated water level data files and converts to water surface elevation.

# Originally written 2025-11-18
# Author(s):      Francis Joyce, Claire WS

# Inputs:         Compensated CSVs, either automatically compensated using the
#                 Solinst software, or manually compensated using 
#                 01_barologger_data_gap_fic.R; CSVs are stored in Box
# Outputs:        Combined CSVs
# 
# Notes:          Revised 3/3/2026 to combine Campus Lagoon data
#                 
# =============================================================================

# load libraries ----
library(tidyverse)
library(janitor)
library(cowplot)
library(scales)
library(measurements)
library(hms)

#interactive plots for exploration
library(plotly)

# define start and end of 2025 water year

#as datetimes
wy_2025_start <- as.POSIXct("2024-10-01 00:00:00", tz = "America/Los_Angeles" )
wy_2025_end <- as.POSIXct("2025-09-30 23:59:59", tz = "America/Los_Angeles")

#as dates
wy_2025_start_date <- ymd("2024-10-01", tz = "America/Los_Angeles")
wy_2025_end_date <- ymd("2025-09-30", tz = "America/Los_Angeles")

# define start and end of 2025 & 2026 water year

#as datetimes
wy_2025_2026_start <- as.POSIXct("2024-10-01 00:00:00", tz = "America/Los_Angeles" )
wy_2025_2026_end <- as.POSIXct("2026-03-30 23:59:59", tz = "America/Los_Angeles")

#as dates
wy_2025_2026_start_date <- ymd("2024-10-01", tz = "America/Los_Angeles")
wy_2025_2026_end_date <- ymd("2026-03-30", tz = "America/Los_Angeles")

# logger elevations ----

#ordered from "downstream" (lower) to upstream (higher)

#note that these elevations are in feet
logger_elev <- read_csv("data/leveloggers/logger_elevations_2025wy.csv")

# Individual stations ----

## Pier ----
pier_2025_wy_a <- read_csv(file = "data/leveloggers/Pier/PIER_03.19.24_05.10.25_Compensated.csv", skip = 13) %>% 
  clean_names() %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = conv_unit(level, "m", "ft")) %>% 
  select(-c(date:ms, level))

pier_2025_wy_b <- read_csv(file = "data/leveloggers/Pier/PIER_05.10.25_05.20.25_Compensated.csv") %>% 
  clean_names()

pier_2025_wy_c <- read_csv(file = "data/leveloggers/Pier/PIER_05.20.24_08.29.25_Compensated.csv", skip = 13) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    comp_level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  select(-(date:level))

pier_2025_wy_d <- read_csv(file = "data/leveloggers/Pier/PIER_08.29.25_11.13.25_Compensated.csv") %>% 
  clean_names()

#combine four shorter timeseries into one covering most of 2025 water year
pier_20240319_20251113 <- bind_rows(pier_2025_wy_a, pier_2025_wy_b, pier_2025_wy_c, pier_2025_wy_d) %>% 
  mutate(
    #parse date from character to date format
    date = date(datetime),
    #create water surface elevation column, by logger elevation
    wse = comp_level_ft + 3.4,
    station = "Pier") %>% 
  # there are also a bunch of NAs bw 2025-09-02 and 2025-10-29
  #filter(is.na(comp_level_ft)) %>% 
  #there is one anomalous value (obvious error) on 2025-11-02 22:15:00
  #crude way of filtering out high values...
  filter(comp_level_ft < 20)
  
#declutter
remove(pier_2025_wy_a, pier_2025_wy_b, pier_2025_wy_c, pier_2025_wy_d)

#plot Pier water levels over 2025 water year
pier_wse_fig <- ggplot(data = pier_20240319_20251113, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  labs(title = "Devereux Slough Pier") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_datetime(date_breaks =  "2 month", 
                   date_minor_breaks = "1 month",
                   date_labels = "%b %Y",
                   limits = c(wy_2025_start, wy_2025_end))

pier_wse_fig

#ggsave(filename = "figures/Pier_wse_wy25_DRAFT.png", plot = pier_wse_fig)

## Venoco bridge ----

#note level is in meters and temp is in C
#not sure what ms column is
venoco_2025_wy_a <- read_csv("data/leveloggers/Venoco_Bridge/Venoco_11.13.24_05.10.25_Compensated.csv", skip = 11) %>% 
  clean_names() %>%
  select(-ms) %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = conv_unit(level, "m", "ft")
)
  
# already in feet  
venoco_2025_wy_b <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_05.10.25_05.20.25_Compensated.csv") %>% 
  clean_names() %>%
  mutate(date = date(datetime),
         time = as_hms(datetime))
  
venoco_2025_wy_c <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_05.20.25_08.29.25_Compensated.csv", skip = 11) %>% 
  clean_names() %>% 
  select(-ms) %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = conv_unit(level, "m", "ft")
  )

#already in feet
venoco_2025_wy_d <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_08.29.25_10.22.25_Compensated.csv") %>% 
  clean_names() %>% 
  mutate(date = date(datetime),
         time = as_hms(datetime))

# combine four different data frames
venoco_2024_11_13_2025_10_22 <- bind_rows(venoco_2025_wy_a, venoco_2025_wy_b, venoco_2025_wy_c, venoco_2025_wy_d) %>% 
  clean_names() %>% 
  mutate(
    #adjust water level by logger elevation
    wse = comp_level_ft + 2.838,
    station = "Venoco") 

#declutter
remove(venoco_2025_wy_a, venoco_2025_wy_b, venoco_2025_wy_c, venoco_2025_wy_d)


#test plot
venoco_wse_fig <- ggplot(data = venoco_2024_11_13_2025_10_22, aes(x = datetime, y = wse)) +
  theme_cowplot() +
  geom_line() +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  labs(title = "Venoco Bridge (north side)") +
  scale_y_continuous(limits = c(0,NA), breaks = breaks_width(2)) +
  scale_x_datetime(date_breaks =  "1 month", 
               date_minor_breaks = "1 month",
               date_labels = "%b",
               limits = c(wy_2025_start, wy_2025_end),
               expand = c(0,0)
               )


venoco_wse_fig

#make interactive
ggplotly(venoco_wse_fig)

#ggsave(filename = paste("figures/Venoco_Bridge_wse_wy25_",
 #                       format(Sys.time(), "%Y-%m-%d"),
  #                      ".pdf"), 
   #                     plot = venoco_wse_fig)


## East Bridge ----

# we have data up to 11/13/24 due to a levelogger malfunction. instrument was
# replaced on 10/22/25. data collection at this location continues from then on.

## Phelps Creek - Marymount Bridge  ----

phelps_wy_25_a <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_03.19.24_05.10.25_Compensated.csv", skip = 11) %>% 
  clean_names() %>%
  select(-ms) %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #convert level from m to ft
    comp_level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) 

phelps_wy_25_b <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_05.10.25_05.20.25_Compensated.csv") %>%
  mutate(date = date(datetime),
         time = as_hms(datetime))

phelps_wy_25_c <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_05.20.25_08.29.25_Compensated.csv", skip = 11) %>%
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #convert level from m to ft
    comp_level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  select(-ms)
 

phelps_wy_26_a <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_08.29.25_11.18.25_Compensated.csv") %>%
  mutate(date = date(datetime),
         time = as_hms(datetime))

# adding phelps data from 11/18/26-03/03/26
phelps_wy_26_b <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_2025.11.18_2026.03.03_Compensated.csv", skip = 11) %>%
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #convert level from m to ft
    comp_level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  select(-ms)

# adding phelps data from 3/3/26 to 4/1/26
phelps_wy_26_c <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_2026.03.03_2026.04.01_Compensated.csv", skip = 11) %>%
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #convert level from m to ft
    comp_level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) %>% 
  select(-ms)

#combine six Phelps dataframes into one

phelps_wy_25_26 <- bind_rows(phelps_wy_25_a, phelps_wy_25_b, phelps_wy_25_c,
                             phelps_wy_26_a, phelps_wy_26_b, phelps_wy_26_c) %>% 
  #remove level column in meters
  select(-level) %>% 
  mutate(
         #adjust water level by logger elevation
         wse = comp_level_ft + 9.99,
         station = "Phelps")

#write combined data to file
write_csv(phelps_wy_25_26,
          "data/leveloggers/Phelps_Creek_Marymount_Bridge/combined/Phelps_2024.03.19_2026.04.01_Compensated.csv")

#declutter
remove(phelps_wy_25_a, phelps_wy_25_b, phelps_wy_25_c, phelps_wy_26_a,
       phelps_wy_26_b, phelps_wy_26_c)

# test plot for wy 25
Phelps_wse_fig <- ggplot(data = phelps_wy_25, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water elevation (ft)") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_datetime(date_breaks =  "2 month", 
                   date_minor_breaks = "1 month",
                   date_labels = "%b %Y",
                   limits = c(wy_2025_start, wy_2025_end))

Phelps_wse_fig

# plot for wy 25 and wy 26 up through March

Phelps_wse_2526_fig <- ggplot(data = phelps_wy_25_26, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water elevation (ft)") +
  xlab("Date") +
  scale_y_continuous(limits = c(10,15)) +
  scale_x_datetime(date_breaks =  "2 month", 
                   date_minor_breaks = "1 month",
                   date_labels = "%b %Y",
                   limits = c(wy_2025_2026_start, wy_2025_2026_end))

Phelps_wse_2526_fig

# Assemble water level + precip multi-panel fig ----

## combine different stations into one data frame ----

wse_2025_combined <- bind_rows(pier_20240319_20251113, 
                               venoco_2024_11_13_2025_10_22,
                               phelps_wy_25) %>% 
  select(-c(conductivity, time, level)) 

#aggregate by taking mean of daily wse, to smooth out figure
wse_2025_combined_daily_mean <- wse_2025_combined %>% 
  group_by(date, station) %>% 
  summarize(wse= mean(wse))

wse_fig <- ggplot(data = wse_2025_combined_daily_mean, aes(x = date, y = wse, color = station)) +
  geom_line() +
  theme_cowplot() +
  theme(legend.position = c(0.05, 0.65),
        legend.box.background = element_rect(color = "black", 
                                             fill = "white", 
                                             linewidth = 0.5, 
                                             linetype = "solid"),
        legend.box.margin = margin(t = 4, r = 4, b = 4, l = 4)) +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,NA), 
                     breaks = breaks_width(2)) +
  scale_x_date(date_breaks =  "3 month", 
                   date_minor_breaks = "1 month",
                   date_labels = "%b %Y",
                   limits = c(wy_2025_start_date, wy_2025_end_date),
               expand = c(0,NA))

wse_fig

#figure without smoothing
wse_raw_fig <- wse_fig <- ggplot(data = wse_2025_combined, aes(x = date, y = wse, color = station)) +
  geom_line() +
  theme_cowplot() +
  theme(legend.position = c(0.05, 0.5),
        legend.box.background = element_rect(color = "black", 
                                             fill = "white", 
                                             linewidth = 0.5, 
                                             linetype = "solid"),
        legend.box.margin = margin(t = 4, r = 4, b = 4, l = 4)) +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,NA), 
                     breaks = breaks_width(2)) +
  scale_x_date(date_breaks =  "1 month", 
               date_minor_breaks = "1 month",
               date_labels = "%b",
               limits = c(wy_2025_start_date, wy_2025_end_date),
               expand = c(0,NA))


wse_raw_fig

ggsave(filename = paste("figures/2025_wy_wse_raw_",
                        format(Sys.time(), "%Y-%m-%d"),
                        ".pdf"), 
       plot = wse_raw_fig,
       width = 8.6,
       height = 6,
       units = "in"
)



#existing precipitation figure from "01_summarize_weather.R" script
source("code/met_station/2025_wy_precip.R")

precip_fig 

## combine water level and precip plot ----
wse_precip_fig <- plot_grid(
  wse_fig + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
    ),
  precip_fig + scale_x_date(limits = c(wy_2025_start_date, wy_2025_end_date),
                            expand = c(0,NA),
                            date_breaks = "3 month",
                            date_labels = "%b %Y"),
  nrow = 2,
  align = "v"
)

wse_precip_fig

#save to file
ggsave(filename = paste("figures/2025_wy_wse_precip_",
                                    format(Sys.time(), "%Y-%m-%d"),
                                     ".pdf"), 
                                    plot = wse_precip_fig,
       width = 8.6,
       height = 6.4,
       units = "in"
       )

# combine Campus Lagoon data into one dataframe ----

lagoon_2025_wy_a <- read_csv(
  file = "data/leveloggers/Campus_Lagoon/CampusLagoon_09.24.25_11.18.25_Compensated.csv") %>% 
  clean_names()

lagoon_2025_wy_b <- read_csv(
  file = "data/leveloggers/Campus_Lagoon/CampusLagoon_2025.11.18_2025.12.23_Compensated.csv",
  skip = 11) %>% 
  clean_names() %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = level) %>% 
  select(-c(date:ms, level))

lagoon_2025_wy_c <- read_csv(
  file = "data/leveloggers/Campus_Lagoon/CampusLagoon_2025.12.23_2026.03.03_Compensated.csv",
  skip = 11) %>% 
  clean_names() %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = level) %>% 
  select(-c(date:ms, level))

#combine three shorter timeseries into one covering most of 2025 water year
lagoon_20250924_20260303 <- bind_rows(lagoon_2025_wy_a, lagoon_2025_wy_b,
                                    lagoon_2025_wy_c)

write_csv(lagoon_20250924_20260303, 
          "data/leveloggers/Campus_Lagoon/CampusLagoon_09.24.25_03.03.26_Compensated.csv")

#declutter
remove(lagoon_2025_wy_a, lagoon_2025_wy_b, lagoon_2025_wy_c)
