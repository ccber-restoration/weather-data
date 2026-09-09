# =============================================================================
# Name:           2025_wy_tides.R
# Description:    preps & plots NOAA tide predictions, to use with levelogger viz

# Author(s):      Francis Joyce

# Inputs:         csv in data/NOAA_tide_predictions      
# Outputs:        plots of tidal predictions      
# 
# Notes:         This script is for use with a specific date range (2025 water year) 
#                 
# =============================================================================

library(tidyverse)

#read in tide data

#note that there are 13 lines of metadata at the top of the txt file
tides_2025_02 <- read_delim(file = "data/NOAA_tide_predictions/NOAA_tide_preds_NAVD_20250210_20250310.txt",
                            skip = 14,
                            delim = "\t",
                            col_names = c("date", "day", "time", "pred")) %>% 
  select(date:pred) %>% 
  mutate(datetime = as.POSIXct(date) + time)

tides_2025_03 <- read_delim(file = "data/NOAA_tide_predictions/NOAA_tide_preds_NAVD_20250311_20250410.txt",
                            skip = 14,
                            delim = "\t",
                            col_names = c("date", "day", "time", "pred")) %>% 
  select(date:pred) %>% 
  mutate(datetime = as.POSIXct(date) + time)

tides_2025 <- bind_rows(tides_2025_02, tides_2025_03) %>% 
  filter(date < as.Date("2025-04-11"))

#quick plot

tide_plot_2025_02 <- ggplot(data = tides_2025, aes(x = datetime, y = pred)) +
  geom_line()

tide_plot_2025_02
