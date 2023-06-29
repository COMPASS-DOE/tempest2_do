## This script constructs the map and plots figures showing the location. 
## Current plan: 3 panels: 1) Left: Map of Chesapeake Bay with GCREW marked
## 2) Map of the 3 plots, but in lower right, and 3) map of a single plot
## blown up to show the locations of sensors and w a legend
##
## 2023-06-28
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")

## Set CRS
common_crs <- 4326


# 2. Create Chesapeake Bay map (Panel 1) ---------------------------------------

## Set regional and CB (inset) bounding boxes
cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.2)

us <- ne_states(country = "united states of america", 
                returnclass = "sf") %>% 
  st_crop(cb_bbox)

ggplot() + 
  geom_sf(data = us)






## Potentially of use: survey points for FW and SW
## https://drive.google.com/drive/folders/1zfpD20z1PuMFFQRGoPJA2HIRseWhxx0J