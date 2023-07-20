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


# 3. Read in FW/SW plot grid lat-longs -----------------------------------------

fw_coords <- read_csv("data/metadata/TEMPEST_FW_Grid_LatLong.csv")
sw_coords <- read_csv("data/metadata/TEMPEST_SW_Grid_LatLong.csv") %>% 
  mutate(LatDD = str_sub(LatDD, start = 1, end = -2), 
         LongDD = as.numeric(str_sub(LongDD, start = 1, end = -2)) * -1)

format_coords <- function(data){
  ## $ = end of string in regex
  grid_cells = c("A1$", "A9$", "K1$", "K9$")
  
  data %>% 
    clean_names() %>% 
    select(point_name, lat_dd, long_dd) %>% 
    filter(grepl(paste(grid_cells, collapse='|'), point_name)) %>% 
    mutate(grid_cell = str_sub(point_name, 4, 5), 
           plot = str_sub(point_name, 1, 2)) %>% 
    mutate(lat = as.numeric(lat_dd), 
           long = as.numeric(long_dd)) %>% 
    select(plot, grid_cell, lat, long)
}

## First, combine the edge-points and 
plots_sf <- bind_rows(format_coords(fw_coords), 
          format_coords(sw_coords)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs) 


polygons <- plots_sf %>% 
  dplyr::group_by(plot) %>% 
  dplyr::summarize() %>%
  st_cast("POLYGON") %>% 
  st_convex_hull()

ggplot() + 
  geom_sf(data = polygons, aes(fill = plot), alpha = 0.5) 
  #geom_sf(data = plots_sf) 






fw_teros_coords <- read_csv("data/metadata/TEMPEST_FW_Teros_DD.csv") %>% 
  mutate(LatDD = str_sub(LatDD, start = 1, end = -2), 
         LongDD = as.numeric(str_sub(LongDD, start = 1, end = -2)) * -1)

## Potentially of use: survey points for FW and SW
## https://drive.google.com/drive/folders/1zfpD20z1PuMFFQRGoPJA2HIRseWhxx0J