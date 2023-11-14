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
source("scripts/0_0_setup.R")

## Set CRS
common_crs <- 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"

## Color scheme for plots
#plot_colors <- c("gray", "blue","forestgreen")
plot_colors <- c("gray", "blue","blue")

# 2. Create Chesapeake Bay map (Panel 1) ---------------------------------------

## Set regional and CB (inset) bounding boxes
cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.2)
conus_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)

us <- ne_states(country = "united states of america", 
                returnclass = "sf") %>% 
  filter(name_en != "Alaska" & name_en != "Hawaii")

gcrew <- tibble(name = "gcrew", 
                lat = 38.87392, 
                long = -76.55206) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)
  
  
ggplot() + 
  geom_sf(data = us, fill = "gray90", color = "black") + 
  geom_sf(data = gcrew, size = 4, color = "white") + 
  geom_sf(data = gcrew, size = 2.5) + 
  coord_sf(crs = coord_sf_crs) + 
  theme_map()
ggsave("figures/fig1_elements/1_conus.pdf", width = 4, height = 3)


# 3. Read in FW/SW plot grid lat-longs -----------------------------------------

fw_coords <- read_csv("data/metadata/TEMPEST_FW_Grid_LatLong.csv")
sw_coords <- read_csv("data/metadata/TEMPEST_SW_Grid_LatLong.csv") %>% 
  mutate(LatDD = as.numeric(str_sub(LatDD, start = 1, end = -2)), 
         LongDD = as.numeric(str_sub(LongDD, start = 1, end = -2)) * -1)

## Since I don't currently have coordinates, we're going to make them up for now
## by transposing SW down
ct_coords <- sw_coords %>% 
  dplyr::mutate(LatDD = LatDD + 0.0006, 
                LongDD = LongDD + 0.0001, 
                PointName_ = str_replace_all(PointName_, "SW_", "CT_")) 

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
          format_coords(sw_coords), 
          format_coords(ct_coords)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs) 

polygons <- plots_sf %>% 
  dplyr::group_by(plot) %>% 
  dplyr::summarize() %>%
  st_cast("POLYGON") %>% 
  st_convex_hull() %>% 
  dplyr::mutate(plot = case_when(plot == "CT" ~ "Control", 
                                 plot == "FW" ~ "Freshwater", 
                                 plot == "SW" ~ "Seawater"))

centers <- polygons %>% 
  st_centroid()

## Read in redox sensor locations - leaving out as coordinates make no sense
# redox_latlongs <- read_csv("data/HydraGO_DATA_20230712_110920.csv") %>% 
#   clean_names() %>% 
#   select(latitude, longitude, site) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = common_crs)

ggplot() + 
  geom_sf(data = polygons, aes(fill = plot), color = "black", alpha = 0.2, show.legend = F) + 
  geom_sf(data = centers, size = 4, color = "white") + 
  geom_sf(data = centers, aes(color = plot), size = 2.5, show.legend = F) + 
  scale_color_manual(values = plot_colors) + 
  scale_fill_manual(values = plot_colors) + 
  ## Also should add labels
  #coord_sf(crs = coord_sf_crs) +
  theme_map() + 
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.3, "in"),
    pad_y = unit(1.2, "in"),
    bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), 
    pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20"))
ggsave("figures/fig1_elements/2_plots.pdf", width = 4, height = 5)




# fw_teros_coords <- read_csv("data/metadata/TEMPEST_FW_Teros_DD.csv") %>% 
#   mutate(LatDD = as.numeric(str_sub(LatDD, start = 1, end = -2)), 
#          LongDD = as.numeric(str_sub(LongDD, start = 1, end = -2)) * -1)

## Potentially of use: survey points for FW and SW
## https://drive.google.com/drive/folders/1zfpD20z1PuMFFQRGoPJA2HIRseWhxx0J

## Now trim teros coordinates to the ones we used


# 
# p2_1 <- ggplot() + 
#   geom_sf(data = polygons, aes(fill = plot), alpha = 0.5) 
# #geom_sf(data = plots_sf) 
# 
# p2_2 <- ggplot() + 
#   geom_sf(data = polygons %>% filter(plot == "FW"), 
#           aes(fill = plot), alpha = 0.5, show.legend = F) + 
#   theme_map()
# 
# p2 <- plot_grid(p2_1, p2_2, ncol = 1, rel_heights = c(0.5, 1))
#   
#   
# plot_grid(p1, p2, nrow = 1)
# ggsave("figures/231018_map_v1.png", width = 8, height = 4)




