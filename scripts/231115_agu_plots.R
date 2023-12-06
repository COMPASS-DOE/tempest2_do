## This script is for putting together all the figures for the TEMPEST AGU poster
##
## 2023-11-15
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")

## Prototyping a custom ggplot theme
theme_agu <- theme(
  #https://emanuelaf.github.io/own-ggplot-theme.html
)
  
contour_width = 10
contour_height = 6

# 2. Map components ------------------------------------------------------------

## Set CRS
common_crs <- 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"

## Color scheme for plots
plot_colors <- c("#F0EBD8", "#222E50","#007991")

cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.2)

# 1. US map
us <- ne_states(country = "united states of america", 
                returnclass = "sf") %>% 
  st_crop(., y = cb_bbox)
  #filter(name_en != "Alaska" & name_en != "Hawaii") 

gcrew <- tibble(name = "TEMPEST", 
                lat = 38.87392, 
                long = -76.55206) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)

ggplot() + 
  geom_sf(data = us, fill = "gray90", color = "black") + 
  geom_sf_label_repel(data = us, aes(label = name_en), 
                      size = 4,
                      #nudge_x = -0.0, nudge_y = -0.1, size = 5,
                      force = 2) + 
  geom_sf_label_repel(data = gcrew, aes(label = name), fill = "lightblue", 
                      size = 6,
                      nudge_x = 0.2, nudge_y = 0.4) + 
  geom_sf(data = gcrew, size = 6, color = "white") + 
  geom_sf(data = gcrew, size = 4, color = "blue") + 
  #coord_sf(crs = coord_sf_crs) + 
  theme_map()
ggsave("figures/agu/1_conus.pdf", width = 5, height = 7)




# 2. Plots 
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

#text_colors = c("black", "white", "white")

# custom_theme <- function() {
#   theme_minimal() +
#     theme(
#       panel.border = element_blank(),  # No panel border
#       axis.title.y = element_text(size = 11, vjust = 0.5),  # Y-axis label customization
#       axis.title.x = element_text(size = 11, vjust = 0.5),  # X-axis label customization
#       axis.text.x = element_text(size = 9),  # X-axis text size
#       axis.text.y = element_text(size = 9),  # Y-axis text size
#       panel.grid.major = element_line(color = "lightgray", size = 0.2),  # Major gridlines
#       panel.grid.minor = element_line(color = "lightgray", size = 0.2),  # Minor gridlines
#       panel.grid.major.x = element_blank(),  # No vertical major gridlines
#       panel.grid.major.y = element_blank(),  # No horizontal major gridlines
#       panel.grid.minor.x = element_blank(),  # No vertical minor gridlines
#       panel.grid.minor.y = element_blank()   # No horizontal minor gridlines
#     )
# }

# Number of desired x-axis labels
#num_labels <- 4

# x_values <- st_coordinates(polygons)[, "X"]
# y_values <- st_coordinates(polygons)[, "Y"]
# 
# # Calculate breaks
# x_breaks <- round(seq(min(x_values), max(x_values), length.out = num_labels), 4)
# y_breaks <- round(seq(min(y_values), max(y_values), length.out = num_labels), 4)

ggplot() + 
  geom_sf(data = polygons, aes(fill = plot), color = "black", alpha = 1, show.legend = F) + 
  geom_sf(data = centers, size = 4, color = "white") + 
  geom_sf(data = centers, aes(color = plot), size = 3, show.legend = F) + 
  geom_sf_label(data = centers, aes(label = plot), 
                fill = "white", alpha = 0.6, show.legend = F, 
                #nudge_x = -0.0001, 
                nudge_y = -0.00008) +
  scale_color_manual(values = plot_colors) + 
  scale_fill_manual(values = plot_colors) + 
  #scale_x_continuous(breaks = x_breaks) +
  #scale_y_continuous(breaks = y_breaks) +
  ## Also should add labels
  #coord_sf(crs = coord_sf_crs) +
  theme_map() + 
  #theme_minimal() + 
  #labs(x = "", y = "") + 
  #custom_theme() + 
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.5, "in"),
    pad_y = unit(1.7, "in"),
    bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.55, "in"), 
    pad_y = unit(1.0, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20"))
ggsave("figures/agu/2_plots.pdf", width = 4, height = 5)



# 3. Precip and VWC time-series ------------------------------------------------

## First, bring in TEROS data and set time-zone
teros <- read_csv("data/231102_teros_final.csv") %>% 
  mutate(datetime_est = with_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))
  #label_flood_periods() %>% 
  #mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))


## Plot that's for reference only
teros %>% 
  filter(datetime > "2023-06-06" & 
           datetime < "2023-06-10" & 
           plot != "Control") %>% 
  ggplot(aes(datetime, vwc, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

## Based on Slack, each flooding event was equivalent to ~ 6", so spread 6" over
## 8 hours and build a time-series (6 in ~ 15 cm)

#Based on wg-tempest Slack, average of 307k L. We'll round to 300k.
avg_gallons <- mean(81264, 81206, 80811, 80800)
avg_liters = avg_gallons * 3.78541

flood_ts <- tibble(datetime_est = seq(from = min(teros$datetime_est), 
                                      to = max(teros$datetime_est), 
                                      by = "5 min"), 
                   rain_l = 0) %>% 
  mutate(water_l = case_when(datetime_est >= dump_start1 & 
                               datetime_est <= dump_end1 ~ 300000 / (10*12), # 15 / (10*12), 
                             datetime_est >= dump_start2 & 
                               datetime_est <= dump_end2 ~ 300000 / (10*12), # 15 / (10*12),
                             TRUE ~ rain_l)) %>% 
  mutate(c_water_l = cumsum(water_l), 
         flood = case_when(datetime_est >= dump_start1 & 
                             datetime_est <= dump_end1 ~ "Flood #1", 
                           datetime_est >= dump_start2 & 
                             datetime_est <= dump_end2 ~ "Flood #2",
                           TRUE ~ NA))
  
## Make a rainfall plot. 
# plot_rain <- ggplot(flood_ts, aes(datetime_est, c_water_l)) + 
#   geom_area(alpha = 0.1) + 
#   geom_line() + 
#   geom_area(data = flood_ts %>% 
#               filter(flood == "Flood #1"), alpha = 0.1, fill = "blue") + 
#   geom_area(data = flood_ts %>% 
#               filter(flood == "Flood #2"), alpha = 0.1, fill = "blue") + 
#   labs(x = "", y = "Water added (L)")


## Make a cumulative rainfall plot
plot_rain_c <- ggplot(flood_ts, aes(datetime_est, c_water_l)) + 
  geom_area(alpha = 0.1) + 
  geom_line() + 
  geom_area(data = flood_ts %>% 
              filter(flood == "Flood #1"), alpha = 0.1, fill = "blue") + 
  geom_area(data = flood_ts %>% 
              filter(flood == "Flood #2"), alpha = 0.1, fill = "blue") + 
  annotate(geom = "text", x = as_datetime("2023-06-05 04:00:00"), y = 150000, 
           label = "Flood #1: 300k L in 10 hrs") + 
  annotate(geom = "text", x = as_datetime("2023-06-06 04:00:00"), y = 450000, 
           label = "Flood #2: 300k L in 10 hrs") + 
  scale_x_datetime(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 620000), expand = c(0, 0)) + 
  labs(x = "", y = "Water added (L)")


#4. Make TEROS VWC plot --------------------------------------------------------

## One option for making the color continuous is 
## https://stackoverflow.com/questions/62543112/how-to-make-discrete-gradient-color-bar-with-geom-contour-filled
## I might just edit them in Affinity though

p_vwc <- ggplot(teros, aes(datetime_est, depth)) + 
  geom_contour_filled(aes(z = vwc), bins = 10) + 
  annotate("rect", xmin = dump_start1, xmax = dump_end1, 
           ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
  annotate("rect", xmin = dump_start2, xmax = dump_end2, 
           ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  scale_x_datetime(expand = c(0,0)) + 
  scale_y_reverse(expand = c(0,0)) + 
  labs(x = "", y = "Depth (cm)", fill = bquote("VWC (m"^{3}/m^{3}*")")) + 
  theme(strip.background = element_rect(fill = "gray70"))

## Pull label
legend <- cowplot::get_legend(p_vwc)


# 5. combine elements for rainfall/vwc figure and export -----------------------

p_widths1 = c(1, 0.2)

p_vwc_combined <- plot_grid(p_vwc + theme(legend.position = "none"), 
                             legend, 
                             rel_widths = p_widths1)

p_rain_combined <- plot_grid(plot_rain_c + theme(legend.position = "none"), 
                            NULL, 
                            rel_widths = p_widths1)

plot_grid(p_rain_combined, 
          p_vwc_combined, 
          ncol = 1, 
          rel_heights = c(0.3, 1), 
          align = "hv")
ggsave("figures/agu/3_rain_and_vwc.pdf", 
       width = contour_width, height = contour_height * 1.3)




ggplot(teros, aes(datetime_est, vwc, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)


# 6. Create DO contour plot ----------------------------------------------------

firesting <- read_csv("data/230712_firesting.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))
  #label_flood_periods %>% 
  #mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))


ggplot(firesting, aes(datetime_est, depth)) + 
  geom_contour_filled(aes(z = do_percent_sat), bins = 10) + 
  geom_point(data = firesting %>% filter(do_percent_sat < 1), color = "orange", alpha = 0.5, size = 0.5) + 
  annotate("rect", xmin = dump_start1, xmax = dump_end1, 
           ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
  annotate("rect", xmin = dump_start2, xmax = dump_end2, 
           ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  scale_x_datetime(expand = c(0,0)) + 
  scale_y_reverse(expand = c(0,0)) + 
  scale_fill_viridis_d(direction = -1) + 
  labs(x = "", y = "Depth (cm)", fill = "DO (%)") + 
  theme(strip.background = element_rect(fill = "gray70"))
ggsave("figures/agu/4_do.pdf", 
       width = contour_width, height = contour_height)


# 7. Create anoxia / rates bar charts ------------------------------------------

anoxia_by_plot_and_depth <- read_csv("data/231116_anoxia_by_plot_and_depth.csv") %>% 
  filter(period_relabel != "Pre-Flood") %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

do_rates <- read_csv("data/231116_do_consumption_rates.csv") %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

rate_stats <- read_csv("data/231116_rate_stats.csv") %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

## Set up a color palette that differs from viridis
depth_colors <- c("#90be6d", "#43aa8b", "#4d908e", "#577590")


## Make bar-plots for summary stats
common_plot_setup <- function(...) {
  
  p <- ggplot(...) +
    geom_col(position = "dodge", color = "gray10", alpha = 0.8) +
    #scale_fill_viridis_d() +
    scale_fill_manual(values = depth_colors) +
    facet_wrap(~plot, ncol = 1) + 
    theme(#axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
      plot.title = element_text(hjust = 0.5))
  
  return(p)
}

p_do_rates <- do_rates %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  filter(consumption == TRUE) %>% 
  group_by(plot, period_relabel, depth) %>% 
  mutate(delta_do_perc_5min = lag(do_percent_sat, 1) - do_percent_sat, 
         delta_do_perc_hr = delta_do_perc_5min * 12) %>% 
  filter(delta_do_perc_5min > 0) %>% # only consumption
  summarize(mean_do_perc_hr = mean(delta_do_perc_hr), 
            median_do_perc_hr = median(delta_do_perc_hr), 
            sd_do_perc_hr = sd(delta_do_perc_hr)) %>% 
  select(plot, depth, period_relabel, mean_do_perc_hr) %>% 
  ungroup() %>% 
  add_row(plot = "Freshwater", ## Add 30cm FW for Flood #2
          depth = 30,
          period_relabel = "Flood #2",
          mean_do_perc_hr = 0) %>% 
  add_row(plot = "Seawater", ## Add 30cm FW for Flood #2
          depth = 30,
          period_relabel = "Flood #2",
          mean_do_perc_hr = 0) %>% 
  common_plot_setup(., aes(period_relabel, mean_do_perc_hr, fill = as.factor(depth))) + 
  labs(x = "", y = "DO consumption rate (% sat/hr)", fill = "Depth (cm)", title = "DO Consumption") 

# ggplot(p_do_rates, aes(period_relabel, mean_do_perc_hr, fill = as.factor(depth))) + 
#   geom_col(show.legend = F) + 
#   facet_wrap(~plot, ncol = 1) + 
#   scale_fill_manual(values = depth_colors) +
#   scale_y_continuous(trans = pseudolog10_trans) + 
#   labs(x = "", y = "Instantaneous rate (% / hr)", color = "Depth (cm)") + 
#   ggtitle("DO Consumption")
#   theme(plot.title = element_text(hjust = 0.5))

## Make individual plots
p_rates <- common_plot_setup(rate_stats, aes(period_relabel, do_perc_hr, fill = as.factor(depth))) + 
  labs(x = "", y = "Mean DO consumption rate (% sat/hr)", fill = "Depth (cm)", title = "DO consumption rates")

p_hypoxic <- common_plot_setup(anoxia_by_plot_and_depth %>% filter(period_relabel != "Pre-Flood"), 
                               aes(x = period_relabel, y = perc_hypoxic, fill = as.factor(depth))) +
  labs(x = "", y = "% of time hypoxic", fill = "Depth (cm)", title = "Hypoxia (DO < 20%)")

p_anoxic <- common_plot_setup(anoxia_by_plot_and_depth %>% filter(period_relabel != "Pre-Flood"), 
                              aes(period_relabel, perc_anoxic, fill = as.factor(depth))) + 
  labs(x = "", y = "% of 24-hr period", fill = "Depth (cm)", title = "Anoxia (DO < 1% sat)")

## Pull label
legend <- cowplot::get_legend(p_rates)

## Create combined plot
plot_grid(p_rates + theme(legend.position = "none"), 
          NULL,
          p_anoxic + theme(legend.position = "none"), 
          legend, 
          rel_widths = c(1, 0.1, 1.5, 0.3),
          nrow = 1)

ggsave("figures/agu/5_do_stats.pdf", 
       width = contour_width, height = contour_height)


# 9. Redox plot ----------------------------------------------------------------

swap <- read_csv("data/230618_swap_redox_raw.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot)) %>% 
  #label_flood_periods() %>% 
  # mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood")) %>% 
  rename("depth" = depth_cm) %>% 
  #filter(!(plot == "Control" & depth == 5)) %>% # Filter out so it's comparable to VWC/DO
  filter(!(plot == "Control" & redox_mv == 0)) %>% # manually remove errors
  filter(!(plot == "Seawater" & redox_mv == 0)) %>% 
  filter(!is.na(eh_mv))

ggplot(swap, aes(datetime_est, depth)) + 
  geom_contour_filled(aes(z = eh_mv), bins = 10) + 
  #geom_point(data = swap %>% filter(eh_mv < 300), color = "orange", alpha = 0.5, size = 0.5) + 
  #geom_point(data = firesting %>% filter(do_percent_sat < 1), color = "orange", alpha = 0.5, size = 0.5) + 
  annotate("rect", xmin = dump_start1, xmax = dump_end1, 
           ymin = 5, ymax = 50, fill = "white", alpha = 0.2) +
  annotate("rect", xmin = dump_start2, xmax = dump_end2, 
           ymin = 5, ymax = 50, fill = "white", alpha = 0.2) +
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  scale_x_datetime(expand = c(0,0)) + 
  scale_y_reverse(expand = c(0,0)) + 
  scale_fill_viridis_d(direction = -1) + 
  labs(x = "", y = "Depth (cm)", fill = "Eh (mV)") + 
  theme(strip.background = element_rect(fill = "gray70"))
ggsave("figures/agu/6_redox.pdf", 
       width = contour_width, height = contour_height)

ggsave("figures/agu/6_redox_tall.pdf", 
       width = contour_width * .5, height = contour_height)

swap %>% 
  drop_na() %>% 
  group_by(depth, plot) %>% 
  summarize(n = n(), 
            min_eh = min(eh_mv), 
            max_eh = max(eh_mv)) %>% 
  mutate(delta_eh = max_eh - min_eh, 
         percent = (delta_eh / max_eh) * 100) %>% 
  filter(plot != "Control")

ggplot(swap, aes(datetime_est, eh_mv, color = plot)) + 
  geom_line() + 
  facet_wrap(~depth, ncol = 1)




















## Combine vwc w DO and make hysteresis
vwc_do_cols <- c("datetime_est", "plot", "depth")

vwc_do <- inner_join(teros %>% select(all_of(vwc_do_cols), vwc), 
                     firesting %>% select(all_of(vwc_do_cols), do_percent_sat), 
                     by = c(vwc_do_cols)) %>% 
  mutate(datetime = datetime_est) %>% 
  label_flood_periods()

vwc_do %>% 
  filter(grepl("#", period_relabel)) %>% 
  filter(plot != "Control") %>% 
  ggplot(aes(vwc, do_percent_sat)) + 
  geom_point(aes(color = period_relabel)) + 
  facet_wrap(plot~depth, scales = "free")


## Cusums


## Check plot to make sure data are clean
ggplot(swap, aes(datetime_est, eh_mv, color = interaction(depth))) + 
  geom_line() + facet_wrap(~plot, ncol = 1)

common_cols <- c("datetime_est", "depth", "plot")

combined_data <- inner_join(firesting %>% select(all_of(common_cols), do_percent_sat), 
                            teros %>% select(all_of(common_cols), vwc),
                 by = common_cols) %>% 
  inner_join(swap %>% select(all_of(common_cols), eh_mv, redox_mv), 
             by = common_cols) %>% 
  mutate(depth = as.factor(depth)) %>% 
  filter(depth == "5" | depth == "30") #%>% 
  #filter(datetime_est < "2023-06-09")

x <- combined_data %>% 
  select(datetime_est, depth, plot, do_percent_sat) %>% 
  mutate(z = (do_percent_sat - mean(do_percent_sat)) / sd(do_percent_sat))

inner_join(x %>% filter(plot == "Control") %>% select(-plot) %>% rename(do_control = do_percent_sat),
           x %>% filter(plot == "Freshwater") %>% select(-plot) %>% rename(do_fw = do_percent_sat), 
           by = c("datetime_est", "depth")) %>% 
  mutate(delta_do = do_fw - mean(do_control)) %>% 
  ggplot(aes(datetime_est, delta_do, color = depth)) + geom_line()

sds <- combined_data %>% 
  group_by(plot, depth) %>% 
  summarize(sd = sd(do_percent_sat))



  mutate(z = (do_percent_sat - mean(do_percent_sat)) / sd(do_percent_sat)) %>% 
ggplot(aes(datetime_est, z, color = plot)) + 
  geom_line() + 
  facet_wrap(~depth)





control_plot <- function(var, direction, y_label, legend_position){
  
  # 1. Calculate standard deviation of the 
  control_sd <- sd(combined_data %>% 
                     filter(plot == "Control") %>% 
                     pull({{var}}))
  
  x <- combined_data %>% 
    mutate(depth = case_when(depth == 5 ~ " 5 cm", 
                             depth == 30 ~ "30 cm")) %>% 
    #filter(plot != "Control") %>% 
    group_by(plot, depth) %>% 
    #filter(plot == "Freshwater" & depth == 5) %>% 
    mutate(z_raw = (({{var}} - mean({{var}})) / control_sd)) %>% 
    mutate(z = z_raw - first(z_raw)) #force to 0
  
  if (direction == "increase") {
    p <- ggplot(x, aes(datetime_est, z, linetype = plot)) + 
      geom_line() + 
      geom_hline(yintercept = c(control_sd, control_sd * -1), linetype = "dashed", color = "gray50") + 
      add_line() + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = control_sd, ymax = Inf, fill = "blue", alpha = 0.2) + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = control_sd * -1, ymax = control_sd * -1, fill = "forestgreen", alpha = 0.2) + 
      facet_wrap(~depth, ncol = 1) + 
      labs(x = "", y = y_label, linetype = "")  + 
      theme(legend.position = legend_position, 
            legend.background = element_blank(), 
            legend.key = element_rect(fill = NA))
  } else if (direction == "decrease") {
    p <- ggplot(x, aes(datetime_est, z, linetype = plot)) + 
      geom_line() + 
      geom_hline(yintercept = c(control_sd, control_sd * -1), linetype = "dashed", color = "gray50") + 
      add_line() + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = control_sd * -1, ymax = control_sd, fill = "forestgreen", alpha = 0.2) + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = -Inf, ymax = control_sd * -1, fill = "blue", alpha = 0.2) + 
      facet_wrap(~depth, ncol = 1) + 
      labs(x = "", y = y_label, linetype = "") + 
      theme(legend.position = legend_position, 
            legend.background = element_blank(), 
            legend.key = element_rect(fill = NA))
  } else {
    stop("Invalid direction. Use 'increase' or 'decrease'.")
  }
  
  return(p)
}

c_vwc <- control_plot(vwc, "increase", "Normalized VWC", c(0.8, 0.9))
c_do <- control_plot(do_percent_sat, "decrease", "Normalized DO", c(0.8, 0.8))
c_redox <- control_plot(eh_mv, "decrease", "Normalized Eh", c(0.8, 0.8))

plot_grid(c_vwc, c_do, c_redox, nrow = 1)
ggsave("figures/agu/6_controls.pdf", width = 12, height = 5)

ggplot(combined_data, aes(datetime_est, redox_mv, color = depth)) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)
  
control_sd <- sd(combined_data %>% 
                   filter(plot == "Control") %>% 
                   pull(redox_mv))


control_plot2 <- function(var, direction, y_label, legend_position){
  
  control_sd <- sd(combined_data %>% 
                     filter(plot == "Control") %>% 
                     pull({{var}}))
  
  x <- combined_data %>% 
    mutate(depth = case_when(depth == 5 ~ " 5 cm", 
                             depth == 30 ~ "30 cm")) %>% 
    filter(plot != "Control") %>% 
    group_by(plot, depth) %>% 
    #filter(plot == "Freshwater" & depth == 5) %>% 
    mutate(z_raw = (({{var}} - mean({{var}})) / control_sd)) %>% 
    mutate(z = z_raw - first(z_raw))
  
  if (direction == "increase") {
    p <- ggplot(x, aes(datetime_est, z, color = plot)) + 
      geom_line(lwd = 0.5) + 
      geom_hline(yintercept = c(control_sd, control_sd * -1), linetype = "dashed", color = "gray50") + 
      add_line() + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = control_sd, ymax = Inf, fill = "forestgreen", alpha = 0.2) + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = control_sd * -1, ymax = control_sd * -1, fill = "red", alpha = 0.2) + 
      facet_wrap(~depth, ncol = 1) + 
      scale_color_manual(values = c("blue", "black")) + 
      labs(x = "", y = y_label, color = "")  + 
      theme(legend.position = legend_position, 
            legend.background = element_blank(), 
            legend.key = element_rect(fill = NA))
  } else if (direction == "decrease") {
    p <- ggplot(x, aes(datetime_est, z, color = plot)) + 
      geom_line(lwd = 0.5) + 
      geom_hline(yintercept = c(control_sd, control_sd * -1), linetype = "dashed", color = "gray50") + 
      add_line() + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = control_sd * -1, ymax = control_sd, fill = "forestgreen", alpha = 0.2) + 
      annotate("rect", xmin = as_datetime(min(x$datetime_est)), 
               xmax = as_datetime(max(x$datetime_est)), 
               ymin = -Inf, ymax = control_sd * -1, fill = "red", alpha = 0.2) + 
      facet_wrap(~depth, ncol = 1) + 
      scale_color_manual(values = c("blue", "black")) + 
      labs(x = "", y = y_label, color = "") + 
      theme(legend.position = legend_position, 
            legend.background = element_blank(), 
            legend.key = element_rect(fill = NA))
  } else {
    stop("Invalid direction. Use 'increase' or 'decrease'.")
  }
  
  return(p)
}

c2_vwc <- control_plot2(vwc, "increase", "Normalized VWC", c(0.8, 0.9))
c2_do <- control_plot2(do_percent_sat, "decrease", "Normalized DO", c(0.8, 0.8))
c2_redox <- control_plot2(redox_mv, "decrease", "Normalized Redox", c(0.8, 0.8))

plot_grid(c2_vwc, c2_do, c2_redox, nrow = 1)
ggsave("figures/agu/6_controls2.pdf", width = 12, height = 5)


