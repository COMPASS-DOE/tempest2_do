## This script is for making the figures used on the ESS PI meeting poster
##
## 2024-03-19
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")

contour_width = 10
contour_height = 6


# 2. Read data -----------------------------------------------------------------

## Read in the three datasets used
teros <- read_csv("data/240326_teros_final.csv") %>% 
  mutate(datetime_est = with_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

firesting <- read_csv("data/240318_firesting_data_final.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

swap <- read_csv("data/ignore/archive/230618_swap_redox_raw.csv") %>% 
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


# 3. Synthesize a flood dataset ------------------------------------------------

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


# 3.5. Make a combined plot ----------------------------------------------------

make_contour_plot <- function(data, var, max_depth, x_lab, y_lab, fill_lab, legend_width, fill_direction){
  
  data_plot <- ggplot(data, aes(datetime_est, depth)) + 
    geom_contour_filled(aes(z = {{var}}), bins = 10, show.legend = F) + 
    annotate("rect", xmin = dump_start1, xmax = dump_end1, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = dump_start2, xmax = dump_end2, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    scale_x_datetime(expand = c(0,0)) + 
    scale_y_reverse(expand = c(0,0)) + 
    scale_fill_viridis_d(direction = fill_direction) + 
    labs(x = x_lab, y = y_lab, fill = fill_lab) + 
    theme(strip.background = element_rect(fill = "gray90"), 
          axis.text = element_text(size = 14),    # Adjust size of axis labels
          axis.title = element_text(size = 16),  
          strip.text = element_text(size = 14)) +  # Remove background from legend
    theme(panel.background = element_blank(), 
          plot.background = element_blank())
  
  legend_raw <- ggplot(data, aes(datetime_est, depth)) +
    geom_tile(aes(fill = {{var}})) +
    facet_wrap(~plot, ncol = 1) + 
    scale_fill_viridis_c(direction = -1) + 
    labs(fill = fill_lab) + 
    theme(legend.background = element_blank())
  
  legend <- get_legend(legend_raw)
  
  plot_grid(data_plot, legend, rel_widths = c(1, legend_width))
}

plot_grid(make_contour_plot(teros, vwc, 30, "", "Soil depth (cm)", bquote("VWC (m"^{3}/m^{3}*")"), 0.15, 1),
          NULL,
          make_contour_plot(firesting, do_percent_sat, 30, "", "Soil depth (cm)", "DO (% Sat)", 0.15, -1), 
          NULL,
          make_contour_plot(swap, eh_mv, 50,  "Datetime", "Soil depth (cm)","Eh (mV)", 0.15, -1),
          rel_heights = c(1, 0.1, 1, 0.1, 1),
          ncol = 1) 
ggsave("figures/ess_pi/240402_timeseries.png", width = 10, height = 20)


# 4. Make flood time-series and VWC response plot ------------------------------

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
  scale_y_continuous(limits = c(0, 620000), expand = c(0, 0), 
                     labels = c("0", "2e5", "4e5", "6e5")) + 
  labs(x = "", y = "Water added (L)")

p_vwc <- ggplot(teros, aes(datetime_est, depth)) + 
  geom_contour_filled(aes(z = vwc), bins = 10, show.legend = F) + 
  annotate("rect", xmin = dump_start1, xmax = dump_end1, 
           ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
  annotate("rect", xmin = dump_start2, xmax = dump_end2, 
           ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  scale_x_datetime(expand = c(0,0)) + 
  scale_y_reverse(expand = c(0,0)) + 
  labs(x = "", y = "Depth (cm)") + 
  theme(strip.background = element_rect(fill = "gray70"))

vwc_legend_raw <- ggplot(teros, aes(datetime_est, depth)) +
  geom_tile(aes(fill = vwc)) +
  facet_wrap(~plot, ncol = 1) + 
  scale_fill_viridis_c() + 
  labs(fill = bquote("VWC (m"^{3}/m^{3}*")"))

vwc_legend <- get_legend(vwc_legend_raw)

vwc_combined <- plot_grid(p_vwc, vwc_legend, rel_widths = c(1, 0.3))

plot_grid(plot_rain_c, NA, 
          p_vwc, 
          vwc_legend, 
          rel_heights = c(0.3, 1), 
          rel_widths = c(1, 0.2))
ggsave("figures/ess_pi/vwc.png", width = 10, height = 8)


# 5. Make DO and Redox plot function -------------------------------------------

make_contour_plot <- function(data, var, max_depth, fill_lab, legend_width){
  
  data_plot <- ggplot(data, aes(datetime_est, depth)) + 
    geom_contour_filled(aes(z = {{var}}), bins = 10, show.legend = F) + 
    annotate("rect", xmin = dump_start1, xmax = dump_end1, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = dump_start2, xmax = dump_end2, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    scale_x_datetime(expand = c(0,0)) + 
    scale_y_reverse(expand = c(0,0)) + 
    scale_fill_viridis_d(direction = -1) + 
    labs(x = "", y = "Depth (cm)", fill = fill_lab) + 
    theme(strip.background = element_rect(fill = "gray70"))
  
  legend_raw <- ggplot(data, aes(datetime_est, depth)) +
    geom_tile(aes(fill = {{var}})) +
    facet_wrap(~plot, ncol = 1) + 
    scale_fill_viridis_c(direction = -1) + 
    labs(fill = fill_lab)
  
  legend <- get_legend(legend_raw)
  
  plot_grid(data_plot, legend, rel_widths = c(1, legend_width))
}

make_contour_plot(firesting, do_percent_sat, 30, "DO (% Sat)", 0.15)
ggsave("figures/ess_pi/do_contours.png", width = 10, height = 6)

make_contour_plot(swap, eh_mv, 50, "Eh (mV)", 0.15)
ggsave("figures/ess_pi/redox_contours.png", width = 10, height = 6)


plot_grid(make_contour_plot(firesting, do_percent_sat, 30, "DO (% Sat)", 0.3), 
          make_contour_plot(swap, eh_mv, 50, "Eh (mV)", 0.3), 
          nrow = 1) 
ggsave("figures/ess_pi/do_redox_1row.png", width = 10, height = 6)


plot_grid(make_contour_plot(firesting, do_percent_sat, 30, "DO (% Sat)", 0.15), 
          make_contour_plot(swap, eh_mv, 50, "Eh (mV)", 0.15), 
          ncol = 1) 
ggsave("figures/ess_pi/do_redox_1col.png", width = 10, height = 8.5)


# 6. Make plot for DO time-series for methodsy section -------------------------

ggplot(firesting, aes(datetime_est, do_percent_sat, linetype = as.factor(depth), color = plot)) + 
  geom_line(show.legend = F) + 
  scale_color_manual(values = c("gray", "orange", "blue")) + 
  labs(x = "", y = "DO (% air sat.)")
ggsave("figures/ess_pi/do_timeseries.png", width = 4, height = 3)




