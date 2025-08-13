## This script is a revised, simplified verion that makes Figure 3 for long-term
## sapflow. 
##
## Peter Regier
## 2025-08-13
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

rm(list = ls())

## Load setup script
source("scripts/0_0_setup.R")
p_load(ggallin)

anyas_colors = c("springgreen2", "cyan2", "violetred2")


# 2. First plot ----------------------------------------------------------------

## You'll need this for when your flooding occurred
flood_doy <- lubridate::yday(dump_start1)

df_trim <- sf_per_hour %>% 
  filter(doy > 100 & doy < 300)

p1 <- ggplot(df_trim, aes(x = doy, color = plot)) + 
  geom_point(aes(y = f_avg_hr), alpha = 0.4) + 
  geom_line(aes(y = f_roll_hr)) + 
  geom_vline(xintercept = flood_doy, linetype = "dashed") + 
  facet_wrap(~species, ncol = 1) + 
  scale_color_manual(values = anyas_colors) + 
  labs(x = "Day of Year", y = expression("Sap flux (cm"^3*"/hr)"), color = "") + 
  theme(legend.position = c(0.78, 0.6), 
        legend.background = element_blank(), 
        legend.key = element_rect(fill = NA, color = NA)) 

# 3. Normalize data -----------------------------------------------------------

df_trim2 <- df_trim %>% 
  group_by(plot, species) %>% 
  mutate(pre_mean = mean(f_avg_hr[doy < flood_doy])) %>% 
  mutate(f_avg_n = f_avg_hr - pre_mean)

calculate_deltas <- function(selected_spp){
  
  ## Set constants
  roll_length = 10
  
  df_trim2 %>% 
    filter(species == selected_spp) %>% 
    dplyr::select(plot, doy, f_avg_n) %>% 
    pivot_wider(names_from = "plot", values_from = "f_avg_n") %>% 
    mutate(delta_sw = Saltwater - Control, 
           delta_fw = Freshwater - Control) %>% 
    mutate(delta_sw_roll = zoo::rollmean(delta_sw, roll_length, fill = NA), 
           delta_fw_roll = zoo::rollmean(delta_fw, roll_length, fill = NA)) %>% 
    mutate(species = selected_spp)
}

deltas <- bind_rows(calculate_deltas("Tulip Poplar"), 
                    calculate_deltas("Beech"), 
                    calculate_deltas("Red Maple"))

ggplot(deltas, aes(x = doy)) + 
  geom_point(aes(y = delta_fw), alpha = 0.4, color = anyas_colors[2]) + 
  geom_point(aes(y = delta_sw), alpha = 0.4, color = anyas_colors[3]) + 
  geom_line(aes(y = delta_fw_roll), color = anyas_colors[2]) + 
  geom_line(aes(y = delta_sw_roll), color = anyas_colors[3]) + 
  geom_smooth(data = deltas %>% filter(doy > flood_doy), 
              aes(y = delta_fw), 
              method = "lm", se = F, 
              alpha = 0.6, color = anyas_colors[2]) + 
  geom_smooth(data = deltas %>% filter(doy > flood_doy), 
              aes(y = delta_sw), 
              method = "lm", se = F,
              alpha = 0.6, color = anyas_colors[3]) + 
  geom_vline(xintercept = flood_doy, linetype = "dashed") + 
  facet_wrap(~species, ncol = 1) + 
  labs(x = "Day of Year", y = "Normalized sap flux", color = "") + 
  theme(legend.position = c(0.8, 0.6), 
        legend.background = element_blank(), 
        legend.key = element_rect(fill = NA, color = NA)) 
