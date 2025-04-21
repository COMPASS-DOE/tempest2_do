
source("scripts/0_0_setup.R")

df_raw <- read_csv("data/250310_2023_sapflow.csv")

df_raw %>% 
  filter(species == "Tulip Poplar", plot == "Saltwater") %>% 
  ggplot(aes(doy, f_avg)) + 
  geom_point() + 
  geom_smooth()

df <- df_raw %>% 
  mutate(pre = ifelse(doy < ))


flood_doy <- lubridate::yday(dump_start1)
roll_length = 10

calculate_deltas <- function(selected_spp){
  df %>% 
    filter(species == selected_spp) %>% 
    dplyr::select(plot, doy, f_avg) %>% 
    pivot_wider(names_from = "plot", values_from = "f_avg") %>% 
    mutate(period = ifelse(doy < flood_doy, "1_preflood", "post-flood"), 
           days_since_flood = doy - flood_doy) %>% 
    mutate(delta_sw = Saltwater - Control, 
           delta_fw = Freshwater - Control) %>% 
    mutate(delta_sw_roll = zoo::rollmean(delta_sw, roll_length, fill = NA), 
           delta_fw_roll = zoo::rollmean(delta_fw, roll_length, fill = NA)) %>% 
    mutate(species = selected_spp)
}

x <- df %>% 
  filter(species == "Tulip Poplar") %>% 
  dplyr::select(plot, doy, f_avg) %>% 
  pivot_wider(names_from = "plot", values_from = "f_avg") %>% 
  mutate(period = ifelse(doy < flood_doy, "1_preflood", "post-flood"), 
         days_since_flood = doy - flood_doy) %>% 
  mutate(delta_sw = (Saltwater - Control) / Control, 
         delta_fw = (Freshwater - Control) / Control) %>% 
  mutate(delta_sw_roll = zoo::rollmean(delta_sw, roll_length, fill = NA), 
         delta_fw_roll = zoo::rollmean(delta_fw, roll_length, fill = NA))


plot_grid(ggplot(x, aes(doy)) + 
            geom_point(aes(y = delta_sw), alpha = 0.5) + 
            geom_line(aes(y = delta_sw_roll)) + 
            geom_vline(xintercept = flood_doy, linetype = "dashed"), 
          ggplot(x, aes(doy)) + 
            geom_point(aes(y = delta_fw), alpha = 0.5) + 
            geom_line(aes(y = delta_fw_roll)) + 
            geom_vline(xintercept = flood_doy, linetype = "dashed"), 
          ncol = 1)


## Do for all species


deltas <- bind_rows(calculate_deltas("Tulip Poplar"), 
                    calculate_deltas("Beech"), 
                    calculate_deltas("Red Maple"))



anyas_colors = c("springgreen2", "cyan2", "violetred2")

p1 <- ggplot(df, aes(x = doy, color = plot)) + 
  geom_point(aes(y = f_avg), alpha = 0.4) + 
  geom_line(aes(y = f_roll)) + 
  annotate(geom = "rect", xmin = 100, xmax = 300, ymin = 0, ymax = 1.6, 
           color = NA, fill = "gray", alpha = 0.2) + 
  geom_vline(xintercept = flood_doy, linetype = "dashed") + 
  facet_wrap(~species, ncol = 1) + 
  scale_color_manual(values = anyas_colors) + 
  labs(x = "Day of Year", y = "Sap flux (cm3/s)", color = "") + 
  theme(legend.position = c(0.8, 0.6), 
        legend.background = element_blank(), 
        legend.key = element_rect(fill = NA, color = NA)) 

p2 <- deltas %>% 
  select(species, doy, contains("delta")) %>% 
  ggplot(aes(x = doy)) + 
  annotate(geom = "rect", xmin = 100, xmax = 300, 
           ymin = min(deltas$delta_sw), 
           ymax = max(deltas$delta_fw), 
           color = NA, fill = "gray", alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(y = delta_sw), color = anyas_colors[3], alpha = 0.2) + 
  geom_line(aes(y = delta_sw_roll), color = anyas_colors[3]) + 
  geom_point(aes(y = delta_fw), color = anyas_colors[2], alpha = 0.2) + 
  geom_line(aes(y = delta_fw_roll), color = anyas_colors[2]) + 
  geom_vline(xintercept = flood_doy, linetype = "dashed") + 
  facet_wrap(~species, ncol = 1) + 
  labs(x = "Day of Year", y = "Sap flux (treatment - Control)", color = "Plot") + 
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_blank(), 
        legend.key = element_rect(fill = NA, color = NA))

plot_grid(p1, p2, nrow = 1)
ggsave("figures/250413_delta_sapflux.png", width = 9, height = 6)

## And now for something totally different

## Trees used for leaf measurements
veg <- read_csv("data/raw_data/vegetation/Compiled data_TEMPEST veg 2023.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(date))) %>% 
  mutate(plot = case_when(plot == "Fresh" ~ "Freshwater", 
                          plot == "Salt" ~ "Estuarine", 
                          TRUE ~ plot)) %>% 
  mutate(sapflux_id = paste0(plot_id, stem_id)) 

check_veg_spp <- read_csv("data/from_kendal/sapflow_inventory.csv") %>% 
  clean_names() %>% 
  filter(sapflux_id %in% unique(veg$sapflux_id))

unique(check_veg_spp$spp)


## Wait, where does that put us on GHGs? Were they more impacted by spp??

ghg_path = "data/raw_data/ghgs/"

tree_ghg_flux <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>%
  clean_names() %>%
  filter(!is.na(timepoint)) %>%
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>%
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>%
  mutate(condition = fct_relevel(condition, "Preflood")) %>% 
  mutate(condition = case_when(condition == "Preflood" ~ "0_preflood", 
                               condition == "Flooded" ~ "1_flood",
                               condition == "Postflood" ~ "2_postflood")) %>% 
  mutate("co2" = flux_co2_umol_m2_min * 60)

ggplot(tree_ghg_flux, aes(species, co2, fill = condition)) + 
         geom_boxplot() + 
         facet_wrap(~plot, ncol = 1)

tree_ghg_flux %>% 
  #filter(species == "Tulip Poplar") %>%
  filter(plot == "Estuarine") %>% 
  anova_test(co2 ~ condition + species)

aov(co2 ~ plot | species, data = tree_ghg_flux)




tree_ghg_conc <- read_csv("data/raw_data/swilson_ghg_concentrations/TEMPEST_TGW_2023_AllData.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(paste(sample_month, sample_day, sample_year)))) %>% 
  mutate(hours = sample_time %/% 100,
         minutes = sample_time %% 100,
         time_hms = sprintf("%02d:%02d:00", hours, minutes)) %>%
  select(-hours, -minutes) %>% 
  mutate(datetime_est = parsedate::parse_date(paste(date, time_hms)) - hours(1)) %>% 
  dplyr::select(datetime_est, date, time_hms, sample_plot, tree_id, 
                co2_conc_ppm_dilcorr, ch4_conc_ppm_dilcorr) %>% 
  mutate(condition = case_when(datetime_est < dump_start1 ~ "0_preflood", 
                               datetime_est > dump_end2 ~ "2_postflood", 
                               TRUE ~ "1_flood")) %>% 
  mutate(sample_plot = ifelse(sample_plot == "Seawater", "Estuarine", sample_plot))



