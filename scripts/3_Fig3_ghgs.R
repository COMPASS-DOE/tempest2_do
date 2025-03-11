## This scripts updates the previous script to create a new, cleaner version of
## the GHG datasets
##
## 2025-03-04
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")

ghg_path = "data/raw_data/ghgs/"


# 2. Read in soil and tree fluxes and concentrations ---------------------------

soil_ghg_flux <- read_csv(paste0(ghg_path, "ghg_fluxes_soil_nickworking.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(condition = fct_relevel(condition, "Preflood")) %>% 
  mutate(condition = case_when(condition == "Preflood" ~ "0_preflood", 
                                condition == "Flooded" ~ "1_flood",
                                condition == "Postflood" ~ "2_postflood"))

soil_ghg_conc <- read_csv("data/240909_soil_ghg_concentrations.csv") %>% 
  mutate(datetime_est = force_tz(datetime_est, tz = common_tz)) %>% 
  mutate(condition = case_when(datetime_est < dump_start1 ~ "0_preflood", 
                               datetime_est > dump_end2 ~ "2_postflood", 
                               TRUE ~ "1_flood")) %>% 
  mutate(sample_plot = ifelse(sample_plot == "Seawater", "Estuarine", sample_plot))


tree_ghg_flux <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>%
  clean_names() %>%
  filter(!is.na(timepoint)) %>%
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>%
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>%
  mutate(condition = fct_relevel(condition, "Preflood")) %>% 
  mutate(condition = case_when(condition == "Preflood" ~ "0_preflood", 
                               condition == "Flooded" ~ "1_flood",
                               condition == "Postflood" ~ "2_postflood"))

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

ghgs <- bind_rows(soil_ghg_flux %>% 
  rename("co2" = co2_umol_m2_s) %>% 
  dplyr::select(plot, condition, co2) %>% 
  mutate(type = "soil", 
         measurement = "flux", 
         unit = "umol_m2_s"), 
  soil_ghg_conc %>% 
  rename("plot" = sample_plot,
         "co2" = co2_conc_ppm_dilcorr) %>% 
  dplyr::select(plot, condition, co2) %>% 
  mutate(type = "soil", 
         measurement = "conc", 
         unit = "ppm"), 
  tree_ghg_flux %>% 
  mutate("co2" = flux_co2_umol_m2_min * 60) %>% 
  dplyr::select(plot, condition, co2) %>% 
  mutate(type = "tree", 
         measurement = "flux", 
         unit = "umol_m2_s"), 
  tree_ghg_conc %>% 
  rename("plot" = sample_plot,
         "co2" = co2_conc_ppm_dilcorr) %>% 
  dplyr::select(plot, condition, co2) %>% 
  mutate(type = "tree", 
         measurement = "conc", 
         unit = "ppm"))





ggplot(ghgs, aes(plot, co2, fill = condition, color = condition)) + 
  #geom_boxplot(alpha = 0.5) + 
  geom_violin(alpha = 0.4) +
  # geom_jitter(alpha = 0.5, 
  #            position = position_dodge(width = 1)) + 
  #facet_grid(type~measurement, scales = "free")
  facet_wrap(type~measurement, scales = "free") + 
  geom_tukey(where = "whisker")


ghgs %>% 
  filter(measurement == "flux" & type == "soil") %>% 
  ggplot(aes(condition, co2)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Soil CO2 flux (umol/m2/min)") + 
  scale_fill_viridis_d() 


make_boxplot <- function(selected_measurement, selected_type, plot_title, y_lab, vert_just){
  ghgs %>% 
    mutate(condition2 = as.factor(case_when(condition == "0_preflood" ~ "Preflood", 
                                  condition == "1_flood" ~ "Flood", 
                                  condition == "2_postflood" ~ "Postflood"))) %>% 
    mutate(condition2 = fct_relevel(condition2, "Preflood")) %>% 
    filter(measurement == selected_measurement & 
             type == selected_type) %>% 
    ggplot(aes(condition2, co2)) + 
    geom_boxplot(aes(fill = condition2), show.legend = F, outlier.shape = NA, 
                 alpha = 0.7) + 
    geom_jitter(aes(color = condition2), width = 0.2, size = 1.5, 
                alpha = 0.6, show.legend = F) + 
    facet_wrap(~plot, nrow = 1) +
    geom_tukey(where = "whisker", vjust = vert_just) + 
    labs(x = "", y = y_lab, title = plot_title) + 
    theme_linedraw(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "gray90"), 
      panel.grid.minor = element_line(color = "gray90"),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(face = "bold", color = "black"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)
    ) + 
    scale_fill_viridis_d(option = "mako") + 
    scale_color_viridis_d(option = "mako", end = 0.8)
}


soil_plots = plot_grid(make_boxplot("flux", "soil", 
                                    "Soils", expression(paste("C", O[2], " Flux (", mu, "mol ", m^2, "/min)")), -5) + 
                         theme(axis.text.x=element_blank()), 
                       make_boxplot("conc", "soil", "", expression(paste("C", O[2], " Concentration (ppm)")), -1), 
                       ncol = 1, labels = c("A", "C"), 
                       align = "hv")

tree_plots = plot_grid(make_boxplot("flux", "tree", 
                                    "Trees", "", -2) + 
                         theme(axis.text.x=element_blank()), 
                       make_boxplot("conc", "tree", "", "", -2), 
                       ncol = 1, labels = c("B", "D"), 
                       align = "hv")

plot_grid(soil_plots, tree_plots, nrow = 1)
ggsave("figures/3_ghgs.png", width = 12, height = 10)
ggsave("figures/3_ghgs.pdf", width = 12, height = 10)

