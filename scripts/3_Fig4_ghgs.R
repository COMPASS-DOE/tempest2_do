## Script to import and plot GHG datasets
## Note that files are potentially preliminary from Nick (Teams chat) and 
## should check with Kaizad/etc if these are being worked up in a final format
##
## 2024-06-03
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")


# 2. Soil CO2 fluxes -----------------------------------------------------------

ghg_path = "data/raw_data/ghgs/"

soil_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_soil_nickworking.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(condition = fct_relevel(condition, "Preflood"))

# soil_ghg %>% 
#   mutate(datetime = parsedate::parse_date(paste(date, time))) %>% 
#   ggplot(aes(datetime, co2_umol_m2_s, group = collar)) + 
#   geom_point() + 
#   facet_wrap(~plot, ncol = 1)

# tree_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>% 
#   clean_names() %>% 
#   filter(!is.na(timepoint)) %>% 
#   mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
#   mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
#   mutate(condition = fct_relevel(condition, "Preflood"))

# comparisons = list(c("pre-flood", "flood"), 
#                    c("pre-flood", "post-flood"), 
#                    c("flood", "post-flood"))

p_fluxes <- ggplot(soil_ghg, aes(condition, co2_umol_m2_min)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Soil CO2 flux (umol/m2/min)") + 
  scale_fill_viridis_d() 


# 3. Soil CO2 concentrations ---------------------------------------------------

sgw <- read_csv("data/240909_soil_ghg_concentrations.csv") %>% 
  mutate(datetime_est = force_tz(datetime_est, tz = common_tz)) %>% 
  mutate(condition = case_when(datetime_est < dump_start1 ~ "0_preflood", 
                               datetime_est > dump_end2 ~ "2_postflood", 
                            TRUE ~ "1_flood")) 

## Double-check things are assigned correctly
ggplot(sgw, aes(datetime_est, co2_conc_ppm_dilcorr, color = condition)) + 
  geom_point()

p_conc <- ggplot(sgw, aes(condition, co2_conc_ppm_dilcorr)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~sample_plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Soil CO2 concentration (ppm)") + 
  scale_fill_viridis_d() 

plot_grid(p_fluxes, p_conc, align = "hv", ncol = 1, labels = c("A", "B"))
ggsave("figures/4_soil_ghgs.png", width = 8, height = 7)
ggsave("figures/4_soil_ghgs.pdf", width = 8, height = 7)

## Add in stats for results section
soil_ghg %>% 
  ungroup() %>% 
  group_by(plot, condition) %>% 
  summarize(mean_co2 = mean(co2_umol_m2_min, na.rm = T), 
            median_co2 = median(co2_umol_m2_min, na.rm = T))

## Means (relative to preflood)
(415-164)/415 ##SW
(494-238)/494 ##FW

## Medians (relative to preflood)
(411-122)/411 ##SW
(379-195)/379 ##FW

## Median change in control
(836-493)/493

p1 <-ggplot(soil_ghg, aes(condition, ch4_nmol_m2_min)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Soil CH4 flux (nmol/m2/min)") + 
  scale_fill_viridis_d()

p2 <- ggplot(tree_ghg, aes(condition, flux_co2_umol_m2_min)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Tree CO2 flux (umol/m2/min)") + 
  scale_fill_viridis_d()

p3 <- ggplot(tree_ghg, aes(condition, flux_ch4_nmol_m2_min)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Tree CH4 flux (nmol/m2/min)") + 
  scale_fill_viridis_d()

plot_grid(p1, p2, p3, ncol = 1)
ggsave("figures/se_ghgs.png", width = 7, height = 10)





