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


# 2. Read in data --------------------------------------------------------------

ghg_path = "data/raw_data/ghgs/"

soil_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_soil_nickworking.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(condition = fct_relevel(condition, "Preflood"))

tree_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(condition = fct_relevel(condition, "Preflood"))

# comparisons = list(c("pre-flood", "flood"), 
#                    c("pre-flood", "post-flood"), 
#                    c("flood", "post-flood"))

ggplot(soil_ghg, aes(condition, co2_umol_m2_min)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Soil CO2 flux (umol/m2/min)") + 
  scale_fill_viridis_d() 
ggsave("figures/4_soil_ghgs.png", width = 7, height = 4)
ggsave("figures/4_soil_ghgs.pdf", width = 7, height = 4)

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

