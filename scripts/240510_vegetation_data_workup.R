


# 1. Setup ---------------------------------------------------------------------

source("scripts/0_0_setup.R")

veg_path <- "data/raw_data/vegetation/"

tree_ghg_fluxes <- readxl::read_excel(paste0(veg_path, "ghg_fluxes_trees_processed.xlsx"), 
                    na = "NA") %>% 
  clean_names() %>% 
  mutate(timepoint_num = as.numeric(str_extract(timepoint, "\\d+"))) %>% 
  dplyr::select(plot, species, timepoint, timepoint_num, condition, 
                flux_co2_umol_m2_min, flux_ch4_nmol_m2_min) %>% 
  drop_na()


tree_ghg_fluxes %>% 
  mutate(condition = str_replace_all(condition, "-", "_")) %>%
  mutate(condition = fct_relevel(condition, "pre_flood")) %>% 
  #filter(grepl("Tulip", species)) %>% 
  #filter(plot == "Control") %>% 
  ggplot(aes(condition, flux_co2_umol_m2_min, fill = condition)) + 
  geom_boxplot(alpha = 0.5) + 
  geom_tukey(where = "whisker") + 
  facet_wrap(species~plot)
ggsave("figures/240510_flux_co2_umol_m2_min.png", width = 7, height = 7)


tree_ghg_fluxes %>% 
  mutate(condition = str_replace_all(condition, "-", "_")) %>%
  mutate(condition = fct_relevel(condition, "pre_flood")) %>% 
  #filter(grepl("Tulip", species)) %>% 
  #filter(plot == "Control") %>% 
  ggplot(aes(condition, flux_ch4_nmol_m2_min, fill = condition)) + 
  geom_boxplot(alpha = 0.5) + 
  geom_tukey(where = "whisker") + 
  facet_wrap(species~plot)
ggsave("figures/240510_flux_ch4_nmol_m2_min.png", width = 7, height = 7)


############




