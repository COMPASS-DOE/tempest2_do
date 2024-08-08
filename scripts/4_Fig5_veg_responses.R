## This is an updated version of several scripts, condensed into one in order to
## make a new version of Figure 5 that includes all vegetation metrics I'm planning
## to use. 
##
## Peter Regier
## 2024-08-06
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")


# 2. Read in datasets ----------------------------------------------------------

## Read in raw sapflow
sapflow <- read_csv("data/240806_sapflow_worked_up.csv")

## Read in tree GHG fluxes
tree_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(condition = fct_relevel(condition, "Preflood"))

## Read in vegetation metrics
## ci = intercellular CO2 (ppm)
## gs = stomatal conductance (mol m-2 s-1)
## A (clean_names = a) = photosynthesis rate (umol m-2 s-1)
veg <- read_csv("data/raw_data/vegetation/Compiled data_TEMPEST veg 2023.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(date)))


# 3. Make plots ----------------------------------------------------------------

p_sapflow <- sapflow %>% 
  filter(plot != "Control" & 
           period != "1_flood") %>% 
  ggplot(aes(plot, fd_n2, fill = period)) + 
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75)) + 
  geom_jitter(aes(color = period), position = position_dodge(width = 0.75), size = 2) +
  geom_hline(yintercept = 0) + 
  #geom_tukey(where = "whisker") + 
  #facet_grid(.~plot) + 
  stat_compare_means() + 
  #stat_compare_means(comparisons = list(c("0_preflood", "2_postflood")), ) + 
  labs(x = "Treatment plot", y = "Sapflux density (?)")

sapflow %>% 
  filter(plot != "Control" & 
           period != "1_flood") %>% 
  mutate(plot_period = paste0(plot, "_", period)) %>% 
  ggplot(aes(plot_period, fd_n2, group = plot_period)) + 
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75)) + 
  geom_tukey(where = "whisker")

p_tree_ghg <- ggplot(tree_ghg, aes(condition, flux_co2_umol_m2_min)) + 
  geom_boxplot(aes(fill = condition), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Tree CO2 flux (umol/m2/min)") + 
  scale_fill_viridis_d()

top_row <- plot_grid(p_sapflow, p_tree_ghg, 
                     nrow = 1, 
                     rel_widths = c(0.8, 1), 
                     labels = c("A", "B"))

plot_veg <- function(var, y_label){
  ggplot(veg_raw, aes(plot, {{var}})) + 
    geom_boxplot(aes(fill = plot), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
    geom_jitter(alpha = 0.5, width = 0.1) + 
    geom_tukey(where = "whisker") + 
    labs(x = "", y = y_label) + 
    scale_fill_viridis_d() 
}

bottom_row <- plot_grid(plot_veg(ci, "Intercellular CO2 (ppm)"), 
          plot_veg(a, "Photosynthesis rate (umol/m2/min)"), 
          plot_veg(gs, "Stomatal conductance (mol/m2/min)"), 
          nrow = 1, 
          labels = c("C", "D", "E"))


plot_grid(top_row, bottom_row, ncol = 1)
ggsave("figures/5_Fig5_vegetation_responses.png", width = 9, height = 8)
ggsave("figures/5_Fig5_vegetation_responses.pdf", width = 9, height = 8)

