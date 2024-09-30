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
sapflow <- read_csv("data/240806_sapflow_worked_up.csv") %>% 
  mutate(period_fct = factor(case_when(period == "0_preflood" ~ "Preflood", 
                                period == "2_postflood" ~ "Postflood"), 
                                levels = c("Preflood", "Postflood")))

## Read in tree GHG fluxes
tree_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(period_fct = factor(condition, levels = c("Preflood", "Flooded", "Postflood")))

## Read in vegetation metrics
## ci = intercellular CO2 (ppm)
## gs = stomatal conductance (mol m-2 s-1)
## A (clean_names = a) = photosynthesis rate (umol m-2 s-1)
veg <- read_csv("data/raw_data/vegetation/Compiled data_TEMPEST veg 2023.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(date))) %>% 
  mutate(plot = case_when(plot == "Fresh" ~ "Freshwater", 
                          plot == "Salt" ~ "Estuarine", 
                          TRUE ~ plot))


# 3. Make plots ----------------------------------------------------------------

# Define the color and alpha scales
colors <- c("blue", "red", "forestgreen")
alphas <- c(0.3, 0.6, 0.9)

p_sapflow <- sapflow %>% 
  filter(plot != "Control" & 
           period != "1_flood") %>% 
  ggplot(aes(period_fct, fd_n2)) + 
  geom_boxplot(aes(fill = plot, alpha = period_fct), 
                   outlier.alpha = 0, show.legend = F) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(values = colors[2:3]) +
  scale_fill_manual(values = colors[2:3]) +
  scale_alpha_manual(values = alphas[c(1,3)]) +
  geom_tukey(where = "whisker", vjust = -2, hjust = -1) +
  #stat_compare_means(label = "p.signif") + 
  labs(x = "", y = "Sapflux density (?)") #+ 
  # theme(legend.position = "top",
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 12),
  #   legend.box = "horizontal",
  #   strip.text = element_text(size = 14, face = "bold"))
  # guides(
  #   #fill = guide_legend(override.aes = list(alpha = alphas[c(1,3)])),
  #   fill = FALSE,
  #   color = FALSE,
  #   alpha = guide_legend(override.aes = list(fill = "red", color = NA)))  # Assuming you want the same color for all alphas in the legend


# sapflow %>% 
#   filter(plot != "Control" & 
#            period != "1_flood") %>% 
#   mutate(plot_period = paste0(plot, "_", period)) %>% 
#   ggplot(aes(plot_period, fd_n2, group = plot_period)) + 
#   geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75)) + 
#   geom_tukey(where = "whisker")



p_tree_ghg <- ggplot(tree_ghg, aes(period_fct, flux_co2_umol_m2_min)) + 
  geom_boxplot(aes(fill = plot, alpha = period_fct), show.legend = F, outlier.alpha = 0) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker", vjust = -1.5, hjust = -0.7) + 
  labs(x = "", y = "Tree CO2 flux (umol/m2/min)") + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_alpha_manual(values = alphas) +
  guides(
    #fill = guide_legend(override.aes = list(alpha = alphas[c(1,3)])),
    fill = FALSE,
    color = FALSE,
    alpha = guide_legend(override.aes = list(fill = "red", color = NA)))  # Assuming you want the same color for all alphas in the legend


top_row <- plot_grid(p_sapflow, p_tree_ghg, 
                     nrow = 1, 
                     rel_widths = c(0.8, 1), 
                     labels = c("A", "B"))

plot_veg <- function(var, y_label){
  ggplot(veg, aes(plot, {{var}})) + 
    geom_boxplot(aes(fill = plot), show.legend = F, 
                 outlier.alpha = 0, alpha = alphas[3]) + 
    geom_jitter(alpha = 0.5, width = 0.1) + 
    geom_tukey(where = "whisker", vjust = -5, hjust = -2) + 
    labs(x = "", y = y_label) + 
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
}

bottom_row <- plot_grid(plot_veg(ci, "Intercellular CO2 (ppm)"), 
          plot_veg(a, "Photosynthesis rate (umol/m2/min)"), 
          plot_veg(gs, "Stomatal conductance (mol/m2/min)"), 
          nrow = 1, 
          labels = c("C", "D", "E"))


plot_grid(top_row, bottom_row, ncol = 1)
ggsave("figures/5_Fig5_vegetation_responses.png", width = 10, height = 8)
ggsave("figures/5_Fig5_vegetation_responses.pdf", width = 10, height = 8)

