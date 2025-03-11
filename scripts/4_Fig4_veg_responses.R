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


# 2. Short-term sapflow plot ---------------------------------------------------

## Read in raw sapflow
sapflow_n2 <- read_csv("data/241125_sapflow_by_spp.csv") %>% 
  mutate(period_fct = factor(case_when(period == "0_preflood" ~ "Preflood", 
                                period == "2_postflood" ~ "Postflood"), 
                                levels = c("Preflood", "Postflood")))

sapflow_n1 <- read_csv("data/250306_sapflow_n1_by_species.csv") %>% 
  filter(period != "1_flood") %>% 
  mutate(period_fct = factor(case_when(period == "0_preflood" ~ "Preflood", 
                                       period == "2_postflood" ~ "Postflood"), 
                             levels = c("Preflood", "Postflood")))

sapflow_plot <- ggplot(sapflow_n1, aes(period_fct, fd_n1)) + 
  geom_jitter(aes(color = period_fct), width = 0.2, size = 1.5, 
              alpha = 0.6, show.legend = F) + 
  geom_boxplot(aes(fill = period_fct), show.legend = F, outlier.shape = NA, 
               alpha = 0.7) + 
  facet_wrap(~plot, nrow = 1) +
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Fd") + 
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


# 3. Veg plots -----------------------------------------------------------------

anyas_colors = c("springgreen2", "violetred2", "cyan2")

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

plot_veg <- function(var, y_lab){
  ggplot(veg, aes(plot, {{var}})) + 
    geom_jitter(aes(color = plot), width = 0.2, size = 1.5, 
                alpha = 0.5, show.legend = F) + 
    geom_boxplot(aes(fill = plot), show.legend = F, outlier.shape = NA, 
                 alpha = 0.7) + 
    geom_tukey(where = "whisker") + 
    labs(x = "", y = y_lab) + 
    theme_linedraw(base_size = 14) +
    theme(panel.grid.major = element_line(color = "gray90"), 
      panel.grid.minor = element_line(color = "gray90"),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(face = "bold", color = "black"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)) + 
    scale_color_manual(values = anyas_colors) + 
    scale_fill_manual(values = anyas_colors) 
}

# plot_grid(sapflow_plot, 
#           plot_veg(ci, bquote("Intercellular CO"[2]*" (ppm)")), 
#           plot_veg(a, bquote("Photosynthesis rate (µmol/m"^{2}*"/min)")), 
#           plot_veg(gs, bquote("Stomatal conductance (µol/m"^{2}*"/min)")), 
#           ncol = 1, 
#           labels = c("A", "B", "C", "D"), 
#           align = "hv")

#ggsave("figures/4_veg.png", width = 3.5, height = 15)
#ggsave("figures/4_veg.pdf", width = 3.5, height = 15)


plot_grid(sapflow_plot,
          NULL,
          plot_veg(ci, bquote("Intercellular CO"[2]*" (ppm)")),
          plot_veg(a, bquote("Photosynthesis rate (µmol/m"^{2}*"/min)")),
          NULL,
          plot_veg(gs, bquote("Stomatal conductance (µol/m"^{2}*"/min)")),
          nrow = 2,
          rel_widths = c(1, 0.1, 1,
                         1, 0.1, 1),
          labels = c("A", "", "B",
                     "C", "", "D"),
          align = "hv")
ggsave("figures/4_veg.png", width = 7, height = 8)
ggsave("figures/4_veg.pdf", width = 7, height = 8)


# 3. Long-term sapflow plot ----------------------------------------------------

sf_filtered <- read_csv("data/250310_2023_sapflow.csv")




