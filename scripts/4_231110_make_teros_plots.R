## This script brings in TEROS data for the TEMPEST 2 event. This is an updated
## version of 230607_teros_data_prep.R which is now archived. This script is 
## specifically designed to make all plots related to TEORS data, specifically
## Figure 2 (VWC time-series) but also a bunch of supplemental figures.
##
## Peter Regier
## 2023-11-10
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")


# 2. Read data -----------------------------------------------------------------

df <- read_csv("data/231102_teros_final.csv") %>% 
  filter(datetime >= pre_event_start & 
           datetime <= post_event_end) %>% 
  label_flood_periods()


# 3. Make time-series plots ----------------------------------------------------

make_teros_contour <- function(var, y_label){
  ggplot(df, aes(datetime, depth)) + 
    geom_contour_filled(aes(z = {{var}}), bins = 10) + 
    scale_y_reverse() + 
    annotate("rect", xmin = dump_start1, xmax = dump_start1 + hours(10), 
             ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = dump_start2, xmax = dump_start2 + hours(10), 
             ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    labs(x = "", y = "Depth (cm)", fill = y_label)
}

teros_width = 9
teros_height = 8

make_teros_contour(tsoil, "Temp (C)")
ggsave("figures/231110_teros_temp_contours.png", width = teros_width, height = teros_height) 

make_teros_contour(ec, "EC (uS/cm)")
ggsave("figures/231110_teros_ec_contours.png", width = teros_width, height = teros_height) 

make_teros_contour(vwc, "VWC (m3/m3)")
ggsave("figures/Fig2_teros_vwc_contours.png", width = teros_width, height = teros_height) 


# 4. Make boxplots -------------------------------------------------------------

require(ggtukey)

## Create a function, which relies heavily on ggtukey::geom_tukey() to do the heavy
## lifting described in https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots
make_tukey_boxplots <- function(data, var, y_label){
  
  period_labels = c("Pre-Flood", "Flood 1", "Flood 2", "Post-Flood")
  
  ggplot(df, aes(x = period, y = {{var}}, fill = period)) + 
    geom_boxplot(show.legend = F) + 
    facet_wrap(~plot) + 
    geom_tukey(where = "whisker") + 
    scale_x_discrete(labels = period_labels) + 
    scale_fill_viridis_d() + 
    labs(x = "", y = y_label) +
    theme(axis.text.x = element_text(angle=45, vjust=.5))
}

plot_grid(make_tukey_boxplots(teros, tsoil, "Temp (C)"),
          make_tukey_boxplots(teros, vwc, "VWC (m3/m3)"),
          make_tukey_boxplots(teros, ec, "EC (mS/cm)"), 
          ncol = 1)
ggsave("figures/231110_teros_tukey_boxplots.png", width = 8, height = 10)



