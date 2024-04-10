## This script creates Figure 2 (volumetric water content), Figure 3 (DO), and
## Figure 4 (redox) for the main text. Since they're all in the same format, 
## I'll make them all using a single function to keep things consistent.
##
## 2024-04-05
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")

## Make a small helper script to format each dataset when reading in

prep_csvs <- function(path){
  read_csv(path) %>% 
    mutate(datetime_est = force_tz(datetime_est, tzon = common_tz)) %>% 
    mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                            TRUE ~ plot))
}

## Load processed datasets. Note that we should only use datetime_est
teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm)


# 2. Plot function for contour plots with continuous legend --------------------

## Plot constants we might wanna change
contour_width = 10
contour_height = 6

make_contour_plot <- function(data, var, max_depth, x_lab, y_lab, fill_lab, legend_width, fill_direction){
  
  data_plot <- ggplot(data, aes(datetime_est, depth)) + 
    geom_contour_filled(aes(z = {{var}}), bins = 10, show.legend = F) + 
    annotate("rect", xmin = dump_start1, xmax = dump_end1, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = dump_start2, xmax = dump_end2, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    scale_x_datetime(expand = c(0,0)) + 
    scale_y_reverse(expand = c(0,0)) + 
    scale_fill_viridis_d(direction = fill_direction) + 
    labs(x = x_lab, y = y_lab, fill = fill_lab) + 
    theme(strip.background = element_rect(fill = "gray90"), 
          axis.text = element_text(size = 14),    # Adjust size of axis labels
          axis.title = element_text(size = 16),  
          strip.text = element_text(size = 14)) +  # Remove background from legend
    theme(panel.background = element_blank(), 
          plot.background = element_blank())
  
  legend_raw <- ggplot(data, aes(datetime_est, depth)) +
    geom_tile(aes(fill = {{var}})) +
    facet_wrap(~plot, ncol = 1) + 
    scale_fill_viridis_c(direction = -1) + 
    labs(fill = fill_lab) + 
    theme(legend.background = element_blank())
  
  legend <- get_legend(legend_raw)
  
  plot_grid(data_plot, legend, rel_widths = c(1, legend_width))
}


# 3. Create plots --------------------------------------------------------------

vwc_plot <- make_contour_plot(teros, vwc, 30, "", "Soil depth (cm)", 
                              bquote("VWC (m"^{3}/m^{3}*")"), 0.15, 1)

do_plot <- make_contour_plot(firesting, do_percent_sat, 30, "", "Soil depth (cm)", 
                  "DO (% Sat)", 0.15, -1)

redox_plot <- make_contour_plot(swap, eh_mv, 50,  "Datetime", "Soil depth (cm)", 
                                "Eh (mV)", 0.15, -1)


# 4. Save plots ----------------------------------------------------------------

save_plot <- function(plot_to_save, name_string){
  
  ## Set dimensions for plot
  plot_width = 10
  plot_height = 6
  
  plot_to_save
  
  ggsave(paste0("figures/", name_string, "_contour.png"), 
         width = plot_width, height = plot_height)
  ggsave(paste0("figures/", name_string, "_contour.pdf"), 
         width = plot_width, height = plot_height)
}


vwc_plot
save_plot(vwc_plot, "2_vwc")

do_plot
save_plot(do_plot, "3_do")

redox_plot
save_plot(redox_plot, "4_redox")




