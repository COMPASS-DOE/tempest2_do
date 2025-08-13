## This scripts updates the previous script which had separate Figure 1-3 for
## VWC, DO, and redox. Reorganizing this so VWC and conductivity are Figure 1, 
## and DO/redox are Figure 2.
##
## 2025-03-03 (Updated 2025-05-07)
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
    mutate(plot = as.factor(case_when(plot == "Seawater" ~ "Saltwater", 
                            plot == "Estuarine" ~ "Saltwater", 
                            TRUE ~ plot))) %>% 
    mutate(plot = fct_relevel(plot, "Control", "Freshwater"))
}

## Load processed datasets. Note that we should only use datetime_est
teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm)


# 2. Plot function for contour plots with continuous legend --------------------

## Plot constants we might wanna change
contour_width = 7
contour_height = 7

## Let's also standardize the datetime range across all parameters to DO
start = min(firesting$datetime_est)
end = max(firesting$datetime_est)

make_contour_plot <- function(data, var, max_depth, x_lab, y_lab, 
                              fill_lab, fill_direction, startpoint){
  
  viridis_endpoint = 0.8
  
  x <- data %>% 
    filter(datetime_est >= start & 
             datetime_est <= end)
  
  data_plot <- ggplot(x, aes(datetime_est, depth)) + 
    geom_contour_filled(aes(z = {{var}}), bins = 10, show.legend = F) + 
    annotate("rect", xmin = dump_start1, xmax = dump_end1, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = dump_start2, xmax = dump_end2, 
             ymin = 5, ymax = max_depth, fill = "white", alpha = 0.2) +
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    scale_x_datetime(expand = c(0,0)) + 
    # scale_x_datetime(
    #   # Create breaks every 2 days, starting from second day
    #   breaks = function(x) {
    #     date_range <- as.Date(range(x))
    #     second_day <- as.POSIXct(date_range[1] + 1)
    #     seq(from = second_day, to = max(x)-days(0), by = "2 day")  # Change "2 day" to adjust frequency
    #   },
    #   labels = scales::date_format("%b %d"),
    #   expand = c(0, 0)
    # ) + 
    scale_y_reverse(expand = c(0,0)) + 
    scale_fill_viridis_d(option = "mako", 
                         direction = fill_direction, 
                         begin = startpoint,
                         end = viridis_endpoint) + 
    labs(x = x_lab, y = y_lab, fill = fill_lab) + 
    theme(strip.background = element_rect(fill = "gray90"), 
          axis.text = element_text(size = 14),    # Adjust size of axis labels
          axis.title = element_text(size = 16),  
          strip.text = element_text(size = 14)) +  # Remove background from legend
    theme(panel.background = element_blank(), 
          plot.background = element_blank(), 
          legend.position = "bottom")
  
  legend_raw <- ggplot(x, aes(datetime_est, depth)) +
    geom_tile(aes(fill = {{var}})) +
    facet_wrap(~plot, ncol = 1) +
    scale_fill_viridis_c(option = "mako",
                         direction = fill_direction,
                         begin = startpoint,
                         end = viridis_endpoint) +
    labs(fill = fill_lab) +
    theme(legend.background = element_blank(),
          legend.position="bottom")
  
  legend <- get_legend(legend_raw)

  plot_grid(data_plot, legend, ncol = 1, rel_heights = c(1, 0.1))
}



# 3. Create plots --------------------------------------------------------------

vwc_plot <- make_contour_plot(teros, vwc, 30, "", "Soil depth (cm)", 
                              bquote("VWC (m"^{3}/m^{3}*")"), -1, 0)

ec_plot <- make_contour_plot(teros, ec, 30, "", "", 
                              bquote("EC (µS/cm)"), -1, 0)

do_plot <- make_contour_plot(firesting, do_percent_sat, 30, "", "Soil depth (cm)", 
                             "DO (% Sat)", 1, 0)

do_plot_no_label <- make_contour_plot(firesting, do_percent_sat, 30, "", "", 
                             "DO (% Sat)", 1, 0)

redox_plot <- make_contour_plot(swap, eh_mv, 50,  "", "", 
                                "Eh (mV)", 1, 0)


# 4. Save plots ----------------------------------------------------------------

save_plot <- function(plot_to_save, name_string){
  
  ## Set dimensions for plot
  plot_width = 12
  plot_height = 6.5
  
  plot_to_save
  
  ggsave(paste0("figures/", name_string, "_contour.png"), 
         width = plot_width, height = plot_height)
  ggsave(paste0("figures/", name_string, "_contour.pdf"), 
         width = plot_width, height = plot_height)
}


### Remaking as a single plot for PNAS
fig1ab <- plot_grid(vwc_plot, ec_plot, 
          nrow = 1, 
          labels = c("A", "B"))

fig1cd <- plot_grid(do_plot_no_label, redox_plot, 
                  nrow = 1, 
          labels = c("C", "D"))

plot_grid(fig1ab, fig1cd, nrow = 1)
ggsave("figures/1_Fig1.png", 
       width = 15, height = 7)

ggsave("figures/1_Fig1.pdf", 
       width = 15, height = 7, dpi = 400)

## Calculate stats for paper
## First, I'd like to know if post-flood values return to pre-flood values. I'll
## define this as the 24 hours before flooding, and the last 24 hours, since 
## all soil datasets are now standardized to the same time-window

compare_pre_post_flood_values <- function(data, par){
  
  x <- data %>% 
    filter(datetime_est >= start & 
             datetime_est <= end)
  
  x %>% 
    dplyr::select(datetime_est, plot, {{par}}) %>% 
    mutate(period = case_when(datetime_est <= start + days(1) ~ "0_Preflood", 
                              datetime_est >= end - days(1) ~ "1_Postflood", 
                              TRUE ~ "Other")) %>% 
    filter(period != "Other") %>% 
    ungroup() %>% 
    group_by(plot, period) %>% 
    summarize(mean = mean({{par}}, na.rm = TRUE),
      min = min({{par}}, na.rm = TRUE),
      max = max({{par}}, na.rm = TRUE))
}

compare_pre_post_flood_values(teros, vwc)
compare_pre_post_flood_values(teros, ec)


compare_pre_post_flood_by_depth <- function(data, par){
  
  x <- data %>% 
    filter(datetime_est >= start & 
             datetime_est <= end)
  
  x %>% 
    dplyr::select(datetime_est, plot, depth, {{par}}) %>% 
    mutate(period = case_when(datetime_est <= start + days(1) ~ "0_Preflood", 
                              datetime_est >= end - days(1) ~ "1_Postflood", 
                              TRUE ~ "Other")) %>% 
    filter(period != "Other") %>% 
    ungroup() %>% 
    group_by(plot, depth, period) %>% 
    summarize(mean = mean({{par}}, na.rm = TRUE),
              min = min({{par}}, na.rm = TRUE),
              max = max({{par}}, na.rm = TRUE))
}

compare_pre_post_flood_by_depth(teros, vwc)
compare_pre_post_flood_by_depth(teros, ec)

compare_pre_post_flood_by_depth(firesting, do_percent_sat)

x <- compare_pre_post_flood_by_depth(swap, eh_mv)


## Now, I'd like to quantify the length of hypoxia by plot/depth
firesting_hypoxia <- firesting %>%
  filter(do_percent_sat < 21) 

firesting_anoxia <- firesting %>%
  filter(do_percent_sat < 1) 


firesting_hypoxia %>% 
  ggplot(aes(datetime_est, as.factor(depth), color = "do_percent_sat")) + 
  geom_point() + 
  facet_wrap(~plot, nrow = 1)
  
firesting_hypoxia %>% 
  group_by(depth, plot) %>% 
  summarize(min_datetime = min(datetime_est), 
            max_datetime = max(datetime_est)) %>% 
  mutate(max_datetime - min_datetime)


firesting_anoxia %>% 
  group_by(depth, plot) %>% 
  summarize(min_datetime = min(datetime_est), 
            max_datetime = max(datetime_est)) %>% 
  mutate(max_datetime - min_datetime)

240300 / (3600 * 24)


## Calculate rates of VWC increase during first flood by plot/depth

teros_flood1 <- teros %>% filter(datetime >= flood1[1] & 
                                  datetime <= flood1[1] + hours(1)) %>% 
  mutate(flood = 1)

teros_flood2 <- teros %>% filter(datetime >= flood2[1] & 
                                   datetime <= flood2[1] + hours(1)) %>% 
  mutate(flood = 2)

teros_rates = bind_rows(teros_flood1, teros_flood2)

ggplot(teros_flood2, aes(datetime_est, vwc, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

teros_flood1 %>% 
  group_by(plot, depth) %>% 
  summarize(start = min(vwc, na.rm = T), 
            end = max(vwc, na.rm = T)) %>% 
  mutate(delta = ((end - start)/start)*100) %>% 
  ungroup() %>% 
  group_by(plot) %>% 
  summarize(mean(delta))

