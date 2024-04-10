## This script makes Figure 5, which is comparing disturbances across the treatment
## plots. I've tried a couple different approaches (and the TSLA paper has wrested
## with how to do this for ages), but I'm hoping that this script can be a single
## place to have the final decision, since I need to just pick something that makes
## sense and run with it
##
## 2024-04-09
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


# 2. Initial visualization of our datasets -------------------------------------

initial_plots <- function(data, var){
  ggplot(data, aes(datetime_est, {{var}}, color = as.factor(depth))) + 
    geom_line(show.legend = T) + 
    facet_wrap(~plot, ncol = 1, scales = "free")
}

plot_grid(initial_plots(teros, vwc), 
          initial_plots(firesting, do_percent_sat),
          initial_plots(swap, eh_mv),
          nrow = 1)

## Trying Patrick et al. (2020)'s stats: 
## https://doi.org/10.1007/s12237-019-00690-3
## ln(min/max / vpre baseline)

# 1. Identify vpre and disturbance time-periods
# 2. calculate values (decide if it's min or max first)
# 3. Plot VWC, and include control...
# 4. If it looks weird, try it with the detrended data

vwc_lrr <- teros %>% 
  mutate(time_period = case_when(datetime_est < as_datetime("2023-06-06 05:00:00", tz = common_tz) ~ "Vpre",
                                 datetime_est < as_datetime("2023-06-07 15:00:00", tz = common_tz) ~ "Event", 
                                 TRUE ~ NA)) %>% 
  ungroup() %>% 
  group_by(plot, depth) %>%
  summarize(
    vpre = mean(vwc[time_period == "Vpre"], na.rm = TRUE),
    event_min = min(vwc[time_period == "Event"], na.rm = TRUE),
    event_max = max(vwc[time_period == "Event"], na.rm = TRUE)) %>% 
  mutate(vdist = ifelse(abs(event_min) > abs(event_max), event_min, event_max), 
         lrr = log(vdist/vpre))


set.seed(123)  # for reproducibility

vwc_lrr %>%
  ggplot(aes(depth, lrr, fill = plot)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_col(position = "dodge", alpha = 0.5) + 
  coord_flip() + 
  scale_x_reverse() + 
  labs(x = "Depth (cm)", y = "Log response ratio")
           
           
calculate_lrr <- function(data, var) {
  # Convert var to string (in case they are passed as bare variable names)
  var <- as.character(substitute(var))
  
  x <- data %>%
    mutate(time_period = case_when(
      datetime_est < as_datetime("2023-06-06 05:00:00", tz = common_tz) ~ "Vpre",
      datetime_est < as_datetime("2023-06-07 15:00:00", tz = common_tz) ~ "Event", 
      TRUE ~ NA_character_)) 
  
  # ggplot(x, aes(datetime_est, .data[[var]], color = time_period)) + 
  #   geom_point() + 
  #   facet_wrap(~plot, ncol = 1)

  dodge_width = 5
  
  x %>%
    ungroup() %>%
    group_by(plot, depth) %>%
    summarize(vpre = mean(.data[[var]][time_period == "Vpre"], na.rm = TRUE),
              event_min = min(.data[[var]][time_period == "Event"], na.rm = TRUE),
              event_max = max(.data[[var]][time_period == "Event"], na.rm = TRUE)) %>%
    mutate(vdist = ifelse(abs(event_min - vpre) > abs(event_max - vpre), event_min, event_max),
           lrr = log(vdist/vpre)) %>%
    ungroup() %>% 
    complete(plot, depth = c(5, 10, 15, 20, 30, 50), fill = list(vpre = NA, event_min = NA, event_max = NA, vdist = NA, lrr = NA)) %>% 
    # mutate(plot_num = as.numeric(as.factor(plot))) %>%
    # ggplot(aes(depth + dodge_width * (plot_num - 1.5), lrr, color = plot)) +
    # geom_hline(yintercept = 0, linetype = "dashed") +
    # geom_segment(aes(xend = depth + dodge_width * (plot_num - 1.5), yend = 0)) +
    # geom_point(size = 3) +
    # coord_flip() +
    # scale_x_continuous(breaks = unique(x$depth)) +
    # labs(x = "Depth (cm)", y = "Log response ratio")
    ggplot(aes(depth, lrr, color = plot)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    #geom_col(position = "dodge", alpha = 0.5) +
    geom_linerange(aes(ymin = 0, ymax = lrr), position = position_dodge(width = dodge_width)) +
    geom_point(position = position_dodge(width = dodge_width), size = 3) +
    coord_flip() +
    scale_x_reverse() +
    labs(x = "Depth (cm)", y = "Log response ratio", color = "")
}

plot_grid(calculate_lrr(teros, vwc) + theme(legend.position = "none") + 
            ggtitle("VWC (m3/m3)"), 
          calculate_lrr(firesting %>% 
                          mutate(do_percent_sat = ifelse(do_percent_sat == 0, 0.01, do_percent_sat)), 
                        do_percent_sat) + theme(legend.position = "none") + 
            ggtitle("DO (mg/L)"), 
          calculate_lrr(swap, eh_mv) + theme(legend.position = "none") + 
            ggtitle("Eh (mV)"), 
          get_legend(calculate_lrr(swap, eh_mv)), 
          nrow = 1, rel_widths = c(1, 1, 1, 0.4))
ggsave("figures/5_Fig5_resistance.png", width = 10, height = 4)

calculate_lrr(teros, vwc)


lrr_vwc <- calculate_lrr(teros, vwc)
lrr_ec <- calculate_lrr(teros, ec)
lrr_do <- calculate_lrr(firesting, do_percent_sat)

calculate_lrr(firesting, do_percent_sat)

get_legend(calculate_lrr(swap, eh_mv))




# 3. Detrend all data based on Control trends (depth-agnostic) -----------------

## There are a couple steps we're going to take to equitably but impartially 
## (i.e., same approach for all sensors) calculate when a time-series is 
## "disturbed" (value extends outside of control range based on Control plot)
## and when it "recovers", i.e., returns inside the control range.
##
## First, based on raw VWC, it's clear that we need to detrend, because there is 
## a remarkably consistent downward linear trend in VWC. That means we should 
## apply to the other metrics as well, even though they don't show strong linear
## trends in the control plot.
##
## Second, because initial values differ between plots, we should normalize in
## some way to account for that, so everything is starting on equal footing.

# detrend_data <- function(data, var) {
#   # Convert var to string (in case they are passed as bare variable names)
#   var <- as.character(substitute(var))
#   var_detrended <- paste0(var, "_detrended")
#   
#   # Convert datetime to numeric (seconds since start of experiment)
#   data <- data %>%
#     mutate(datetime_sec = as.numeric(difftime(datetime_est, min(datetime_est), units = "secs")))
#   
#   # Fit a linear model to the control plot
#   control_model <- data %>%
#     filter(plot == "Control") %>%
#     lm(reformulate("datetime_sec", var), data = .)
#   
#   # Extract the slope and intercept
#   intercept <- coef(control_model)[1]
#   slope <- coef(control_model)[2]
#   
#   # Detrend all the plots
#   data_detrended_raw <- data %>%
#     mutate("{var_detrended}" := !!sym(var) - (intercept + slope * datetime_sec))
#   
#   # Calculate mean values by depth for Control plot
#   control_stats <- data_detrended_raw %>% 
#     filter(plot == "Control") %>% 
#     summarize(range = max(.data[[var_detrended]], na.rm = TRUE) - min(.data[[var_detrended]], na.rm = TRUE))
#   
#   # Calculate min_control and max_control
#   data_detrended <- data_detrended_raw %>%
#     group_by(plot) %>%
#     mutate(min_control = mean(.data[[var_detrended]][datetime_est == min(datetime_est, na.rm = TRUE)], na.rm = TRUE) - 0.5 * control_stats$range,
#            max_control = mean(.data[[var_detrended]][datetime_est == min(datetime_est, na.rm = TRUE)], na.rm = TRUE) + 0.5 * control_stats$range)
#   
#   return(data_detrended)
# }
# 
# teros_detrend <- detrend_data(teros, vwc)
# teros_ec_detrend <- detrend_data(teros, ec)
# firesting_detrend <- detrend_data(firesting, do_percent_sat)
# swap_detrend <- detrend_data(swap, eh_mv)
# 
# initial_plots(teros_detrend, vwc_detrended) + 
#   geom_hline(aes(yintercept = min_control), linetype = "dashed")  + 
#   geom_hline(aes(yintercept = max_control), linetype = "dashed")
# 
# initial_plots(firesting_detrend, do_percent_sat_detrended) + 
#   geom_hline(aes(yintercept = min_control), linetype = "dashed")  + 
#   geom_hline(aes(yintercept = max_control), linetype = "dashed")
# 
# initial_plots(swap_detrend, eh_mv_detrended) + 
#   geom_hline(aes(yintercept = min_control), linetype = "dashed")  + 
#   geom_hline(aes(yintercept = max_control), linetype = "dashed")
# 
# initial_plots(teros_ec_detrend, ec_detrended) + 
#   geom_hline(aes(yintercept = min_control), linetype = "dashed")  + 
#   geom_hline(aes(yintercept = max_control), linetype = "dashed")
# 
# 
# 
# 
