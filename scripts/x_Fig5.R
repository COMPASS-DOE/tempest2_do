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


# 3. Calculate and plot resistance ---------------------------------------------

## Based on  Patrick et al. (2020)'s stats: ln(min/max / vpre baseline)
## https://doi.org/10.1007/s12237-019-00690-3

## Function to calculate log-response ratio and plot it
calculate_lrr <- function(data, var) {
  # Convert var to string (in case they are passed as bare variable names)
  var <- as.character(substitute(var))
  
  x <- data %>%
    mutate(time_period = case_when(
      datetime_est < as_datetime("2023-06-06 05:00:00", tz = common_tz) ~ "Vpre",
      datetime_est < as_datetime("2023-06-07 15:00:00", tz = common_tz) ~ "Event", 
      TRUE ~ NA_character_)) 

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
    ggplot(aes(depth, lrr, color = plot)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
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


# 4. Calculate and plot response -----------------------------------------------

## Also from Patrick et al. (2020) - the number of hours it takes for values to
## return to baseline (mean of vpre). I'm going to amend this a little and add
## some wiggle room via standard deviation of vpre as well.

calculate_response <- function(data, var){
  
  # Convert var to string (in case they are passed as bare variable names)
  var <- as.character(substitute(var))
  
  x <- data %>%
    mutate(time_period = case_when(
      datetime_est < as_datetime("2023-06-06 05:00:00", tz = common_tz) ~ "Vpre",
      datetime_est < as_datetime("2023-06-07 15:00:00", tz = common_tz) ~ "Event", 
      TRUE ~ "Post")) 
  
  ## Calculate Vpre stats
  vpre_stats <- x %>% 
    ungroup() %>%
    group_by(plot, depth) %>%
    summarize(vpre = mean(.data[[var]][time_period == "Vpre"], na.rm = TRUE),
              vpre_sd = sd(.data[[var]][time_period == "Vpre"], na.rm = TRUE)) 
  
  ## Calculate event stats
  event_stats <- x %>% 
    ungroup() %>%
    group_by(plot, depth) %>%
    filter(time_period == "Event") %>% #which.min doesn't work with var[...] filtering
    summarize(event_min = min(.data[[var]], na.rm = TRUE),
              event_max = max(.data[[var]], na.rm = TRUE), 
              event_min_time = datetime_est[which.min(.data[[var]])],
              event_max_time = datetime_est[which.max(.data[[var]])]) 
  
  ## Combine stats tibbles
  stats <- inner_join(vpre_stats, 
                      event_stats, 
                      by = c("plot", "depth")) %>%
    mutate(vdist = ifelse(abs(event_min - vpre) > abs(event_max - vpre), "decrease", "increase")) 
  
  inner_join(x, stats, by = c("plot", "depth")) %>% 
    mutate(recovery_value = ifelse(vdist == "decrease", vpre - vpre_sd, vpre + vpre_sd)) %>% 
    mutate(recovered = ifelse((vdist == "decrease" & var >= recovery_value) | 
                                (vdist == "increase" & var <= recovery_value), "recovered", "disturbed")) %>% 
    filter(time_period == "Post" & recovered == "recovered") %>%
    ungroup() %>% 
    group_by(plot, depth) %>% 
    summarize(recovery_time = first(datetime_est)) %>% 
    right_join(stats, by = c("plot", "depth")) %>% 
    mutate(recovery_hours = ifelse(vdist == "decrease", 
                                   difftime(recovery_time, event_min_time, units = "hours"),
                                   difftime(recovery_time, event_max_time, units = "hours"))) %>% 
    mutate(var = var)
  
}

dodge_width = 0.5

bind_rows(calculate_response(teros, vwc), 
          calculate_response(firesting, do_percent_sat), 
          calculate_response(swap, eh_mv)) %>% 
  mutate(recovery_hours = ifelse(is.na(recovery_hours), 150, recovery_hours)) %>% 
  ggplot(aes(as.factor(depth), recovery_hours, color = plot)) + 
  geom_linerange(aes(ymin = 0, ymax = recovery_hours), position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width), size = 3) +
  facet_wrap(~var, nrow = 1) + 
  coord_flip()
  
  
  
