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
    geom_line(show.legend = F) + 
    facet_wrap(~plot, ncol = 1) 
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
  
  x %>%
    ungroup() %>%
    group_by(plot, depth) %>%
    summarize(vpre = mean(.data[[var]][time_period == "Vpre"], na.rm = TRUE),
              event_min = min(.data[[var]][time_period == "Event"], na.rm = TRUE),
              event_max = max(.data[[var]][time_period == "Event"], na.rm = TRUE)) %>%
    mutate(vdist = ifelse(abs(event_min - vpre) > abs(event_max - vpre), event_min, event_max),
           lrr = log(vdist/vpre)) %>%
    ungroup() %>% 
    complete(plot, depth = c(5, 10, 15, 20, 30, 50), 
             fill = list(vpre = NA, event_min = NA, event_max = NA, vdist = NA, lrr = NA))
}

plot_resistance <- function(...){
  
  dodge_width = 5
  
  ggplot(data = ..., aes(depth, lrr, color = plot)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_linerange(aes(ymin = 0, ymax = lrr), position = position_dodge(width = dodge_width)) +
    geom_point(position = position_dodge(width = dodge_width), size = 3) +
    coord_flip() +
    scale_x_reverse() +
    labs(x = "Depth (cm)", y = "Log response ratio", color = "") + 
    scale_color_manual(values = c("#25B163", "#C13E72", "#0DB0AE"))
}

plot_grid(plot_resistance(calculate_lrr(teros, vwc)) + 
            theme(legend.position = "none") + 
            ggtitle("VWC (m3/m3)"), 
          plot_resistance(calculate_lrr(firesting %>% 
                          mutate(do_percent_sat = ifelse(do_percent_sat == 0, 0.01, do_percent_sat)), 
                        do_percent_sat)) + 
            theme(legend.position = "none") + 
            ggtitle("DO (mg/L)"), 
          plot_resistance(calculate_lrr(swap, eh_mv)) + 
                            theme(legend.position = "none") + 
            ggtitle("Eh (mV)"), 
          get_legend(plot_resistance(calculate_lrr(swap, eh_mv))), 
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
  
  event_stats <- x %>%
    ungroup() %>%
    group_by(plot, depth) %>%
    summarize(vpre_mean = mean(.data[[var]][time_period == "Vpre"], na.rm = TRUE),
              vpre_sd = sd(.data[[var]][time_period == "Vpre"], na.rm = TRUE),
              event_min = min(.data[[var]][time_period == "Event"], na.rm = TRUE),
              event_max = max(.data[[var]][time_period == "Event"], na.rm = TRUE)) %>% 
    left_join(x %>% 
                filter(time_period == "Event") %>%
                group_by(plot, depth) %>%
                summarize(event_min_time = datetime_est[which.min(.data[[var]])],
                          event_max_time = datetime_est[which.max(.data[[var]])]),
              by = c("plot", "depth")) %>% 
    mutate(vdist = ifelse(abs(event_min - vpre_mean) > abs(event_max - vpre_mean), 
                          event_min, event_max),
           vdist_dir = ifelse(abs(event_min - vpre_mean) > abs(event_max - vpre_mean), 
                          "decrease", "increase"),
           vdist_time = as_datetime(ifelse(abs(event_min - vpre_mean) > abs(event_max - vpre_mean), 
                               event_min_time, event_max_time), tz = common_tz),
           lrr = log(vdist/vpre_mean)) %>%
    ungroup() %>% 
    complete(plot, depth = c(5, 10, 15, 20, 30, 50), 
             fill = list(vpre_mean = NA, 
                         event_min = NA, 
                         event_max = NA, 
                         event_min_time = NA,
                         event_max_time = NA,
                         vdist = NA, 
                         vdist_time = NA,
                         lrr = NA))
  
  
  #return(event_stats)
  
  x %>%
    full_join(event_stats, by = c("plot", "depth")) %>%
    ungroup() %>%
     group_by(plot, depth) %>%
    filter(ifelse(vdist_dir == "increase",
                  datetime_est > event_max_time,
                  datetime_est > event_min_time)) %>% 
    mutate(baseline = case_when(vdist_dir == "increase" ~ vpre_mean + vpre_sd, 
                                vdist_dir == "decrease" ~ vpre_mean + vpre_sd,
                                TRUE ~ NA)) %>% 
    summarize(vdist_time = as_datetime(first(vdist_time), 
                             tz = common_tz), 
              return_time = as_datetime(ifelse(first(vdist_dir) == "increase", 
                                  datetime_est[which(.data[[var]] <= baseline)[1]], 
                                  datetime_est[which(.data[[var]] >= baseline)[1]]), 
              tz = common_tz))
}


bind_rows(calculate_response(teros, vwc) %>% mutate(var = "vwc"), 
          calculate_response(firesting, do_percent_sat) %>% mutate(var = "do"), 
          calculate_response(swap, eh_mv) %>% mutate(var = "eh")) %>% 
  mutate(recovery = as.numeric(return_time - vdist_time)) %>% 
  filter(plot != "Control") %>% 
  ggplot(aes(recovery, as.factor(depth), color = var)) + 
  geom_point() + 
  facet_wrap(~plot) + 
  scale_x_log10()




dodge_width = 0.5

calculate_response(teros, vwc) %>% 
  ggplot(aes(datetime_est, vwc, color = recovered, group = as.factor(depth))) +
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

bind_rows(calculate_response(teros, vwc), 
          calculate_response(firesting, do_percent_sat), 
          calculate_response(swap, eh_mv)) %>% 
  mutate(recovery_hours = ifelse(is.na(recovery_hours), 150, recovery_hours)) %>% 
  ggplot(aes(as.factor(depth), recovery_hours, color = plot)) + 
  geom_linerange(aes(ymin = 0, ymax = recovery_hours), position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width), size = 3) +
  facet_wrap(~var, nrow = 1) + 
  coord_flip()


  
  




  
## PI meeting figure - delete later

x <- teros %>% 
  filter(plot == "Freshwater" & 
           depth == 5) %>%
  filter(datetime_est > "2023-06-05" & 
           datetime_est < "2023-06-09") %>% 
  mutate(vpre = mean(vwc[datetime_est < dump_start1]), 
         vdist = ifelse(vwc == max(vwc), "vdist", NA))

x %>% 
  ggplot(aes(datetime_est, vwc)) + 
  geom_line() + 
  geom_line(data = x %>% filter(datetime < dump_start1), aes(y = vpre), color = "red") + 
  geom_point(data = x %>% filter(vdist == "vdist"), color = "red", size = 4, alpha = 0.7) + 
  annotate("rect", xmin = min(x$datetime_est), xmax = dump_start1, 
           ymin = 0.33, ymax = 0.425, fill = "gray", alpha = 0.2) +
  annotate("rect", xmin = dump_start1, xmax = max(x$datetime_est), 
           ymin = 0.33, ymax = 0.425, fill = "gray30", alpha = 0.2) +
  labs(x = "Datetime", y = "Response") + 
  scale_y_continuous(limits = c(0.33, 0.425)) + 
  theme(#axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())
ggsave("figures/240414_effect_calc_plot.png", width = 3, height = 2.5)


## PI meeting stats

swap %>% 
  group_by(plot, depth) %>% 
  summarize(vpre = mean(eh_mv[datetime_est < dump_start1], na.rm = T), 
            vdist = min(eh_mv, na.rm = T)) %>% 
  mutate(ratio = (vdist-vpre) / vpre)



