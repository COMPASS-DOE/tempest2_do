## This script will use the functions in 
## https://github.com/COMPASS-DOE/tempest-system-level-analysis/blob/recovery-rate-bounds/scripts/tmp_test_functions.R
## as a template to calculate recovery metrics and make recovery plots
##
## 2024-02-13
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")


# 2. Read in data --------------------------------------------------------------

## TEROS dataset
teros <- read_csv("data/240326_teros_final.csv") %>% 
  mutate(datetime_est = with_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

## DO dataset
firesting <- read_csv("data/240318_firesting_final.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

## Redox dataset
swap <- read_csv("data/240404_swap_final.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot)) %>% 
  #label_flood_periods() %>% 
  # mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood")) %>% 
  rename("depth" = depth_cm) %>% 
  #filter(!(plot == "Control" & depth == 5)) %>% # Filter out so it's comparable to VWC/DO
  filter(!(plot == "Control" & redox_mv == 0)) %>% # manually remove errors
  filter(!(plot == "Seawater" & redox_mv == 0)) %>% 
  filter(!is.na(eh_mv))

common_cols = c("datetime_est", "plot", "depth")

df <- inner_join(firesting %>% dplyr::select(datetime, all_of(common_cols), do_percent_sat), 
                 swap %>% dplyr::select(all_of(common_cols), eh_mv), 
                 by = common_cols) %>% 
  label_flood_periods() %>% 
  filter(period == "1_flood1" | 
           period == "2_flood2")

df %>% 
  filter(plot != "Control") %>% 
  ungroup() %>% 
  mutate(datetime_hourly = round_date(datetime_est, unit = "hour")) %>% 
  group_by(plot, depth, datetime_hourly) %>% 
  summarize(period = first(period), 
            do_percent_sat = mean(do_percent_sat), 
            eh_mv = mean(eh_mv)) %>% 
  ggplot(aes(do_percent_sat, eh_mv, 
             pch = as.factor(plot), color = datetime_hourly)) + 
  geom_path() + 
  geom_point(alpha = 0.5, size = 4) + 
  facet_wrap(period~depth) + 
  scale_color_viridis_c()


# 3. Make function -------------------------------------------------------------




## Function inputs: data = dataset, var = variable
## See 240213_recovery_metrics_calculation.RMD for a detailed explanation
plot_recovery <- function(data, var, multiplier){
  
  ## 1. Calculate SD of the Control plot for raw (non-zscore-normalized) data
  sd_control <- data %>% 
    filter(plot == "Control") %>% 
    group_by(depth) %>% 
    summarize(sd = sd({{var}}, na.rm = T)) %>% 
    ungroup() %>% 
    summarize(sd = max(sd)) %>% # Use max to capture maximum variability in the control plot
    pull(sd)
  
  ## 2. Calculate z-scores for var based on sd_control
  z_data <- data %>%
    #group_by(plot, depth) %>%
    group_by(plot) %>% 
    mutate(zscore = ({{var}} - mean({{var}}, na.rm = TRUE)) / sd_control) %>% 
    select(datetime_est, depth, zscore)
  
  ## 3. Set when the events start for Vpre calculation
  events_start <- as_datetime("2023-06-06 05:00:00", tz = common_tz)
  
  ## 4. Calculate Vpre for centering disturbance bounds
  vpre <- z_data %>% 
    mutate(vpre = case_when(datetime_est > events_start - hours(24) & 
                              datetime_est < events_start ~ "Vpre", 
                            TRUE ~ NA)) %>% 
    filter(vpre == "Vpre") %>% 
    ungroup() %>% 
    group_by(plot) %>% 
    summarize(vpre_mean = mean(zscore)) 
  
  ## 5. Set multiplier (how many standard deviations?)
  #multiplier = 1 - added as var
  
  ## 6. Bind data and make plot
  z_data %>% 
    inner_join(vpre, by = "plot") %>% 
    mutate(upper = vpre_mean + 1 * multiplier,  # 1 = SD of z-score Control
           lower = vpre_mean - 1 * multiplier) %>%  # 1 = SD of z-score Control
    ggplot(aes(datetime_est, zscore, color = as.factor(depth))) + 
    geom_line() + 
    geom_hline(aes(yintercept = upper)) + 
    geom_hline(aes(yintercept = lower), linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1, scales = "free_y") + 
    labs(x = "", color = "Depth (cm)")
}


plot_recovery_no_control <- function(data, var, multiplier){
  
  ## 1. Calculate SD of the Control plot for raw (non-zscore-normalized) data
  sd_control <- data %>% 
    filter(plot == "Control") %>% 
    group_by(depth) %>% 
    summarize(sd = sd({{var}}, na.rm = T)) %>% 
    ungroup() %>% 
    summarize(sd = max(sd)) %>% # Use max to capture maximum variability in the control plot
    pull(sd)
  
  ## 2. Calculate z-scores for var based on sd_control
  z_data <- data %>%
    #group_by(plot, depth) %>%
    group_by(plot) %>% 
    mutate(zscore = ({{var}} - mean({{var}}, na.rm = TRUE)) / sd_control) %>% 
    select(datetime_est, depth, zscore)
  
  ## 3. Set when the events start for Vpre calculation
  events_start <- as_datetime("2023-06-06 05:00:00", tz = common_tz)
  
  ## 4. Calculate Vpre for centering disturbance bounds
  vpre <- z_data %>% 
    mutate(vpre = case_when(datetime_est > events_start - hours(24) & 
                              datetime_est < events_start ~ "Vpre", 
                            TRUE ~ NA)) %>% 
    filter(vpre == "Vpre") %>% 
    ungroup() %>% 
    group_by(plot) %>% 
    summarize(vpre_mean = mean(zscore)) 
  
  ## 5. Set multiplier (how many standard deviations?)
  #multiplier = 1 - added as var
  
  ## 6. Bind data and make plot
  z_data %>% 
    inner_join(vpre, by = "plot") %>% 
    filter(plot != "Control") %>% 
    mutate(upper = vpre_mean + 1 * multiplier,  # 1 = SD of z-score Control
           lower = vpre_mean - 1 * multiplier) %>%  # 1 = SD of z-score Control
    ggplot(aes(datetime_est, zscore, color = as.factor(depth))) + 
    geom_line() + 
    geom_hline(aes(yintercept = upper)) + 
    geom_hline(aes(yintercept = lower), linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1, scales = "free_y") + 
    labs(x = "", color = "Depth (cm)")
}

# 4. Make plots ----------------------------------------------------------------

plot_path <- "figures/recovery_plots/240213_recovery_plot_"
pwidth = 6
pheight = 6

plot_recovery(teros, vwc, 3)
ggsave(paste0(plot_path, "teros_vwc.png"), width = pwidth, height = pheight)

plot_recovery(teros, ec, 3)
ggsave(paste0(plot_path, "teros_ec.png"), width = pwidth, height = pheight)

plot_recovery(firesting, do_percent_sat, 3)
ggsave(paste0(plot_path, "firesting_do.png"), width = pwidth, height = pheight)

plot_recovery(swap, eh_mv, 3)
ggsave(paste0(plot_path, "swap_eh.png"), width = pwidth, height = pheight)

plot_grid(plot_recovery_no_control(teros, vwc, 3) + theme(legend.position = "none") + ylab("Normalized VWC"), 
          plot_recovery_no_control(firesting, do_percent_sat, 3) + theme(legend.position = "none") + ylab("Normalized DO"), 
          plot_recovery_no_control(swap, eh_mv, 3) + ylab("Normalized Eh"),
          nrow = 1, rel_widths = c(1, 1, 1.25))
ggsave(paste0(plot_path, "all.png"), width = pwidth*2, height = pheight)



