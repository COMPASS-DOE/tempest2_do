## stashing code used to detrend time-series just in case it becomes useful in
## future. 

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