## Maximum hourly decrease rates. Let's try something a little different than before,
## we're going to have a rolling hourly window, and we'll calculate the change 
## over that rolling window, then take the maximum across the whole time-series
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


# 2. Calculate rolling rates

group_columns = c("datetime_est", "plot", "depth")

teros %>% 
  ungroup() %>% 
  dplyr::select(all_of(group_columns), vwc) %>% 
  mutate(z_vwc = (vwc - mean(vwc, na.rm = T)) / sd(vwc, na.rm = T)) %>% 
  ggplot(aes(datetime_est, z_vwc, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

## Incubated function
zscore_data <- function(data, var) {
  var <- sym(var)
  z_var <- sym(paste0("z_", var))
  
  data %>% 
    ungroup() %>% 
    mutate(!!z_var := ( !!var - mean(!!var, na.rm = T)) / sd(!!var, na.rm = T)) %>% 
    dplyr::select(all_of(group_columns), !!z_var)
    # ggplot(aes(datetime_est, !!z_var, color = as.factor(depth))) + 
    # geom_line() + 
    # facet_wrap(~plot, ncol = 1)
}

# Call the function
right_join(zscore_data(teros, "vwc"), 
           zscore_data(firesting, "do_percent_sat"), 
           by = group_columns) %>% 
  inner_join(., zscore_data(swap, "eh_mv"), by = group_columns) %>% 
  drop_na() %>% 
  pivot_longer(cols = c(contains("z_"))) %>% 
  ggplot(aes(datetime_est, value, color = name)) + 
  geom_line() + 
  facet_wrap(depth~plot, ncol = 2)



