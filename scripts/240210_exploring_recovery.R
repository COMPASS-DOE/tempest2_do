## Adapted from https://github.com/COMPASS-DOE/tempest-system-level-analysis/blob/recovery-rate-bounds/scripts/tmp_test_functions.R
## 

# 1. Load Packages -------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")

require(pacman)
pacman::p_load(dplyr, tidyr, ggplot2, readr, broom,
               janitor, lubridate)


# 2. Define Functions ----------------------------------------------------------

# Function to normalize datasets by calculating a zscore based on the standard 
# deviation of the Control plot for each analyte
# df is a data frame
# vars is a vector of one or more variables
# plot_col_name is a character vector
# scale_col_name is a character vector
zscore = function(df, vars, plot_col_name, scale_col_name){
  
  #calculate df of control plot for each analyte
  control_sd <- df %>%
    dplyr::filter({{plot_col_name}} == "Control") %>%
    summarise(across(vars,
                     list(min_c = ~min(.x, na.rm = TRUE),
                          max_c = ~max(.x, na.rm = TRUE)),
                     .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = where(is.numeric)) %>% 
  rename(sd_control = value)
  
  control_sd
  
   # df %>% 
   #   pivot_longer(cols = where(is.numeric)) %>% 
    #  left_join(control_sd, by = "name") #%>% 
     #group_by({{plot_col_name}}, {{scale_col_name}}, name) %>%
   # mutate(zscore = (value - mean(value, na.rm = TRUE)) / sd_control) %>% 
    # pivot_wider(names_from = name, values_from = where(is.numeric))
  
}


# 3. Bring in datasets ---------------------------------------------------------

firesting <- read_csv("data/230610_firesting.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(depth_chr = as.character(depth))

zscore(firesting, c("temp_c", "do_percent_sat"), plot, depth_chr)

