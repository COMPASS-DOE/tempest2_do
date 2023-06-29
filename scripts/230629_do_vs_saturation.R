## This script explores ways to relate DO dynamics to soil saturation and redox
## conditions

require("scripts/0_setup.R")

common_cols <- c("datetime", "depth_cm", "plot")

redox <- read_csv("data/230618_swap_redox_raw.csv") %>% 
  select(common_cols, redox_mv)

do <- read_csv("data/230610_firesting.csv") %>%
  rename(depth_cm = "depth") %>% 
  select(common_cols, do_percent_sat) %>% 
  filter(plot != "Control")

teros <- read_csv("data/230610_teros_medians.csv") %>% 
  rename(depth_cm = "depth") %>% 
  select(common_cols, vwc, tsoil, ec)

df <- inner_join(teros, do, by = common_cols) %>% 
  label_flood_periods()

means <-  df %>% 
  group_by(depth_cm, plot, period) %>% 
  summarize(across(where(is.numeric), mean, na.rm = T))

perc_oxic <-  df %>% 
  group_by(depth_cm, plot, period) %>% 
  summarize(perc_anoxic = length(do_percent_sat[do_percent_sat < 5]) / length(do_percent_sat), 
            perc_hyoxic = length(do_percent_sat[do_percent_sat < 20]) / length(do_percent_sat))

inner_join(means, perc_oxic, by = c("depth_cm", "plot", "period")) %>% 
  ungroup() %>% 
  select(is.numeric) %>% 
  cor()

df %>% 
  filter(do_percent_sat < 20) %>% 
  group_by(depth_cm, plot) %>% 
  summarize(do = mean(do_percent_sat), 
            n_hypoxic = length(do_percent_sat),
            max_vwc = max(vwc))



