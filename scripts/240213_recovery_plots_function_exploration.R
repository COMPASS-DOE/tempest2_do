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

firesting <- read_csv("data/230712_firesting.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

## Subset to build function
x <- firesting %>% 
  select(datetime_est, plot, depth, do_percent_sat)

ggplot(x, aes(datetime_est, do_percent_sat, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

# 3. Calculate modified cusums -------------------------------------------------

## Calculate the SD of the control. Since we have multiple depths but they aren't
## all co-located with test plots, we'll calculate standard deviations for each
## depth then average that for a single bound
sd_control <- x %>% 
  filter(plot == "Control") %>% 
  group_by(depth) %>% 
  summarize(sd = sd(do_percent_sat, na.rm = T)) %>% 
  ungroup() %>% 
  summarize(sd = max(sd)) %>% # Let's take max as a good-faith effort to capture maximum variability in the control plot
  pull(sd)

x %>% 
  group_by(plot, depth) %>% 
  mutate(zscore = (do_percent_sat - mean(do_percent_sat, na.rm = TRUE)) / sd_control) %>% 
  ggplot(aes(datetime_est, zscore, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)



## That turned out weird. Let's try it with TEROS VWC
teros <- read_csv("data/231102_teros_final.csv") %>% 
  mutate(datetime_est = with_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

y <- teros %>% 
  select(datetime_est, plot, depth, vwc)

sd_control <- y %>% 
  filter(plot == "Control") %>% 
  group_by(depth) %>% 
  summarize(sd = sd(vwc, na.rm = T)) %>% 
  ungroup() %>% 
  summarize(sd = max(sd)) %>% # Let's take max as a good-faith effort to capture maximum variability in the control plot
  pull(sd)

events_start <- as_datetime("2023-06-06 05:00:00", tz = common_tz)

z <- y %>% 
  group_by(plot, depth) %>% 
  mutate(zscore = (vwc - mean(vwc, na.rm = TRUE)) / sd_control) %>% 
  mutate(vpre = case_when(datetime_est > events_start - hours(24) & 
                            datetime_est < events_start ~ "Vpre", 
                          TRUE ~ NA))

vpre_means <- z %>% 
  filter(vpre == "Vpre") %>% 
  group_by(plot) %>% 
  summarize(vpre_mean = mean(zscore))

multiplier = 1

to_plot <- z %>% 
  inner_join(vpre_means, by = c("plot")) %>% 
  select(-vpre) %>% 
  mutate(upper = vpre_mean + 1 * multiplier, 
         lower = vpre_mean - 1 * multiplier)

ggplot(to_plot, aes(datetime_est, zscore, color = as.factor(depth))) + 
  geom_line() + 
  geom_hline(aes(yintercept = upper)) + 
  geom_hline(aes(yintercept = lower), linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1)






