## This script creates hysteresis plots for DO and redox against TEROS datasets
##
## 2023-06-19
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")

# 3. Prep data -----------------------------------------------------------------

floods <- c("Flood #1","Flood #2")

## Read in data and set windows. We're using a consistent 24-hour time-period
## to represent each section, since we observe that treatment impacts persist
## after each event, and the max window is 24 hours based on flood recurrance
## interval.

teros <- read_csv("data/230610_teros_medians.csv") %>% 
  label_flood_periods() %>% 
  filter(period_relabel %in% floods) %>% 
  group_by(plot, period_relabel) %>% 
  mutate(index = 1:n())

ggplot(teros, aes(datetime, vwc, color = as.factor(depth), size = index)) + 
  geom_point() + 
  facet_wrap(~plot, ncol = 1)

firesting <- read_csv("data/230610_firesting.csv") %>% 
  label_flood_periods() %>% 
  filter(period_relabel %in% floods) %>% 
  mutate(plot = case_when(plot == "FW" ~ "Freshwater", 
                          plot == "SW" ~ "Seawater")) %>% 
  mutate(depth = case_when(depth == 10 | depth == 20 ~ 15, 
                           TRUE ~ depth)) %>% 
  group_by(datetime, plot, depth) %>% 
  summarize(do_percent_sat = mean_(do_percent_sat), 
            period = first(period_relabel))

redox <- read_csv("data/230618_swap_redox_raw.csv") %>% 
  label_flood_periods() %>% 
  filter(period_relabel %in% floods) %>% 
  mutate(plot = case_when(plot == "FW" ~ "Freshwater", 
                          plot == "SW" ~ "Seawater")) %>% 
  rename("depth" = depth_cm) %>% 
  filter(depth != 50)

# 4. Bin and bind to single dataset --------------------------------------------

df <- full_join(teros, firesting, by = c("datetime", "plot", "depth")) %>% 
  full_join(redox, by = c("datetime", "plot", "depth")) %>% 
  ungroup() %>% 
  group_by(plot, period) %>% 
  filter(!is.na(period))


# 5. Function for plotting hysteresis ------------------------------------------

make_hysteresis

ggplot(df %>% filter(plot != "Control"), aes(vwc, do_percent_sat, pch = as.factor(depth))) + 
  geom_point(aes(color = index)) + 
  geom_path(aes()) + 
  facet_wrap(plot~period) + 
  scale_color_viridis_c()


ggplot(df %>% filter(plot != "Control"), aes(ec, do_percent_sat, color = as.factor(depth))) + 
  geom_point() + 
  geom_path() + 
  facet_wrap(plot~period)

ggplot(df %>% filter(plot != "Control"), aes(redox_mv, do_percent_sat, color = datetime)) + 
  geom_point() + 
  facet_wrap(plot~period)

ggplot(teros_bin, aes(datetime, vwc, color = plot)) + 
  geom_line()









