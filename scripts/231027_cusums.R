## This script makes cumulative sums figures which may or may not be super 
## duper useful for understanding temporal sequences of things

## First, load setup script to set up environment
source("scripts/0_setup.R")

firesting <- read_csv("data/230610_firesting.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))

teros <- read_csv("data/230712_teros_means.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))

swap <- read_csv("data/230618_swap_redox_raw.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood")) %>% 
  rename("depth" = depth_cm)

common_cols <- c("datetime", "depth", "plot")

df <- inner_join(firesting %>% select(all_of(common_cols), period_relabel, do_percent_sat), 
                 swap %>% select(all_of(common_cols), redox_mv),
                 by = common_cols) %>% 
  inner_join(teros %>% select(all_of(common_cols), vwc), 
             by = common_cols) %>% 
  mutate(depth = as.factor(depth)) %>% 
  group_by(plot, depth) %>% 
  mutate(across(where(is.numeric), list(z = ~ (.-mean(.))/sd(.)))) %>% 
  mutate(across(contains("_z"), list(c = ~ cumsum(.))))

df_plot <- df %>% 
  select(datetime, contains("_c")) %>% 
  pivot_longer(cols = -c(datetime, plot, depth)) 

df_extrema <- df_plot %>% 
  filter(datetime < "2023-06-07 18:00") %>% # Limits to Flood #1 / Flood #2
  group_by(plot, depth, name) %>% 
  filter(abs(value) == max(abs(value))) %>% 
  mutate(time = format(datetime, "%H:%M"))
  

ggplot(df_plot, aes(datetime, value, color = name)) +
  geom_line() + 
  geom_point(alpha = 0.1) + 
  geom_point(data = df_extrema, color = "black", size = 3) + 
  geom_point(data = df_extrema, size = 2) + 
  geom_text(data = df_extrema, aes(label = time), hjust = -0.1) +
  add_line() + 
  facet_wrap(plot~depth)
ggsave("figures/231026_cusums_5_30_FW_SW.png", width = 9, height = 7)


ts_plot <- function(var){
  ggplot(df, aes(datetime, {{var}}, color = depth)) + 
    geom_line() + 
    facet_wrap(~plot, nrow = 1)
}

plot_grid(ts_plot(do_percent_sat), 
          ts_plot(do_percent_sat_z_c), 
          ncol = 1)



