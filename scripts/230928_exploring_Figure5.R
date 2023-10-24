# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")

library(WaveletComp)
library(zoo)

# 2. Read in data --------------------------------------------------------------

firesting <- read_csv("data/230610_firesting.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))

teros <- read_csv("data/230712_teros_means.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))

df_do <- inner_join(firesting %>% dplyr::select(datetime, plot, period_relabel, depth, do_percent_sat), 
                 teros %>% select(datetime, plot, depth, vwc),
                 by = c("datetime", "plot", "depth")) %>% 
  filter(grepl("#", period_relabel)) %>% 
  filter(depth == 5) %>% 
  group_by(period_relabel, plot) %>% 
  mutate(group_index = 1:n()) %>% 
  ungroup()


df_do %>% 
  #filter(period_relabel == "Flood #1") %>% 
  ggplot(aes(vwc, do_percent_sat, color = group_index/4)) + 
  geom_path() + 
  geom_point() + 
  facet_wrap(period_relabel~plot) + 
  labs(x = "VWC (m3/m3", y = "DO (mg/L)", color = "Hrs since \n flood start")
ggsave("figures/230928_5cm_vwc_do_hysteresis.png", width = 8, height = 6)


p0 <- ggplot(df_do, aes(group_index, vwc)) + 
  geom_line() + 
  facet_wrap(plot ~ period_relabel, nrow = 1)

p1 <- ggplot(df_do, aes(group_index, do_percent_sat)) + 
  geom_line() + 
  #geom_point(aes(color = vwc)) + 
  facet_wrap(plot ~ period_relabel, nrow = 1)# + 
  #scale_color_viridis_c()


ggplot(df_do, aes(group_index, vwc)) + 
  geom_line() + 
  geom_point(aes(color = do_percent_sat, 
                 size = 100 - do_percent_sat)) + 
  facet_wrap(plot ~ period_relabel, nrow = 1) + 
  scale_color_viridis_c()

plot_grid(p0, p1, ncol = 1)

x <- df_do %>% 
  filter(plot == "Freshwater" & 
           period_relabel == "Flood #1") %>% 
  mutate(index = 1:n()) %>% 
  select(index, vwc, do_percent_sat) %>% 
  as.data.frame()

y <- analyze.wavelet(my.data = x, my.series = 2)

y <- analyze.coherency(my.data = x, my.pair = c("vwc", 
                                                "do_percent_sat"))

wt.image(y)


window_width <- 10

x %>%
  mutate(rolling_correlation = rollapply(do_percent_sat, width = window_width, 
                                         FUN = function(x) cor(x, vwc, method = "pearson"), 
                                         align = "right", 
                                         fill = NA))

