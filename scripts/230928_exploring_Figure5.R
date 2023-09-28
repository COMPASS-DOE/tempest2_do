# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


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


