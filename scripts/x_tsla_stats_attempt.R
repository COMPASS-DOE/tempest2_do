## Try and prove a new way to normalize stuff, and see if it works. It's going to
## be hard to describe, but I'll try with comments, but really it just needs to be 
## coded. If it works for VWC and for EC then it might work for other stuff

## First, load setup script to set up environment
source("scripts/0_0_setup.R")

## Read in TEROS dataset
teros <- read_csv("data/231102_teros_final.csv") %>% 
  mutate(datetime_est = with_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

## Trim to 30 cm and visualize
teros_trim <- teros %>% 
  filter(depth == 30) %>% 
  mutate(period = ifelse(datetime_est < dump_start1, "Vpre", "Event"))

vwc_sd = teros_trim %>% 
  filter(plot != "Control") %>% 
  filter(period == "Vpre") %>% 
  group_by(plot) %>% 
  summarize(sd = sd(vwc)) %>% 
  ungroup() %>% 
  summarize(mean(sd)) %>% 
  pull()

vwc_sd_control = teros_trim %>% 
  filter(plot != "Control") %>% 
  #filter(period == "Vpre") %>% 
  summarize(sd = sd(vwc)) %>% 
  pull()

ggplot(teros_trim, aes(datetime_est, vwc)) + 
  geom_line() + 
  facet_wrap(~plot, nrow = 1)

## Normalize via z-scores
teros_z <- teros_trim %>% 
  group_by(plot) %>% 
  mutate(z_vwc = (vwc - mean(vwc)) / sd(vwc)) %>% 
  mutate(z_ec = (ec - mean(ec)) / sd(ec)) %>% 
  mutate(vwc_n = z_vwc - first(z_vwc)) %>% 
  mutate(ec_n = z_ec - first(z_ec))

ggplot(teros_z, aes(datetime_est, vwc_n)) + 
  geom_line() + 
  facet_wrap(~plot, nrow = 1)

p_vwc <- teros_z %>% 
  select(datetime, plot, vwc_n) %>% 
  pivot_wider(names_from = plot,
              values_from = vwc_n) %>%
  pivot_longer(cols = c(Freshwater, Estuarine),
               names_to = "plot",
               values_to = "vwc_n") %>% 
  mutate(dif_vwc = vwc_n - Control) %>% 
  ggplot(aes(datetime, dif_vwc)) + 
  geom_line() + 
  facet_wrap(~plot, nrow = 1)

p_ec <- teros_z %>% 
  select(datetime, plot, ec_n) %>% 
  pivot_wider(names_from = plot,
              values_from = ec_n) %>%
  pivot_longer(cols = c(Freshwater, Estuarine),
               names_to = "plot",
               values_to = "ec_n") %>% 
  mutate(dif_ec = ec_n - Control) %>% 
  ggplot(aes(datetime, dif_ec)) + 
  geom_line() + 
  facet_wrap(~plot, nrow = 1)

plot_grid(p_vwc, p_ec, ncol = 1)



