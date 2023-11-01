## Calculate wavelet coherence between redox and DO at 5 and 30 cm to see if that
## helps us decipher what's happening
## https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018WR024555
## see Fig 1 for inspiration of a cool way to show DO-redox-vwc relationships

require(pacman)
p_load(tidyverse,
       WaveletComp, 
       RTransferEntropy)

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

ggplot(swap, aes(datetime, redox_mv, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(ref ~ plot)

common_cols <- c("datetime", "depth", "plot")

x <- inner_join(firesting %>% select(all_of(common_cols), do_percent_sat), 
                swap %>% select(all_of(common_cols), redox_mv),
                by = common_cols) %>% 
  inner_join(teros %>% select(all_of(common_cols), vwc), 
             by = common_cols) %>% 
  filter(plot == "Freshwater" & 
           depth == 5) %>% 
  select(-depth) %>% 
  mutate(across(where(is.numeric), list(z = ~ (.-mean(.))/sd(.)))) %>% 
  mutate(across(contains("_z"), list(c = ~ cumsum(.))))

x %>% 
  select(datetime, contains("_c")) %>% 
  pivot_longer(cols = -c(datetime)) %>% 
  ggplot(aes(datetime, value, color = name)) +
  geom_line() + 
  geom_point(alpha = 0.1)


x1 <- inner_join(firesting %>% select(all_of(common_cols), do_percent_sat), 
                swap %>% select(all_of(common_cols), redox_mv),
                by = common_cols) %>% 
  inner_join(teros %>% select(all_of(common_cols), vwc), 
             by = common_cols) %>% 
  mutate(depth = as.factor(depth)) %>% 
  group_by(plot, depth) %>% 
  mutate(across(where(is.numeric), list(z = ~ (.-mean(.))/sd(.)))) %>% 
  mutate(across(contains("_z"), list(c = ~ cumsum(.))))

x1 %>% 
  select(datetime, contains("_c")) %>% 
  pivot_longer(cols = -c(datetime, plot, depth)) %>% 
  ggplot(aes(datetime, value, color = name)) +
  geom_line() + 
  geom_point(alpha = 0.1) + 
  facet_wrap(plot~depth)
ggsave("figures/231026_cusums_5_30_FW_SW.png", width = 5, height = 7)


te_vwc_do <- transfer_entropy(x = x$vwc, 
                       y = x$do_percent_sat)


te_vwc_redox <- transfer_entropy(x = x$vwc, 
                       y = x$redox_mv)


te_do_redox <- transfer_entropy(x = x$do_percent_sat, 
                       y = x$redox_mv)

y <- x %>% 
  rename("do" = do_percent_sat,
         "redox" = redox_mv) %>% 
  select(datetime, do, redox, vwc) %>% 
  as.data.frame()

z <- analyze.coherency(my.data = y, 
                       my.pair = c("vwc", "redox"))

wc.image(z)


## DTW?

p_load(dtw)

a <- dtw(x = y$vwc, 
         y = y$do, 
         keep=TRUE)

plot(a, type="threeway")

