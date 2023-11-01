## Let's make a concerted effort to visualize our datasets and the relationships
## as mutual information. The idea is to see which parameters drive redox, and 
## if their importance changes with 1) depth, or 2) salinity


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

common_cols <- c("datetime", "depth", "plot")

df <- inner_join(firesting %>% select(all_of(common_cols), do_percent_sat), 
                swap %>% select(all_of(common_cols), redox_mv),
                by = common_cols) %>% 
  inner_join(teros %>% select(all_of(common_cols), tsoil, ec, vwc), 
             by = common_cols)


## First, make a test dataset
x <- df  %>% 
  filter(plot == "Freshwater" & 
           depth == 5) %>% 
  rename("do" = do_percent_sat, 
         "redox" = redox_mv) %>% 
  mutate(rand1 = runif(nrow(x)), 
         rand2 = runif(nrow(x))) %>% 
  drop_na() %>% 
  mutate(index = 1:n()) %>% 
  select(index, vwc, do, redox, rand1, rand2) 

#ggplot(x, aes(index, redox)) + geom_point()

calc_mi <- function(selected_plot, 
                    selected_depth){
  x <- df %>% 
    filter(plot == selected_plot & 
             depth == selected_depth) %>% 
    drop_na() %>% 
    rename("do" = do_percent_sat, 
           "redox" = redox_mv) %>%
    mutate(rand = runif(nrow(.))) %>%
    select(-c(datetime, depth, plot))
  
  xd <- discretize(x)
  
  tibble(plot = selected_plot, 
         depth = selected_depth, 
         m_0 = mutinformation(xd$redox, xd$rand), 
         m_1 = mutinformation(xd$redox, xd$redox),
         m_tsoil = mutinformation(xd$redox, xd$tsoil), 
         m_ec = mutinformation(xd$redox, xd$ec), 
         m_vwc = mutinformation(xd$redox, xd$vwc), 
         m_do = mutinformation(xd$redox, xd$do))
}

mi_raw <- bind_rows(calc_mi("Freshwater", 5), 
          calc_mi("Seawater", 5), 
          calc_mi("Freshwater", 30), 
          calc_mi("Seawater", 30))

mi_raw %>% 
  mutate(across(contains("m_"), function(...){(... - m_0) / m_1})) %>% 
  select(-c(m_0, m_1)) %>% 
  pivot_longer(cols = c("m_tsoil", "m_ec", "m_vwc", "m_do")) %>% 
  ggplot(aes(name, value)) + 
  geom_col() + 
  facet_wrap(plot~depth)
ggsave("figures/231026_mutual_information.png", width = 6, height = 6)





