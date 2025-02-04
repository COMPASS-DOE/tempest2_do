## This script explores if we can use Fausto's delineations to say something
## semi-quantitative about redox patterns and how they might relate to different
## processes
##
## 2025-01-29
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")


# 2. Prep data -----------------------------------------------------------------

swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm) %>% 
  mutate(eh_cat = case_when(eh_mv >= 400 ~ "1. oxidizing", 
                            eh_mv < 400 & eh_mv >= 200 ~ "2. weakly reducing", 
                            eh_mv < 200 & eh_mv >= -100 ~ "3. moderately reducing", 
                            eh_mv < -100 ~ "4. strongly reducing"))


# 2. Plot function for contour plots with continuous legend --------------------

# ggplot(swap, aes(datetime_est, as.factor(depth))) + 
#   geom_tile(aes(fill = eh_cat)) + 
#   facet_wrap(~plot, ncol = 1) + 
#   scale_x_datetime(expand = c(0,0)) + 
#   scale_fill_viridis_d(option = "D", direction = 1)

swap %>% 
  filter(datetime >= flood1 &
           datetime <= flood1 + days(5)) %>%
  group_by(plot, depth, eh_cat) %>% 
  summarize(n_eh_cat = n()) %>% 
  ungroup() %>% 
  group_by(plot, depth) %>% 
  mutate(perc = n_eh_cat / sum(n_eh_cat)) %>% 
  ggplot(aes(as.factor(depth), perc, fill = eh_cat)) + 
  geom_col(position = "stack", width = 0.8, alpha = 0.8) + 
  facet_wrap(~plot, ncol = 1)  + 
  scale_fill_viridis_d(option = "D", direction = 1) + 
  labs(x = "Depth (cm)", y = "Percent", fill = "") #+
  #theme(legend.position = "bottom")
ggsave("figures/x_eh_categories.png", width = 5, height = 4)




  