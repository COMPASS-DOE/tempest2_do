## This script looks specifically at DBH to see if trees are growing faster in 
## the control plot compared to the treatment plots

source("scripts/0_0_setup.R")


## Read in species information
spp_raw <- read_csv("../data/tempest_tree_inventory.csv") %>% 
  clean_names() 

spp_long <- spp_raw %>%
  pivot_longer(
    cols = starts_with("dbh_") | starts_with("date_") | starts_with("status_"),
    names_to = c(".value", "year"),
    names_sep = "_") %>%
  rename(dbh = dbh,
         date = date,
         status = status) %>%
  filter(in_plot == TRUE) %>% # only trees in plots
  filter(status == "LI") %>% # only live trees
  select(-c(in_plot, notes, js_codes, date)) %>% 
  drop_na()

ggplot(spp_long, aes(as.numeric(year), dbh, color = as.factor(tag))) +
  geom_smooth(show.legend = F, se = F, lwd = 0.1) + 
  geom_point(show.legend = F) + 
  facet_wrap(~plot, nrow = 1)

## That doesn't show much, so let's summarize change
spp_long %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year == "2019" | 
           year == "2024") %>%
  pivot_wider(names_from = year,
              values_from = dbh,
              names_prefix = "dbh_") %>% 
  mutate(delta_dbh = ((dbh_2024 - dbh_2019)/dbh_2019) * 100) %>% 
  mutate(genus = str_sub(species_code, 1, 2)) %>% 
  filter(genus %in% c("AC", "CA", "FA", "LI")) %>% 
  ggplot(aes(genus, delta_dbh, color = plot)) + 
  geom_boxplot()

