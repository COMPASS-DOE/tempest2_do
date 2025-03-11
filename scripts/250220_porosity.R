
## Load setup script
source("scripts/0_0_setup.R")

df <- read_csv("data/raw_data/tempest_porosity_data.csv") %>% 
  clean_names() %>% 
  mutate(plot = str_sub(name_of_sample, 1, 1))

unique(df$plot)

ggplot(df, aes(as.factor(approx_depth_cm), porosity, fill = as.factor(plot))) + 
  geom_boxplot()

ggplot(df, aes(vmc, porosity, color = as.factor(approx_depth_cm))) + 
  geom_point() +
  facet_wrap(~plot, nrow = 1)