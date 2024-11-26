## Initial workup of 

require(pacman)
p_load(tidyverse, readxl, janitor)

path <- "/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/COMPASS-DOE/tempest2_do/"

df <- read_xlsx(paste0(path, "data/raw_data/vegetation/241111_TEMPEST2_water_potentials.xlsx")) %>% 
  clean_names()

ggplot(df, aes(stem_id, water_potential, fill = plot, color = plot)) + 
  geom_boxplot(alpha = 0.2) + 
  geom_jitter(width = 0.5)

ggplot(df, aes(time, water_potential, color = plot)) + 
  geom_point()

df %>% 
  filter(time < 500 | time > 700) %>% 
ggplot(aes(stem_id, water_potential, fill = plot, color = plot)) + 
  geom_boxplot(alpha = 0.2) + 
  geom_jitter(width = 0.5)