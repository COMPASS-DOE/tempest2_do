
source("scripts/0_0_setup.R")
p_load(readxl)

df <- read_xlsx("data/raw_data/vegetation/241111_TEMPEST2_water_potentials.xlsx") %>% 
  clean_names() %>% 
  mutate(time_str = sprintf("%04d", time),                      # Ensure time is at least 4 digits
         time_hms = hms::as_hms(sprintf("%02d:%02d:00",
                                        as.integer(substr(time_str, 1, nchar(time_str)-2)),
                                        as.integer(substr(time_str, nchar(time_str)-1, nchar(time_str)))))) %>% 
  mutate(datetime = update(date, hours = hour(time_hms), minutes = minute(time_hms))) %>% 
  mutate(type = case_when(time < 600 ~ "Ypd", 
                          time > 1000 & time < 1400 ~ "Ymd", 
                          TRUE ~ "other"))

ggplot(df, aes(time, water_potential, color = stem_id)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  facet_wrap(~plot, ncol = 1, scales = "free_y")

ymd_ypd <- df %>% 
  filter(type != "other") %>% 
  filter(plot != "C") %>% 
  group_by(plot, stem_id, type) %>% 
  summarize(water_potential = median(water_potential, na.rm = T)) %>% 
  pivot_wider(names_from = "type", values_from = "water_potential") %>%
  mutate('Ymd-Ypd' = Ymd - Ypd) 

ymd_ypd %>% 
  ggplot(aes(plot, `Ymd-Ypd`)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  stat_compare_means()

plot_grid(ggplot(ymd_ypd, aes(plot, Ypd)) + 
            geom_boxplot() + 
            geom_jitter(alpha = 0.5, width = 0.1) + 
            stat_compare_means(), 
          ggplot(ymd_ypd, aes(plot, Ymd)) + 
            geom_boxplot() + 
            geom_jitter(alpha = 0.5, width = 0.1) + 
            stat_compare_means(), 
          nrow = 1)

## Only Beech trees were included
spp <- read_csv("data/from_kendal/sapflow_inventory.csv") %>% 
  clean_names() %>% 
  rename("stem_id" = sapflux_id) %>% 
  filter(stem_id %in% unique(df$stem_id)) %>% 
  dplyr::select(plot, stem_id, spp)
  

df %>% 
  filter(type != "other") %>% 
  filter(plot != "C") %>% 
  mutate(hour = hour(time_hms)) %>% 
  ggplot(aes(as.factor(hour), water_potential)) + 
  geom_boxplot() + 
  facet_wrap(~plot, ncol = 1)

df %>% 
  filter(type != "other") %>% 
  filter(plot != "C") %>% 
  mutate(hour = hour(time_hms)) %>% 
  ggplot(aes(as.factor(hour), water_potential, fill = plot)) + 
  geom_boxplot() 

df %>% 
  mutate(hour = hour(time_hms)) %>% 
  mutate(period = ifelse(hour < 6, "0_predawn", "1_midday"))



