## This script calculates k which is a potential proxy for belowground conductance
## per Nate. Based on Fick's law, Js = k(Ypd - Ymd), where Ypd = predawn water
## potential, Ymd = midday water potential, and Js is sapflow. Calculate since
## this will give us an estimate to inform the conceptual model
## We need two things to calculate this: 1) sapflow for 6/13 for each tree we 
## measured water potential for, and 2) water potentials

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_0_setup.R")
p_load(readxl)


# 2. Read water potentials -----------------------------------------------------

Y_raw <- read_xlsx("data/raw_data/vegetation/241111_TEMPEST2_water_potentials.xlsx") %>% 
  clean_names() %>% 
  mutate(time_str = sprintf("%04d", time),                      # Ensure time is at least 4 digits
         time_hms = hms::as_hms(sprintf("%02d:%02d:00",
                                        as.integer(substr(time_str, 1, nchar(time_str)-2)),
                                        as.integer(substr(time_str, nchar(time_str)-1, nchar(time_str)))))) %>% 
  mutate(datetime = update(date, hours = hour(time_hms), minutes = minute(time_hms))) %>% 
  mutate(type = case_when(time < 600 ~ "Ypd", 
                          time >= 1100 & time <= 1200 ~ "Ymd", 
                          TRUE ~ "other")) %>% 
  mutate(hour = hour(time_hms))

Y_raw %>% 
  filter(type != "other") %>% 
  ggplot(aes(as.factor(hour), water_potential, color = plot)) + 
  geom_point() + 
  facet_wrap(~stem_id)


# 3. Calculate change in water potential (Ypd - Ymd) ---------------------------

Ypd_Ymd <- Y_raw %>% 
  filter(type != "other") %>% 
  group_by(plot, stem_id, type) %>% 
  summarize(water_potential = mean(water_potential, na.rm = T)) %>% 
  pivot_wider(names_from = "type", values_from = "water_potential") %>% 
  mutate(Ypd_Ymd = Ypd - Ymd)

ggplot(Ypd_Ymd, aes(plot, Ypd_Ymd)) + 
  geom_boxplot()


# 4. Read in sapflow -----------------------------------------------------------

sapflow_raw <- read_csv("data/250421_sapflow_by_tree.csv")

sapflow_trim <- sapflow_raw %>% 
  filter(Date == "2023-06-13")


# 5. Calculate k! --------------------------------------------------------------

## Join datasets and calculate k
df <- inner_join(sapflow_trim %>% dplyr::select(-plot), 
                 Ypd_Ymd, 
                 by = c("sensor_id" = "stem_id")) %>% 
  mutate(k = F_avg / Ypd_Ymd) %>% 
  filter(k > -4)
  
ggplot(df, aes(plot, k)) + 
  geom_boxplot() + 
  geom_tukey(where = "whisker")











