## This script calculates k which is a potential proxy for belowground conductance
## per Nate. Based on Fick's law, Js = k(Ypd - Ymd), where Ypd = predawn water
## potential, Ymd = midday water potential, and Js is sapflow. Calculate since
## this will give us an estimate to inform the conceptual model

source("scripts/0_0_setup.R")

sapflow_raw <- read_csv("data/250310_2023_sapflow.csv")

p_load(readxl)

Y_raw <- read_xlsx("data/raw_data/vegetation/241111_TEMPEST2_water_potentials.xlsx") %>% 
  clean_names() %>% 
  mutate(time_str = sprintf("%04d", time),                      # Ensure time is at least 4 digits
         time_hms = hms::as_hms(sprintf("%02d:%02d:00",
                                        as.integer(substr(time_str, 1, nchar(time_str)-2)),
                                        as.integer(substr(time_str, nchar(time_str)-1, nchar(time_str)))))) %>% 
  mutate(datetime = update(date, hours = hour(time_hms), minutes = minute(time_hms))) %>% 
  mutate(type = case_when(time < 600 ~ "Ypd", 
                          time > 1000 ~ "Ymd", 
                          TRUE ~ "other")) %>% 
  mutate(hour = hour(time_hms))

ggplot(Y_raw, aes(as.factor(hour), water_potential, fill = plot)) + 
  geom_boxplot()

## All water potentials measured on Beech
spp <- read_csv("data/from_kendal/sapflow_inventory.csv") %>% 
  clean_names() %>% 
  rename("stem_id" = sapflux_id) %>% 
  filter(stem_id %in% unique(Y_raw$stem_id)) %>% 
  dplyr::select(plot, stem_id, spp)

Y_raw %>% 
  filter(type != "other") %>% 
  select(stem_id, type, water_potential) %>% 
  pivot_wider(names_from = "type", values_from = "water_potential")


## Ultimate goal is to compare single values for k between control and treatment
## plots



