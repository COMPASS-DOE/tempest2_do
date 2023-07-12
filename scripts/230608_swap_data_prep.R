## This script imports redox sensor data, which is stored on the SERC Dropbox 
## under .../TEMPEST_PNNL_Data/Current_data". Because this is a finite time-period, 
## and data aren't going to change, I'm going to pull static copies so it's
## 1) easier to put on github/etc, 2) not going to change, and 3) pipe won't break
## if dropbox comms drop
##
## 2023-06-06 (updated 6/28/23)
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")

## Load packages
require(pacman)
p_load(tidyverse, 
       rdrop2, 
       cowplot, 
       lubridate, 
       plotly,
       parsedate, # parse_date()
       janitor, # clean_names()
       purrr) # map()

## Set ggplot theme
theme_set(theme_bw())


# 2. Pull in data and format ---------------------------------------------------

## Control is PB81, logger is labeled CR1000
## FW is PB82, logger is labeled CR1000_2
## SW is PB83, logger is labeled CR1000_3

read_swap <- function(path){
  read_delim(path, skip = 1) %>% 
    slice(3:n()) %>% 
    mutate(across(c(contains("Batt"), contains("Redox")), as.numeric)) %>% 
    clean_names() %>% 
    mutate(datetime = lubridate::as_datetime(timestamp, tz = common_tz)) 
}


swap_list <- list.files(path = "data/swap", pattern = ".dat", full.names = "T")

control <- read_swap(swap_list[grepl("81", swap_list)]) %>% 
  mutate(plot = "Control")
fw <- read_swap("data/swap/CR1000_2_Table1.dat") %>% 
  mutate(plot = "FW")
sw <- read_swap(swap_list[grepl("83", swap_list)]) %>% 
  mutate(plot = "SW")

set_depths <- function(data){
  data %>% 
    pivot_longer(cols = contains("redox_"), names_to = "sensor", values_to = "redox_mv") %>% 
    separate(sensor, into = c("scrap", "ref", "sensor"), sep = "_") %>% 
    mutate(depth_cm = case_when(sensor == "1" | sensor == "5" | sensor == "9" | sensor == "13" | sensor == "17" ~ 5, 
                             sensor == "2" | sensor == "6" | sensor == "10" | sensor == "14" | sensor == "18" ~ 15, 
                             sensor == "3" | sensor == "7" | sensor == "11" | sensor == "15" | sensor == "19" ~ 30, 
                             sensor == "4" | sensor == "8" | sensor == "12" | sensor == "16" | sensor == "20" ~ 50,
                             TRUE ~ 0)) %>% 
    select(-c(statname, scrap, timestamp))
}

df_raw <- bind_rows(set_depths(control), 
                set_depths(fw), 
                set_depths(sw)) %>% 
  filter(datetime > pre_event_start & 
           datetime < post_event_end)

# Because FW record starts before Control/SW, trim to match
sw_start <- min(df_raw %>% filter(plot == "SW") %>% pull(datetime))

## We're also filtering to just the first ref probe based on early issues with rb
df_final <- df_raw %>% 
  group_by(datetime, depth_cm, ref, plot) %>% 
  summarize(batt_v = mean(batt_v), 
            redox_mv = mean(redox_mv)) %>% 
  filter(datetime >= sw_start) %>% 
  filter(ref == "ra") %>% 
  mutate(datetime_raw = as.character(datetime)) %>% 
  mutate(plot = case_when(plot == "FW" ~ "Freshwater", 
                          plot == "SW" ~ "Seawater", 
                          TRUE ~ plot))

df_final %>% 
  filter(datetime > sw_start) %>% 
  ggplot(aes(datetime, depth_cm)) + 
  geom_contour_filled(aes(z = redox_mv), bins = 20) + 
  scale_y_reverse() + 
  geom_vline(aes(xintercept = dump_start1), color = "black", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "black", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "", y = "Depth (cm)", fill = "Redox (mv)")
ggsave("figures/230609_redox_contours.png", width = 9, height = 8)

write_csv(df_final, "data/230618_swap_redox_raw.csv")



