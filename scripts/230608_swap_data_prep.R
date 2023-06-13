## This script imports redox sensor data
##
## 2023-06-06
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

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


swap_path <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Current_data"
swap_list <- list.files(swap_path, "Redox", full.names = T)

control <- read_swap(swap_list[grepl("81", swap_list)]) %>% 
  mutate(plot = "Control")
fw <- read_swap("data/redox_sensors/CR1000_2_Table1.dat") %>% 
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

df <- bind_rows(set_depths(control), 
                set_depths(fw), 
                set_depths(sw)) %>% 
  filter(datetime > "2023-06-06")

max_datetime <- max(df %>% filter(plot == "FW") %>% pull(datetime))

df_trim <- df %>% 
  filter(datetime <= max_datetime)

# ggplot(df, aes(datetime, redox_mv, group = sensor, color = depth_cm)) + 
#   geom_line() + 
#   facet_wrap(ref~plot)
# 
# p <- ggplot(df %>% filter(plot == "FW" & ref == "ra"), aes(datetime, redox_mv, group = sensor)) + 
#   geom_line() + 
#   facet_wrap(~depth_cm)
# 
# ggplotly(p)


write_csv(df, "data/230609_swap_redox_raw.csv")

dump_start1 = as.POSIXct("2023-06-06 05:00", tz = common_tz)
dump_start2 = as.POSIXct("2023-06-07 05:00", tz = common_tz)

df_trim %>% 
  group_by(datetime, depth_cm, ref, plot) %>% 
  summarize(batt_v = mean(batt_v), 
            redox_mv = mean(redox_mv)) %>% 
  filter(ref == "ra") %>% 
  ggplot(aes(datetime, depth_cm)) + 
  #geom_raster(aes(fill = redox_mv)) + 
  geom_contour_filled(aes(z = redox_mv), bins = 10) + 
  scale_y_reverse() + 
  geom_vline(aes(xintercept = dump_start1), color = "black", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "black", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "", y = "Depth (cm)", fill = "Redox (mv)")
ggsave("figures/230609_redox_contours.png", width = 9, height = 8)






