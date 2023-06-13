## This script reads in Firesting data. All instruments were set in EST, so
## time-zones shouldn't be an issue. However, there are multiple files that need
## to be explained: 
## FW: there are three datasets, 1 is prior to events, 2 is most of both events,
## and 3 is the tail end of the second event
## SW: there are two datasets, 1 is most of both events, 2 is the tail end of the
## second event
## Control is three handhelds, so three separate filepaths
##
## 2023-06-08
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


# 2. Read in data --------------------------------------------------------------

## Set paths
fw1_filepath <- "data/firesting/2023-06-04_082732_230604_fw_tempest_1/ChannelData"
fw2_filepath <- "data/firesting/2023-06-05_100032_230605_fw_tempest_2/ChannelData/"
fw3_filepath <- "data/firesting/2023-06-08_081002_230605_fw_tempest_3/ChannelData/"
sw1_filepath <- "data/firesting/2023-06-04_084853_230604_sw_tempest_1/ChannelData/"
sw2_filepath <- "data/firesting/2023-06-08_081743_230607_tempest_sw_2/ChannelData/"


## Set raw firesting data column names
firesting_names <- c("date", "time", "dt_s", "do_percent_sat", "dphi", 
                     "intensity_mv", "light_mv", "do_status", "temp_date", "temp_time", 
                     "temp_dt_s", "temp_c", "temp_status", "press_date", "press_time", 
                     "press_dt_s", "pressure_mbar", "press_status")


import_4channel <- function(path){
  
  import_firesting <- function(filepath) {
    ## Read in the 4 O2 channels and temperature
    read_delim(filepath, delim = "\t", skip = 25,  col_names = F) %>% 
      magrittr::set_colnames(firesting_names) %>% 
      mutate(datetime_raw = paste(date, time)) %>% 
      mutate(datetime = lubridate::as_datetime(datetime_raw, format = "%d-%m-%Y %H:%M:%S", 
                                                   tz = common_tz)) %>% 
      dplyr::select(datetime_raw, datetime, temp_c, do_percent_sat)
  }
  
  firsting1 <- import_firesting(file.path(path, "A_Firesting O2 (4 Channels)_(A Ch.1)_Oxygen.txt")) %>%
    mutate(depth = 5)
  firsting2 <- import_firesting(file.path(path, "A_Firesting O2 (4 Channels)_(A Ch.2)_Oxygen.txt")) %>% 
    mutate(depth = 10)
  firsting3 <- import_firesting(file.path(path, "A_Firesting O2 (4 Channels)_(A Ch.3)_Oxygen.txt")) %>% 
    mutate(depth = 20)
  firsting4 <- import_firesting(file.path(path, "A_Firesting O2 (4 Channels)_(A Ch.4)_Oxygen.txt")) %>% 
    mutate(depth = 30)
  
  bind_rows(firsting1, firsting2, firsting3, firsting4)
}


fw <- bind_rows(import_4channel(fw1_filepath), 
                import_4channel(fw2_filepath), 
                import_4channel(fw3_filepath)) %>% 
  mutate(datetime = round_date(datetime, "5 min")) %>% 
  mutate(plot = "FW")

sw <- bind_rows(import_4channel(sw1_filepath), 
                import_4channel(sw2_filepath)) %>% 
  mutate(datetime = round_date(datetime, "5 min")) %>% 
  mutate(plot = "SW")

fwsw <- bind_rows(fw, sw) %>% 
  #mutate(depth_reorder = fct_reorder(depth, c("30", "20", "10", "5"))) %>% 
  filter(datetime > as_datetime("2023-06-04 18:00:00", tz = common_tz)) %>% 
  mutate(do_percent_sat = ifelse(do_percent_sat < 0, 0, do_percent_sat)) %>% 
  mutate(datetime_raw = as.character(datetime_raw))

write_csv(fwsw %>% 
            mutate(datetime = as.character(datetime)), 
          "data/230610_firesting.csv")


ggplot(fwsw, aes(datetime, depth)) + 
  #geom_tile() +
  geom_contour_filled(aes(z = do_percent_sat)) + 
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  scale_y_reverse() + 
  scale_fill_viridis_d(option = "A", direction = -1) + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "", y = "Depth (cm)", fill = "DO (%)")
ggsave("figures/230610_firesting.png", width = 8, height = 6)

data_10cm <- fwsw %>% filter(depth == 10) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(plot, datetime, do_percent_sat)

write_csv(data_10cm, "data/230612_do_10cm_amp.csv")


ggplot(data_10cm, aes(as_datetime(datetime), do_percent_sat, color = plot)) + geom_line()

