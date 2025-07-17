## Script for Kent for reading in and organizing 4-channel Firesting datasets.
## Please note that you'll need to edit filepaths to map to your local path.
## Because this code is built for my environment, this is equivalent to dummy
## code, though the function at the core of the script should work regardless.
##
## 2023-07-07
## Peter Regier


# 1. Setup ---------------------------------------------------------------------

## Firesting general paths - WILL NEED TO CHANGE TO YOUR LOCAL PATH
four_channel_path <- "data/firesting/four_channel/"


# 2. Read in 4-channel data ----------------------------------------------------

## Set paths of individual runs - WILL NEED TO CHANGE TO YOUR FOLDER NAMES
fw1_filepath <- paste0(four_channel_path, "2023-06-04_082732_230604_fw_tempest_1/ChannelData")
fw2_filepath <- paste0(four_channel_path, "2023-06-05_100032_230605_fw_tempest_2/ChannelData")
fw3_filepath <- paste0(four_channel_path, "2023-06-08_081002_230605_fw_tempest_3/ChannelData")


## Set raw firesting data column names - IF YOU'RE USING A UNIT OTHER THAN % SAT
## YOU'LL NEED TO CHANGE THIS. Look at the raw data to see what your column names 
## should be.
four_channel_names <- c("date", "time", "dt_s", "do_percent_sat", "dphi", 
                        "intensity_mv", "light_mv", "do_status", "temp_date", "temp_time", 
                        "temp_dt_s", "temp_c", "temp_status", "press_date", "press_time", 
                        "press_dt_s", "pressure_mbar", "press_status")

import_4channel <- function(path){
  
  import_firesting <- function(filepath) {
    ## Read in the 4 O2 channels and temperature
    read_delim(filepath, delim = "\t", skip = 25,  col_names = F) %>% 
      magrittr::set_colnames(four_channel_names) %>% 
      mutate(datetime_raw = paste(date, time)) %>% 
      mutate(datetime = lubridate::as_datetime(datetime_raw, format = "%d-%m-%Y %H:%M:%S", 
                                               tz = common_tz)) %>% 
      dplyr::select(datetime_raw, datetime, temp_c, do_percent_sat)
  }
  
  ## WILL NEED TO CHANGE DEPTH TO WHATEVER YOUR VARIABLE REPRESENTED BY EACH CHANNEL IS
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
