## This script is a stripped-down verson of 230607_teros_data_prep.R which tried
## to fully clean and bin the dataset to a single usable version. Unfortunately,
## these data are going to be a pain to clean, and will require justification at
## each step, so this new script is going to just bring in data, sort by 
## temporal resolution (5-min or 15-min), and export two datasets. We'll then
## use a subsequent RMD to both clean and document our choices along the way.
##
## 2023-07-10
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


# 2. Pull in data and format ---------------------------------------------------

## Set paths for current and archived loggernet data (will replace with strings once 
## all relevant data are in archive)
teros_path_current <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Current_data"
teros_path_archive <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive"

## Create the list of files to read
teros_list_all <- list.files(teros_path_archive, "Terosdata", full.names = T)[grepl("202306|202307", list.files(teros_path_archive, "Terosdata"))]

## Set up a function to read in data
read_teros <- function(filename){
  
  message(paste("Reading", filename))
  
  read_delim(file = filename, skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    gather(channel, value, -timestamp, -record, -statname) %>%
    mutate(datetime = force_tz(parsedate::parse_date(timestamp), tz = common_tz)) %>% 
    separate(statname, into = c("inst", "data_logger_id"), sep = "_") %>% 
    select(-inst, -timestamp) %>%
    mutate(channel = str_replace(channel, "teros_", "")) %>% 
    separate(channel, into = c("terosdata_table_channel", "variable"), sep = "_") %>% 
    mutate(variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
           # Give them sensible names
           variable = case_when(variable == 1 ~ "vwc",
                                variable == 2 ~ "tsoil",
                                variable == 3 ~ "ec"), 
           value = as.numeric(value)) 
}

## Also need the table to match plot and grid to channel and logger
teros_table <- read_csv("/Users/regi350/Downloads/TEROS_Network_Location copy.csv") %>% 
  clean_names() %>% 
  mutate(data_logger_id = as.character(data_logger_id),
         terosdata_table_channel = as.character(terosdata_table_channel))

## We now need to split our files by resolution
teros_list_5min <- teros_list_all[grepl("_5min_", teros_list_all)]
teros_list_15min <- teros_list_all[!(grepl("_5min_", teros_list_all))]

## Start a parallel structure
plan(multisession, workers = 10)

## Read in 5-min data and bind to a single dataframe
tic("read in 5-minute teros data") ## ~151s
teros_5min_raw <- teros_list_5min %>% 
  future_map(read_teros, .progress = F) %>% # false because it's not working
  bind_rows() 
toc()

## Read in 15-min data and bind to a single dataframe
tic("read in 15-minute teros data") ## ~49s
teros_15min_raw <- teros_list_15min %>% 
  future_map(read_teros, .progress = F) %>% # false because it's not working
  bind_rows() 
toc()

clean_teros <- function(data){
  
  ## Set start and end dates based on setup.R
  start_date = pre_event_start
  end_date = post_event_end
  
  inner_join(data, teros_table %>% select(data_logger_id, terosdata_table_channel, plot, grid_square, depth), 
             by = c("data_logger_id", "terosdata_table_channel")) %>% 
    select(datetime, data_logger_id, plot, grid_square, depth, variable, value) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    unnest(vwc, tsoil, ec) %>% 
    mutate(vwc = 3.879*10^-4 * vwc - 0.6956) %>% 
    mutate(sensor_id = paste0(grid_square, "_", depth, "cm")) %>% 
    filter(vwc >= 0 & ec >= 0 & tsoil >= 0) %>% 
    filter(datetime >= start_date & 
             datetime <= end_date) 
}

## Clean both dataframes
clean_5min <- clean_teros(teros_5min_raw) %>% arrange(plot, datetime)
clean_15min <- clean_teros(teros_15min_raw) %>% arrange(plot, datetime)

## Write out datasets
write_csv(clean_5min, "data/230710_teros_raw_5min.csv")
write_csv(clean_15min, "data/230710_teros_raw_15min.csv")




