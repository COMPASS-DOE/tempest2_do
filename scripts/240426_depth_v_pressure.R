


## Load packages
require(pacman)
p_load(cowplot, 
       googledrive, 
       googlesheets4,
       gsw,
       tidyverse, 
       janitor,
       parsedate,
       plotly,
       hms,
       lubridate)

theme_set(theme_bw())

## Authorize access to GDrive
#googledrive::drive_auth()

## Set the GDrive folder to find files
current_directory = "https://drive.google.com/drive/folders/1-1nAeF2hTlCNvg_TNbJC0t6QBanLuk6g"
archive_directory = "https://drive.google.com/drive/folders/16zmJrgSm7OHBn-4bDWvvO9E8NeX92nrl"

raw_data_path <- "data/raw_from_gdrive/synoptic_troll/"

options(gargle_oauth_email = "peter.regier@pnnl.gov")

files_raw <- bind_rows(drive_ls(current_directory) %>% 
                         filter(grepl("WaterLevel", name)), 
                       drive_ls(archive_directory) %>%
                         filter(grepl("WaterLevel", name)))


## This function reads in and cleans up each file
read_data <- function(data){
  
  site <- str_split(data, "_", simplify = T)[,5]
  location <- str_split(data, "_", simplify = T)[,6]
  
  read_delim(file = data, skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    mutate(datetime = parsedate::parse_date(timestamp)) %>% 
    filter(datetime > "2022-03-01") %>% 
    mutate_at(vars(contains("600")), as.numeric) %>% 
    rename_with(~str_remove(., '600[a-z]')) %>% 
    rename("pressure_psi" = pressure) %>% 
    mutate(pressure_mbar = pressure_psi * 68.948) %>% 
    #separate(statname, c("project", "site", "location")) %>% 
    mutate(site = site, 
           location = location) %>% 
    select(datetime, site, location, temperature, salinity, rdo_concen, p_h, p_h_orp,
           depth, water_density, pressure_mbar, pressure_psi, voltage_ext, battery_int)
}

## Read in data and bind to a single dataframe
df_raw <- list.files(raw_data_path, full.names = T) %>% 
  map(read_data) %>% 
  bind_rows()



