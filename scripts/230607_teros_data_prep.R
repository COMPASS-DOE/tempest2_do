## This script brings in TEROS data for the TEMPEST 2 event
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
       parsedate, # parse_date()
       janitor, # clean_names()
       purrr) # map()

## Set ggplot theme
theme_set(theme_bw())


# 2. Pull in data and format ---------------------------------------------------

teros_path <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Current_data"

teros_list <- list.files(teros_path, "Terosdata_5")

## Also need the table to match plot and grid to channel and logger
teros_table <- read_csv("/Users/regi350/Downloads/TEROS_Network_Location copy.csv") %>% 
  clean_names() %>% 
  mutate(data_logger_id = as.character(data_logger_id),
         terosdata_table_channel = as.character(terosdata_table_channel))


read_teros <- function(filename){
  read_delim(file = paste0(teros_path, "/", filename), skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    gather(channel, value, -timestamp, -record, -statname) %>%
    mutate(datetime = force_tz(parsedate::parse_date(timestamp), tz = Sys.timezone())) %>% 
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


teros_raw <- teros_list %>% 
  map(read_teros) %>% 
  bind_rows() 

teros_all <- inner_join(teros_raw, teros_table %>% select(data_logger_id, terosdata_table_channel, plot, grid_square, depth), 
                        by = c("data_logger_id", "terosdata_table_channel")) %>% 
  select(datetime, data_logger_id, plot, grid_square, depth, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  unnest(vwc, tsoil, ec) %>% 
  mutate(vwc = 3.879*10^-4 * vwc - 0.6956) %>% 
  unique()


# 3. Trim and calculate data ---------------------------------------------------

start_date = "2023-06-01"
end_date = "2023-06-10"

teros_trim <- teros_all %>% 
  filter(datetime >= start_date & 
           datetime <= end_date) 


# 4. QC data -------------------------------------------------------------------

check_datasets <- function(var){
  ggplot(teros_trim, aes(datetime, {{var}}, color = interaction(depth, grid_square))) + 
    geom_line() + 
    facet_wrap(~plot, ncol = 1, scales = "free_y")
}

check_datasets(vwc)




#write_csv(df_cor, "data/teros_data.csv")
