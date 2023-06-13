## This script brings in TEROS data for the TEMPEST 2 event
##
## 2023-06-06
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


# 2. Pull in data and format ---------------------------------------------------

teros_path_current <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Current_data"
teros_path_archive <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive"

teros_list <- c(list.files(teros_path_archive, "Terosdata_5", full.names = T)[grepl("202306", list.files(teros_path_archive, "Terosdata_5"))], 
  list.files(teros_path_current, "Terosdata_5", full.names = T))

## Also need the table to match plot and grid to channel and logger
teros_table <- read_csv("/Users/regi350/Downloads/TEROS_Network_Location copy.csv") %>% 
  clean_names() %>% 
  mutate(data_logger_id = as.character(data_logger_id),
         terosdata_table_channel = as.character(terosdata_table_channel))


read_teros <- function(filename){
  read_delim(file = filename, skip = 1) %>% 
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


# 3. Trim and summarize data ---------------------------------------------------

start_date = "2023-06-01"

teros_trim <- teros_all %>% 
  filter(datetime >= start_date)



teros_means <- teros_trim %>% 
  filter(vwc > 0) %>% 
  group_by(datetime, depth, plot) %>% 
  summarize(vwc = median_(vwc), 
            tsoil = median_(tsoil), 
            ec = median_(ec)) %>% 
  drop_na()


ggplot(teros_means, aes(datetime, depth)) + 
  geom_contour_filled(aes(z = tsoil)) + 
  scale_y_reverse() + 
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "", y = "Depth (cm)", fill = "Temp (C)")
ggsave("figures/230609_teros_temp_contours.png", width = 9, height = 8) 


ggplot(teros_means, aes(datetime, depth)) + 
  geom_contour_filled(aes(z = ec)) + 
  scale_y_reverse() + 
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "", y = "Depth (cm)", fill = "EC (uS/cm)")
ggsave("figures/230609_teros_ec_contours.png", width = 9, height = 8) 


ggplot(teros_means, aes(datetime, depth)) + 
  geom_contour_filled(aes(z = vwc)) + 
  scale_y_reverse() + 
  geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "", y = "Depth (cm)", fill = "VWC (m3/m3)")
ggsave("figures/230609_teros_vwc_contours.png", width = 9, height = 8) 



write_csv(teros_trim, "data/230610_teros.csv")






