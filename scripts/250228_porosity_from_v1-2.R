## This script pulls in v1-2 data from TEMPEST for 2023 for VWC/air temp/rain 
## to explore potential proxy for soil porosity

source("scripts/0_0_setup.R")

p_load(tictoc, 
       plotly)

theme_set(theme_bw())

## Pull VWC for TEMPEST 2021
tempest21_source = "https://drive.google.com/drive/folders/1BPjE12PsBTnqWs8xD85elbNlCkPEITxU"
tempest21_destination = "data/ignore/v1-2/TMP_2021/"

tempest21_files <- drive_ls(tempest21_source, pattern = ".csv")

# tic("pull TEMPEST files from GDrive") #200-300s
# for(i in 1:nrow(tempest21_files)){
#   drive_download(tempest21_files$name[i],
#                  path = paste0(tempest21_destination, tempest21_files$name[i]),
#                  overwrite = T)
# }
# toc()


## Pull VWC for TEMPEST 2023
tempest23_source = "https://drive.google.com/drive/folders/1ohykyRSfFHdd5eMNmg8miEOreITUpnbc"
tempest23_destination = "data/ignore/v1-2/TMP_2023/"

tempest23_files <- drive_ls(tempest23_source, pattern = ".csv")

# tic("pull TEMPEST files from GDrive") #200-300s
# for(i in 1:nrow(tempest23_files)){
#   drive_download(tempest23_files$name[i],
#                  path = paste0(tempest23_destination, tempest23_files$name[i]),
#                  overwrite = T)
# }
# toc()

## Pull Air Temp / Rain for GCW 2023
gcw_source = "https://drive.google.com/drive/folders/1VqG6HA5j2RRMFHkI_QkIqoLD8sC5guKe"
gcw_destination = "data/ignore/v1-2/GCW/"

gcw_files <- drive_ls(gcw_source, pattern = ".csv") %>% 
  filter(grepl("GCW_W", name))

# tic("pull GCW files from GDrive") #200-300s
# for(i in 1:nrow(gcw_files)){
#   drive_download(gcw_files$name[i],
#                  path = paste0(gcw_destination, gcw_files$name[i]),
#                  overwrite = T)
# }
# toc()


## Pull in and format VWC data -------------------------------------------------

## Need to set up a function that pulls the plot from the filename

read_teros21 <- function(file){
  
  message(file)
  
  site = substr(file, 5, 5)
  
  read_csv(paste0(tempest21_destination, file)) %>% 
    clean_names() %>% 
    filter(research_name == "soil_vwc_15cm") %>% 
    filter(f_oob == 0) %>% 
    filter(f_oos == 0) %>% 
    group_by(timestamp) %>% 
    summarize(value = mean(value, na.rm = T)) %>% 
    mutate(site = site)
  
}

read_teros23 <- function(file){
  
  message(file)
  
  site = substr(file, 5, 5)
  
  read_csv(paste0(tempest23_destination, file)) %>% 
    clean_names() %>% 
    filter(research_name == "soil_vwc_15cm") %>% 
    filter(f_oob == 0) %>% 
    filter(f_oos == 0) %>% 
    group_by(timestamp) %>% 
    summarize(value = mean(value, na.rm = T)) %>% 
    mutate(site = site)
  
}

teros_21 <- tempest21_files$name %>% 
  map(read_teros21) %>% 
  bind_rows()

teros_23 <- tempest23_files$name %>% 
  map(read_teros23) %>% 
  bind_rows()

teros_raw <- bind_rows(teros_21, teros_23)

teros_raw %>% 
  ggplot(aes(timestamp, value, color = site)) + 
  geom_line()

teros_bin <- teros_raw %>% 
  ungroup() %>% 
  mutate(datetime = round_date(timestamp, "15 min")) %>% 
  group_by(datetime, site) %>% 
  summarize(vwc = mean(value, na.rm = T))

teros_bin23 <- teros_bin %>% 
  filter(datetime > "2022-12-30")

teros_bin23 %>% 
  ggplot(aes(datetime, vwc, color = site)) + 
  geom_line()

## Cleanup
rm(teros_raw)

## Now, pull in air temp and rain

read_gcw <- function(file){
  
  message(file)
  
  read_csv(paste0(gcw_destination, file)) %>% 
    clean_names() %>% 
    filter(research_name %in% c("wx_gcrew_rain15", "wx_tempavg15")) %>% 
    filter(f_oob == 0) %>% 
    filter(f_oos == 0) %>% 
    select(timestamp, research_name, value) %>% 
    pivot_wider(names_from = "research_name", values_from = "value") %>% 
    rename("rain" = wx_gcrew_rain15, 
           "air_temp" = wx_tempavg15)
}

gcw_raw <- gcw_files$name %>% 
  map(read_gcw) %>% 
  bind_rows() %>% 
  rename("datetime" = timestamp)

plot_grid(ggplot(gcw_raw, aes(timestamp, air_temp)) + 
            geom_line(), 
          ggplot(gcw_raw, aes(timestamp, rain)) + 
            geom_line(), 
          ncol = 1)


plot_grid(teros_bin23 %>% 
            mutate(month = as.factor(month(datetime))) %>% 
            ggplot(aes(month, vwc, fill = site)) + geom_boxplot(),
          gcw_raw %>% 
            mutate(month = as.factor(month(datetime))) %>% 
            ggplot(aes(month, air_temp)) + geom_boxplot(), 
          gcw_raw %>% 
            mutate(month = as.factor(month(datetime))) %>% 
            ggplot(aes(month, rain)) + geom_boxplot(), 
          ncol = 1)

## Let's isolate some rain events of similar magnitudes before and 
## after the event

period_start = "2023-04-01"
period_end = "2023-09-01"

p1 <- teros_bin23 %>% 
  filter(datetime > period_start) %>% 
  filter(datetime < period_end) %>% 
  ggplot(aes(datetime, vwc, color = site)) + geom_line()

p2 <- gcw_raw %>% 
  filter(datetime > period_start) %>% 
  filter(datetime < period_end) %>% 
  ggplot(aes(datetime, rain)) + geom_line()
  
plot_grid(p2, p1, ncol = 1, rel_heights = c(0.5, 1))

df <- left_join(teros_bin23, gcw_raw, by = "datetime") %>% 
  mutate(rain_clean = ifelse(site == "C", rain, NA)) %>% 
  mutate(temp_clean = ifelse(site == "C", air_temp, NA))

make_plot <- function(data){
  p1 <- data %>% drop_na() %>% 
    ggplot(aes(datetime, rain_clean)) + 
    geom_line() + 
    ylim(0, 1.3)
  p2 <- ggplot(data, aes(datetime, vwc, color = site)) + 
    geom_line()  + 
    ylim(0.27, 0.41)
  
  plot_grid(p1, p2, ncol = 1, rel_heights = c(0.5, 1))
}

t1 <- df %>% 
  filter(datetime > "2023-04-14" & 
           datetime < "2023-04-16")

t2 <- df %>% 
  filter(datetime > "2023-07-23" & 
           datetime < "2023-07-27")

plot_grid(make_plot(t1), 
          make_plot(t2), 
          nrow = 1)








