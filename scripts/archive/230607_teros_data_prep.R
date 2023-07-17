## This script brings in TEROS data for the TEMPEST 2 event. Because there is a 
## lot of heterogeneity between TEROS nests, we're going to target our dataset to
## just the nest closest to the DO/redox sensors in each plot. If that fails, we'll
## broaden the spatial net slightly to include the closest 2-3 nests until we are 
## confident we have high-quality data representing local soil conditions
##
## Instead of pulling off grid-cells, we can use logger numbers to specify which
## data we want to pull:
## FW: PNNL_21
## SW: PNNL_32
## Control: PNNL_12
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

## Set paths for current and archived loggernet data (will replace with strings once 
## all relevant data are in archive)
teros_path_current <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Current_data"
teros_path_archive <- "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive"

## Create the list of files to read
teros_list_all <- c(list.files(teros_path_archive, "Terosdata", full.names = T)[grepl("202306", list.files(teros_path_archive, "Terosdata"))], 
  list.files(teros_path_current, "Terosdata", full.names = T))

## Trim to nearest loggers only, based on
## https://stackoverflow.com/questions/7597559/grep-using-a-character-vector-with-multiple-patterns
loggers_to_match <- c("PNNL_21", "PNNL_32", "PNNL_12")
teros_list_single_loggers <- unique(grep(paste(loggers_to_match, collapse = "|"), 
                                         teros_list_all, value = TRUE))

teros_list <- teros_list_single_loggers[!grepl("_5min", teros_list_single_loggers)]

## Also need the table to match plot and grid to channel and logger
teros_table <- read_csv("/Users/regi350/Downloads/TEROS_Network_Location copy.csv") %>% 
  clean_names() %>% 
  mutate(data_logger_id = as.character(data_logger_id),
         terosdata_table_channel = as.character(terosdata_table_channel))

## Set up a function to read in data
read_teros <- function(filename){
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

## Start a parallel structure
plan(multisession, workers = 10)

## Read in the data and bind to a single dataframe
tic("read in teros data")
teros_raw <- teros_list %>% 
  future_map(read_teros, .progress = F) %>% # false because it's not working
  bind_rows() 
toc()

## Join data with metadata and do initial corrections and QC
teros_all <- inner_join(teros_raw, teros_table %>% select(data_logger_id, terosdata_table_channel, plot, grid_square, depth), 
                        by = c("data_logger_id", "terosdata_table_channel")) %>% 
  select(datetime, data_logger_id, plot, grid_square, depth, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  unnest(vwc, tsoil, ec) %>% 
  mutate(vwc = 3.879*10^-4 * vwc - 0.6956) %>% 
  mutate(sensor_id = paste0(grid_square, "_", depth, "cm")) %>% 
  filter(vwc >= 0 & ec >= 0 & tsoil >= 0) %>% 
  unique()


# 3. Trim and clean data -------------------------------------------------------

### 1. Trim
## Set start and end dates based on setup.R
start_date = pre_event_start
end_date = post_event_end

## Trim data
teros_trim <- teros_all %>% 
  filter(datetime >= start_date & 
            datetime <= end_date) 
    #%>% 
  # filter(plot == "Control" & grid_square == "C6" | 
  #          plot == "Freshwater" & grid_square == "F4" |
  #          plot == "Seawater" & grid_square == "F6")



### 2. Clean non-responsive sensors 

## There's some issue with spuriously high and constant (ie not responding to the
## environment) sensors. Based on plotting the data, we established a cutoff for 
## sensors with a high mean VWC and low sd, indicative of erroneous behavior. 
## Similar behavior wasn't observed in either FW or SW
ggplot(teros_trim, aes(datetime, vwc, color = sensor_id)) + 
  geom_line() + 
  facet_wrap(~plot)

## Calculate means and standard deviations by sensors
control_sensors <- teros_trim %>% 
  filter(plot == "Control") %>% 
  group_by(sensor_id) %>% 
  summarize(mean_vwc = mean(vwc), 
            sd_vwc = sd(vwc))

## Based on this plot, we see that there are 9 offenders with means > 0.5 and 
## sd << 0.01. 
plot_grid(ggplot(control_sensors, aes(sensor_id, mean_vwc)) + 
            geom_col(), 
          ggplot(control_sensors, aes(sensor_id, sd_vwc)) + 
            geom_col(), 
          ncol = 1)

## List the sensors to scrub
to_scrub <- control_sensors %>% 
  filter(mean_vwc > 0.5 & sd_vwc < 0.001)

## Scrub the sensors not reporting real data
teros_scrubbed1 <- teros_trim %>% 
  filter(!(sensor_id %in% to_scrub$sensor_id)) %>% 
  drop_na()

## Double-check that things look good
check_plot <- function(data, var){
  ggplot(data, aes(datetime, {{var}}, color = sensor_id)) + 
    geom_line() + 
    facet_wrap(~plot)
}

## These plots look good, except there are a number of sensors that are only 
## online for part of the experimental window. Let's trim the dataset to a window
## and see how many sensors are fully online for the pre-exp-post time-period
plot_grid(check_plot(teros_scrubbed1, vwc), 
          check_plot(teros_scrubbed1, ec), 
          ncol = 1)

### 3. Scrub sensors with incomplete records

## First, calculate the counts for each sensor
sensor_counts <- teros_scrubbed1 %>%
  group_by(sensor_id, plot) %>% 
  count() 

ggplot(sensor_counts, aes(sensor_id, n)) + 
  geom_col() + 
  facet_wrap(~plot, ncol = 1)

## Let's scrub any sensor with less than 1000 values
#sensors_to_keep <- sensor_counts %>% filter(n > 1000) %>% pull(sensor_id)

## This seems unnecessary, and currently is, leaving in case I need to try and 
## pull 5-minute data in, which had some issues
teros_scrubbed2 <- teros_scrubbed1 #%>% 
  #filter(sensor_id %in% sensors_to_keep)

plot_grid(check_plot(teros_scrubbed2, vwc), 
          check_plot(teros_scrubbed2, ec), 
          ncol = 1)

teros_scrubbed2 %>% 
  group_by(plot, depth) %>% 
  summarize(n_distinct(sensor_id))




# 4. Calculate means and standard deviations for cleaned data ------------------

teros_medians <- teros_scrubbed2 %>% 
  group_by(datetime, depth, plot) %>% 
  summarize(vwc = median_(vwc), 
            tsoil = median_(tsoil), 
            ec = median_(ec)) %>% 
  drop_na()

make_teros_contour <- function(var, y_label){
  ggplot(teros_medians, aes(datetime, depth)) + 
    geom_contour_filled(aes(z = {{var}}), bins = 10) + 
    scale_y_reverse() + 
    annotate("rect", xmin = dump_start1, xmax = dump_start1 + hours(10), 
             ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = dump_start2, xmax = dump_start2 + hours(10), 
             ymin = 5, ymax = 30, fill = "white", alpha = 0.2) +
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    labs(x = "", y = "Depth (cm)", fill = y_label)
}

make_teros_contour(tsoil, "Temp (C)")
ggsave("figures/230609_teros_temp_contours.png", width = 9, height = 8) 


make_teros_contour(ec, "EC (uS/cm)")
ggsave("figures/230609_teros_ec_contours.png", width = 9, height = 8) 


make_teros_contour(vwc, "VWC (m3/m3)")
ggsave("figures/230609_teros_vwc_contours.png", width = 9, height = 8) 

plot_grid(make_teros_contour(vwc, "VWC (m3/m3)"), 
          make_teros_contour(ec, "EC (uS/cm)"), 
          nrow = 1)
ggsave("figures/230609_teros_vwc_ec_contours.png", width = 12, height = 8) 


# ggplot(teros_medians, aes(datetime, depth)) + 
#   geom_contour_filled(aes(z = ec)) + 
#   scale_y_reverse() + 
#   geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
#   geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
#   facet_wrap(~plot, ncol = 1) + 
#   labs(x = "", y = "Depth (cm)", fill = )

write_csv(teros_medians, "data/230610_teros_medians.csv")



