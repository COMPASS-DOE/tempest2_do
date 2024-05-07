## Pull in L1 Sapflow, correct, and analyze if we see responses in sapflow to 
## 
## 2024-04-18
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")

library(plotly)


# 2. Pull in L1 data -----------------------------------------------------------

l1_file_list <- drive_ls("https://drive.google.com/drive/folders/1HNsS2ACiZ_efOPJGtGzr5IxGBjBcsOPb", 
                      pattern = ".csv")

raw_data_path <- "data/l1_raw_files/"

drive_download_ <- function(data){
  #message(paste("Downloading", data$name))
  # you could add an ifelse to only download files it doesn't fine in raw_data_path
  drive_download(data$id, overwrite = T, path = paste0(raw_data_path, data$name))
}

## Use a for-loop to read in files in a way that I can see what's going on
## Download data to local. I tried to map() but for some reasons it doesn't work?
for(i in 1:nrow(l1_file_list)){
  drive_download_(l1_file_list %>% slice(i))
}


# 3. Read and format L1 data ---------------------------------------------------


## To do: pull in voltage and rain as well then pivot_wider()
read_sapflow <- function(name){
  read_csv(paste0(raw_data_path, name)) %>% 
  clean_names() %>% 
  filter(grepl("sapflow", research_name)) %>% 
  mutate(datetime_est = force_tz(timestamp, tzone = common_tz), 
         plot = case_when(plot == "C" ~ "Control", 
                          plot == "S" ~ "Estuarine", 
                          plot == "F" ~ "Freshwater")) %>% 
    #filter(f_oob == 0) %>% 
  dplyr::select(timestamp, datetime_est, plot, location, sensor_id, value, contains("f_")) %>% 
  rename("sapflow_mv" = value)
}

##Bind data together and filter out the data we definitely won't need.
sapflow_raw <- l1_file_list$name %>% 
  map(read_sapflow) %>% 
  bind_rows() %>%
  filter(datetime_est > "2023-05-01" &
           datetime_est < "2023-08-01")

## Plotly plot to look at individual sensors
p <- ggplot(sapflow_raw,aes(datetime_est, sapflow_mv, color = sensor_id)) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

 ggplotly(p)


# 4. Cleaning ------------------------------------------------------------------

## Based on the plot above, there are sensors that periodically drop to 0. We
## can use the lack of values between 0 and 0.2 to set a threshold of 0.1. Let's
## ID which sensors have issues.
bad_threshold = 0.1

bad_sensors <- sapflow_raw %>%
  filter(sapflow_mv < bad_threshold) %>%
  group_by(sensor_id) %>%
  summarize(sensor_id = first(sensor_id))

sensors_to_remove <- bind_rows(sapflow_raw %>%
                                 group_by(sensor_id) %>%
                                 summarize(sensor_id = first(sensor_id)) %>% 
                                 filter(grepl("D", sensor_id)), 
                               bad_sensors) %>% 
  unique() %>% pull()

sapflow_qc1 <- sapflow_raw %>% 
  filter(!(sensor_id %in% sensors_to_remove))

# See https://github.com/COMPASS-DOE/tempest-system-level-analysis/blob/main/scripts/sapflow.Rmd#L87
# Correct to Fd for now
x <- sapflow_raw %>% 
  filter(plot == "Freshwater") %>% 
  filter(datetime_est > event_start &
           datetime_est < event_end) %>% 
  filter(sapflow_mv > bad_threshold) %>% 
  mutate(date = date(datetime_est))

x %>% 
  ggplot(aes(datetime_est, sapflow_mv, color = sensor_id)) + 
  geom_line() + 
  geom_vline(aes(xintercept = dump_start1)) + 
  facet_wrap(~sensor_id)
 

## Example

sapflow_dtmax <- x %>% 
  mutate(hour = hour(datetime_est)) %>% 
  mutate(date = date(datetime_est)) %>% 
  filter(hour >= 0, hour <= 5) %>%  # Originally 0-5, 0-7 works
  group_by(date, plot, sensor_id) %>% 
  summarise(dTmax = max(sapflow_mv, na.rm = TRUE), dTmax_datetime = datetime_est[which.max(sapflow_mv)])

x %>% 
  #filter(plot == "Control") %>% 
  ggplot(aes(x = datetime_est, y = sapflow_mv, group = sensor_id, color= as.factor(sensor_id))) + 
  geom_line() + 
  geom_point(data = filter(sapflow_dtmax, plot == "Freshwater"), aes(x = dTmax_datetime, y = dTmax), color = "black") + 
  facet_wrap(~sensor_id, scales = "free") 

## Remove any sensors that have trends. SP will confirm wth NM
## Automate removing sensors with "high" slopes wtf that is

## This is not Js, this is sap flux density (see L125)
sfd_data <- x %>% 
  left_join(sapflow_dtmax, by = c("date", "plot", "sensor_id")) %>% 
  mutate(Fd = 360000 * (0.00011899) * (((dTmax / sapflow_mv) - 1)^1.231))

## Use this to look at sensors, and throw out weirdos. F10 and F9 for starters
ggplotly(sfd_data %>% 
  #filter(sensor_id == "F19D") %>% 
ggplot(aes(x = datetime_est, y = (Fd), color = sensor_id)) + 
  #annotate("rect", xmin=EVENT_START, xmax=EVENT_STOP, ymin= -Inf, ymax=Inf, alpha=0.6, fill="lightblue") +
  geom_line() +
  facet_wrap(~sensor_id) + 
  ylab(expression(paste("Sap Flux Density (", F[d], ", g \u2022 ", cm^{-2}, " \u2022 ", hr^{-1}, ")"))) +
  theme(legend.position="bottom"))


y <- sfd_data %>% 
  filter(sensor_id == "F12") %>% 
  select(datetime_est, sapflow_mv, Fd) %>% 
  ungroup() %>% 
  left_join(sapflow_dtmax %>% 
              ungroup() %>% 
              filter(sensor_id == "F12") %>% 
              rename("max_sapflow" = dTmax) %>% 
              dplyr::select(dTmax_datetime, max_sapflow), 
            by = c("datetime_est" = "dTmax_datetime"))

midnights <- y$datetime_est[format(y$datetime_est, "%H:%M:%S") == "00:00:00"]

plot_grid(ggplot(y, aes(datetime_est, sapflow_mv)) + 
            geom_vline(xintercept = midnights, linetype = "dashed", color = "gray") +
            geom_line() + 
            geom_point(data = y %>% filter(!is.na(max_sapflow)), color = "black"), 
          ggplot(y, aes(datetime_est, Fd)) + 
            geom_vline(xintercept = midnights, linetype = "dashed", color = "gray") + 
            geom_line() + 
            geom_point(data = y %>% filter(!is.na(max_sapflow)), color = "black"), 
          ncol = 1)
ggsave("figures/240430_sapflow_issue_for_nm.png", width = 6, height = 8)



