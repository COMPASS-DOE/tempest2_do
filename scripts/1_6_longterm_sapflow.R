## This script prepares and analyzes as much sapflow data from CY23 as possible
## to explore if we have a useful data-based demonstration of acute disturbance
## manifesting chronic disturbance. Code and data originally provided by 
## Kendalynn Morris with some edits by Peter Regier
##
## Peter Regier (Original code/data from Kendal)
## 2024-01-09 (Updated 2025-05-02)
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")
source("data/from_kendal/create_inventory.R")
p_load(broom, multcomp, zoo, ggallin)


# 2. Read in sapflow -----------------------------------------------------------

list_tmp_files <- function(folder){
  list.files(paste0("data/l1_raw_files/TMP/", folder), 
             pattern = ".csv", 
             recursive = TRUE, 
             full.names = TRUE)}

#folders <- c("TMP_2021", "TMP_2022", "TMP_2023")
folders <- c("TMP_2023")

all_files <- unlist(lapply(folders, list_tmp_files))

read_in_sapflow <- function(f) {
  
  message("Reading ", basename(f))
  
  read_csv(f, col_types = "ccTccccdccii") %>% 
    clean_names() %>% 
    filter(research_name == "sapflow_2.5cm")}

#Correction for F19 being mislabeled as F19D in L1 data
sapflow_raw <- all_files %>% 
  map(read_in_sapflow) %>% 
  bind_rows() %>% 
  drop_na(sensor_id) %>%
  mutate(sensor_id = ifelse(sensor_id == "F19D", "F19", sensor_id)) %>% 
  dplyr::select(-c(instrument, id))

#First, isolate sapflow data
sapflow_v0 <- sapflow_raw %>%
  mutate(plot = case_when(plot == "C" ~ "Control",
                          plot == "F" ~ "Freshwater",
                          plot == "S" ~ "Saltwater")) %>% 
  dplyr::select(plot, timestamp, sensor_id, value) %>%
  rename("sapflow_2.5cm" = value) %>% 
  mutate(date = date(timestamp))


# 2. Add species info ----------------------------------------------------------

#Combining it all: editing dataframes for variables to match 
sapflow_inventory <- read_csv("data/from_kendal/sapflow_inventory.csv") %>% 
  clean_names()

species <- sapflow_inventory %>%
  mutate(species = substr(spp,1,4),
         species = case_when(spp == "ACRU" ~ "Red Maple",
                             spp == "LITU" ~ "Tulip Poplar",
                             spp == "FAGR" ~ "Beech")) %>%
  dplyr::select(plot, sapflux_id, species) %>% 
  mutate(plot = case_when(plot == "FW" ~ "Freshwater",
                          plot == "SW" ~ "Saltwater", 
                          TRUE ~ plot))


# 3. Join sapflow and species --------------------------------------------------

## Join sapflow with species info
sapflow_sp <- full_join(sapflow_v0, 
                        species, 
                        by = c("sensor_id" = "sapflux_id", 
                               "plot" = "plot"))


# 4. Calculate dTmax -----------------------------------------------------------

## We made several decisions as a council of COMPASS sapflow users: 
## https://docs.google.com/document/d/1AABnMlggFASJJyjSllISeX67ECYr8hLQt7vpId9ZrAU/edit?tab=t.0
### 1. night is defined as Hour >= 0, Hour <= 4
### 2. midday is defined as Hour >= 11, Hour =< 14
### 3. matching KAM, using a filter of F < 0.005

#Calculate dTmax
sapflow_dtmax <- sapflow_sp %>% 
  drop_na(timestamp) %>% # There are 51 rows with NAs for datetime
  mutate(hour = hour(timestamp)) %>% 
  filter(hour >= 0 & hour <= 4) %>%  ## Decision made by consensus
  group_by(date, plot, species, sensor_id) %>% 
  summarize(dTmax = max(sapflow_2.5cm, na.rm = TRUE), 
            dTmax_time = timestamp[which.max(sapflow_2.5cm)])

#Calculate Fd
# convert the probe raw values (in mV) to sap flux velocity (cm/cm^2/s)
# Granier equation is Fd = (k * (deltaTmax - deltaT))^1.231
# k = 0.011899
sfd_data <- sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("plot", "species", "sensor_id", "date")) %>% 
  mutate(F = ((0.011899 * (((dTmax / sapflow_2.5cm) - 1)))^1.231))


# 5. Add in tree dimensional information ---------------------------------------

#Combining it all: editing dataframes for variables to match 
tree_dat <- as_tibble(readRDS("data/from_kendal/dbh.rds"))

## Diameter at breast height
dbh <- tree_dat %>%
  dplyr::select(Tree_ID, Sapflux_ID, spp,
                DBH_2024, DBH_2023, DBH_2022, DBH_2021)

#Using allometric equations, scale Fd measurements
#DBH measurements are in cm 
SA <- function(Species, DBH) {
  case_when(
    Species == "Red Maple" ~ (0.5973*(DBH)^2.0743),
    Species == "Tulip Poplar" ~ (0.8086*(DBH)^1.8331),
    Species == "Beech" ~ (0.8198*(DBH)^1.8635))
}

## Surface area
sa <- dbh %>%
  mutate(Species = spp) %>%
  mutate(Species = substr(Species,1,4),
         Species = case_when(Species == "ACRU" ~ "Red Maple",
                             Species == "LITU" ~ "Tulip Poplar",
                             Species == "FAGR" ~ "Beech")) %>%
  mutate(across(starts_with("DBH_"), ~SA(Species, .), .names = "SA_{str_extract(.col, '[0-9]{4}')}"))

sa_long <- sa %>% 
  pivot_longer(cols = starts_with("SA_"),
               names_to = "Year",
               names_prefix = "SA_",
               values_to = "SA") %>%
  mutate(Year = as.numeric(Year)) 


# 6. Scale sap flux to surface area --------------------------------------------

## Scaled data
scaled <- merge(sfd_data %>% mutate(year = year(timestamp)), 
                sa_long, by.x = c("sensor_id", "year", "species"), 
                by.y = c("Sapflux_ID", "Year", "Species"), all.x = TRUE) %>% 
  as_tibble()

#final units are cubic centimeters per second
sf_scaled <- scaled %>%
  dplyr::select(sensor_id, year, species, plot, timestamp, F, SA) %>%
  mutate(Fd = SA * F)


# 7. Clean up / finalize data --------------------------------------------------

sf_plot_avg <- sf_scaled %>% 
  mutate(Hour = hour(timestamp)) %>%
  mutate(Date = date(timestamp)) %>%
  mutate(monthyr = floor_date(timestamp, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 14) %>%  ## Decision made by consensus
  filter(F <= 0.005, F >= 0) %>% ## Based on KAM's threshold
  group_by(plot, species, Date) %>% 
  summarise(F_avg = mean(F * 3600, na.rm = TRUE)) #cm3/s to cm3/hr

ggplot(sf_plot_avg) + 
  geom_point(aes (x = Date, y = F_avg, color = species)) + 
  facet_wrap(~plot, ncol = 1, scales = "fixed") + 
  labs(y = "Avg Sap Flux Density", x = "Date", title = "Sap Flux Density Averaged Daily, 11 AM - 2 PM")

# Add rolling means

roll_length = 10

sf_rollmean <- sf_plot_avg %>% 
  clean_names() %>% 
  ungroup() %>% 
  group_by(plot, species) %>% 
  mutate(f_roll = zoo::rollmean(f_avg, roll_length, fill = NA)) %>% 
  mutate(doy = yday(date),
         year = year(date)) 

sf_rollmean %>%
  ggplot(aes(x = doy, color = plot)) +
  geom_point(aes(y = f_avg), alpha = 0.2) + 
  geom_line(aes(y = f_roll)) + 
  facet_wrap(~species, 
             scales = "free_y", 
             nrow = 3)

write_csv(sf_rollmean, "data/250502_2023_sapflow.csv")


# Optionally, let's clean based on clouds and rain

# X. Read in BP and PAR --------------------------------------------------------

list_gcw_files <- function(folder){
  list.files(paste0("data/l1_raw_files/GCW/", folder), 
             pattern = ".csv", 
             recursive = TRUE, 
             full.names = TRUE)
}

#folders <- c("GCW_2021", "GCW_2022", "GCW_2023")
folders <- c("GCW_2023")

gcw_files <- unlist(lapply(folders, list_gcw_files))

## From the metadata: 
#
# wx_par_den15 (µmol/m2/s): Average photosynthetically active radiation (PAR) 
# flux density over 15 minute period; site specific correction factor applied   
#
# wx_gcrew_rain15 (cm): Total rain over 15 minute period IN CENTIMETERS; from the 
# GCREW met station and only appears at GCW-W

read_in_gcw <- function(f) {
  
  message("Reading ", basename(f))
  
  variables <- c("wx_gcrew_rain15", "wx_par_den15")
  
  read_csv(f) %>%
    clean_names() %>%
    filter(research_name %in% variables) %>% 
    dplyr::select(timestamp, research_name, value) %>% 
    mutate(research_name = case_when(research_name == "wx_par_den15" ~ "par15", 
                                     research_name == "wx_gcrew_rain15" ~ "rain15")) %>% 
    pivot_wider(names_from = "research_name", values_from = "value")
}

par_and_rain <- gcw_files %>% 
  map(read_in_gcw) %>% 
  bind_rows()

par_and_rain_doy <- par_and_rain %>% 
  mutate(date = as_date(timestamp), 
         hour = hour(timestamp)) %>% 
  mutate(par_umol_m2_day = par15 / (60*60*24)) %>% 
  group_by(date) %>% 
  summarize(rain_cm_day = sum(rain15, na.rm = T), 
            par_mean = mean(par15, na.rm = T), 
            par_mean_11to12 = mean(par15[hour >= 11 & hour <= 12], na.rm = TRUE))

## Use the plots below to visually determine what "rainy" and "cloudy" mean
cloudy_threshold = 500
rainy_threshold = 1

ggplot(par_and_rain_doy, aes(date, par_mean_11to12)) + 
  geom_point(color = "gray") + 
  geom_hline(yintercept = cloudy_threshold) + 
  geom_point(data = par_and_rain_doy %>% 
               filter(par_mean_11to12 < cloudy_threshold), 
             color = "red") + 
  ggtitle("Gray = retained, Red = removed")

ggplot(par_and_rain_doy, aes(date, rain_cm_day)) + 
  geom_point(color = "gray") + 
  geom_hline(yintercept = rainy_threshold) + 
  geom_point(data = par_and_rain_doy %>% 
               filter(rain_cm_day > rainy_threshold), 
             color = "red") + 
  ggtitle("Gray = retained, Red = removed")

dates_with_good_weather <- par_and_rain_doy %>% 
  filter(par_mean_11to12 > cloudy_threshold) %>% #PAR is high enough
  filter(rain_cm_day < rainy_threshold) #Less than 1cm of rain


sf_rollmean_clean <- sf_plot_avg %>% 
  clean_names() %>% 
  filter(date %in% dates_with_good_weather$date) %>% 
  ungroup() %>% 
  group_by(plot, species) %>% 
  mutate(f_roll = zoo::rollmean(f_avg, roll_length, fill = NA)) %>% 
  mutate(doy = yday(date),
         year = year(date)) 

write_csv(sf_rollmean_clean, "data/250502_2023_sapflow_cleaned.csv")

