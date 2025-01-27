
# This code compiles a complete time series
# of sapflow and related abiotic data from TEMPEST for 2021-2024
# Sapflow, soil EC & vwc at 15 cm, average air temp, & PAR

# To run, user must download ad unzip sources files from COMPASS-FME Level 1 data
# For sapflow, soil VWC, & EC: doi:10.15485/2479200
# For Tair & PAR: doi:10.15485/2439400

library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

#TEMPEST data from 2021-24
site <- "TMP"
variables <- c("sapflow_2.5cm", "soil_vwc_15cm", "soil_EC_15cm")

pat <- paste0("^", site, ".*csv$")

#Lists of data for different years for TEMPEST
files_T24 <- list.files("data/l1_raw_files/TMP_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)
#files_T23 <- list.files("Data/Unzipped/TMP_2023/", pattern = pat, recursive = TRUE, full.names = TRUE)
#files_T22 <- list.files("Data/Unzipped/TMP_2022/", pattern = pat, recursive = TRUE, full.names = TRUE)
#files_T21 <- list.files("Data/Unzipped/TMP_2021/", pattern = pat, recursive = TRUE, full.names = TRUE)

#files_T <- c(files_T24, files_T23, files_T22, files_T21)

read_in_data <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables | x$Sensor_ID == "F19D",]
  #Unknown is F19D present in current v1-1 data
  #keeping this to be conservative
}

#Bind together all files 
df <- read_in_data(files_T24)
#dat <- do.call("rbind", dat)

tmp_full <- dat

#Correction for F19 being mislabeled as F19D in L1 data
tmp_full %>%
  drop_na(Sensor_ID) %>%
  mutate(Sensor_ID = ifelse(Sensor_ID == "F19D", "F19", Sensor_ID)) -> tmp_full

saveRDS(tmp_full, "tmp_full.rds")
#tmp_full <- readRDS("tmp_full.rds")

#GCREW data from 2021-24
#Note: vappress is all 0 for now until we get that sorted out
#Update: vappress doesn't exist in the ESS-DIVE level 1 data
site <- "GCW"
variables <- c("wx_tempavg15", "wx_par_den15")

pat <- paste0("^", site, ".*csv$")

#Lists of data for different years for GCREW
files_G24 <- list.files("Data/Unzipped/GCW_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G23 <- list.files("Data/Unzipped/GCW_2023/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G22 <- list.files("Data/Unzipped/GCW_2022/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G21 <- list.files("Data/Unzipped/GCW_2021/", pattern = pat, recursive = TRUE, full.names = TRUE)

files_G <- c(files_G24, files_G23, files_G22, files_G21)

f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables,]
}

#Bind together all files 
dat <- lapply(files_G,  f)
dat <- do.call("rbind", dat)

gcw_full <- dat
saveRDS(gcw_full, "gcw_full.rds")
#gcw_full <- readRDS("gcw_full.rds")

#Combining it all: editing dataframes for variables to match 
tree_dat <- readRDS("inventory.rds")

tmp_full %>%
  mutate(Plot = substr(Plot,1,1),
         Plot = case_when(Plot == "C" ~ "Control",
                          Plot == "F" ~ "Freshwater",
                          Plot == "S" ~ "Saltwater", )) -> tmp_full
tree_dat %>%
  mutate(Species = substr(spp,1,4),
         Species = case_when(spp == "ACRU" ~ "Red Maple",
                             spp == "LITU" ~ "Tulip Poplar",
                             spp == "FAGR" ~ "Beech")) %>%
  dplyr::select(Plot, Sapflux_ID, Species) -> species

#Because different variables are at different spatial resolutions, we have to
#separate variables into dataframes then merge again by timestamp

#Create sapflow-only dataframe with scaled Fd

#First, isolate sapflow data
sapflow <- tmp_full %>% 
  filter(Instrument == "Sapflow",
         Value >= 0.01, Value <=0.7) %>%
  dplyr::select(Plot, TIMESTAMP, Sensor_ID, Value) %>%
  mutate(sapflow_2.5cm = Value) %>% 
  mutate(Date = date(TIMESTAMP))

#Merge sapflow and species dataframe
sapflow %>% 
  merge(species, ., by.x = c("Sapflux_ID", "Plot"), by.y = c("Sensor_ID", "Plot"),
        all.x = TRUE, all.y = TRUE) %>%
  mutate(ID = Sapflux_ID) -> sapflow_sp

#Calculate dTmax
sapflow_sp %>% 
  mutate(Date = date(TIMESTAMP)) %>%
  group_by(Date, Plot, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE), 
            dTmax_time = TIMESTAMP[which.max(Value)])-> sapflow_dtmax

#Calculate Fd
# convert the probe raw values (in mV) to sap flux velocity (cm/cm^2/s)
# Granier equation is Fd = (k * (deltaTmax - deltaT))^1.231
# k = 0.011899

sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("Plot", "Species", "ID", "Date")) %>% 
  mutate(Fd = ((0.011899 * (((dTmax / Value) - 1)))^1.231))  -> sfd_data

tree_dat %>%
  dplyr::select(Tree_ID, Sapflux_ID, spp,
                DBH_2024, DBH_2023, DBH_2022, DBH_2021) -> dbh


#Using allometric equations, scale Fd measurements
#DBH measurements are in cm 
SA <- function(Species, DBH) {
  case_when(
    Species == "Red Maple" ~ (0.5973*(DBH)^2.0743),
    Species == "Tulip Poplar" ~ (0.8086*(DBH)^1.8331),
    Species == "Beech" ~ (0.8198*(DBH)^1.8635))
}

dbh %>%
  mutate(Species = spp) %>%
  mutate(Species = substr(Species,1,4),
         Species = case_when(Species == "ACRU" ~ "Red Maple",
                             Species == "LITU" ~ "Tulip Poplar",
                             Species == "FAGR" ~ "Beech")) %>%
  mutate(across(starts_with("DBH_"), ~SA(Species, .), .names = "SA_{str_extract(.col, '[0-9]{4}')}")) -> sa

sa %>% 
  pivot_longer(cols = starts_with("SA_"),
               names_to = "Year",
               names_prefix = "SA_",
               values_to = "SA") %>%
  mutate(Year = as.numeric(Year)) -> sa_long

mutate(sfd_data, Year = year(TIMESTAMP)) -> sfd_data

scaled <- merge(sfd_data, sa_long, by.x = c("ID", "Year", "Species"), 
                by.y = c("Sapflux_ID", "Year", "Species"), all.x = TRUE)

#final units are cubic centimeters per second
scaled %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, Fd, SA) %>%
  mutate(F = SA * Fd) -> sf_scaled

#Now let's make some plots to double check 

sf_scaled %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  mutate(monthyr = floor_date(TIMESTAMP, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(F <= 2, F >= 0) %>%
  group_by(Plot, Species, Date) %>% 
  summarise(F_avg = mean(F, na.rm = TRUE)) -> sf_plot_avg

ggplot(sf_plot_avg) + 
  geom_point(aes (x = Date, y = F_avg, color = Species)) + 
  facet_wrap(~Plot, ncol = 1, scales = "fixed") + 
  labs(y = "Avg Sap Flux Density", x = "Date", title = "Sap Flux Density Averaged Daily, 11 AM - 12 PM")
  
#ggsave("Full_sapflow.jpeg")

#Option to save just the sapflow data as an RDS
#saveRDS(sf_scaled, "Sapflow_21_24.rds")

#Now we add in our abiotic data
  #Create soil vwc dataframe
  #Take average value of all soil vwc measurements in each plot

swc_15 <- tmp_full %>%
    filter(research_name == "soil_vwc_15cm") %>%
    group_by(TIMESTAMP, Plot) %>%
    drop_na(Value) %>%
    summarize(soil_vwc_15cm = mean(Value)) 

tmp_data <- 
  left_join(sf_scaled, swc_15, by = c("Plot", "TIMESTAMP"))  

#same for ec
ec_15 <- tmp_full %>%
  filter(research_name == "soil_EC_15cm") %>%
  group_by(TIMESTAMP, Plot) %>%
  drop_na(Value) %>%
  summarize(soil_ec_15cm = mean(Value)) 

final_tmp_data <- 
  left_join(tmp_data, ec_15, by = c("Plot", "TIMESTAMP"))  

#Now the gcrew data 
#Note: only freshwater (wetland) will have these variables,
#but we can extrapolate to other plots
#Note: first few months of 2022 don't have PAR or temp values

gcw_full %>%
  mutate(Plot = substr(Plot,1,2),
         Plot = case_when(Plot == "W" ~ "Freshwater",)) %>%
  dplyr::select(Plot, TIMESTAMP, Value, research_name) -> gcw


gcw %>%
  filter(research_name == "wx_par_den15") %>%
  mutate(PAR = Value) %>% 
  dplyr::select(TIMESTAMP, PAR) -> par

gcw %>%
  filter(research_name == "wx_tempavg15") %>%
  mutate(TEMP = Value) %>% 
  dplyr::select(TIMESTAMP, TEMP) -> temp

abiotic_data <- 
  merge(temp, par, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all = TRUE)

final_data <- 
  merge(final_tmp_data, abiotic_data, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all.x = TRUE) 

#Now we have a full time series for 2021-2024!

saveRDS(final_data,"Sapflow_BACI.rds")


