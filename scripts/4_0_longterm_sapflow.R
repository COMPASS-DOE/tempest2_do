## This script prepares and analyzes as much sapflow data from CY23 as possible
## to explore if we have a useful data-based demonstration of acute disturbance
## manifesting chronic disturbance. Code and data originally provided by 
## Kendalynn Morris with some edits by Peter Regier
##
## Peter Regier (Original code/data from Kendal)
## 2024-01-09
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


# 3. Read in BP and PAR --------------------------------------------------------

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


# 4. Combine datasets ----------------------------------------------------------

#Combining it all: editing dataframes for variables to match 
sapflow_inventory <- read_csv("data/from_kendal/sapflow_inventory.csv") %>% 
  clean_names()

sapflow_formatted <- sapflow_raw %>%
  mutate(plot = case_when(plot == "C" ~ "Control",
                          plot == "F" ~ "Freshwater",
                          plot == "S" ~ "Saltwater"))

species <- sapflow_inventory %>%
  mutate(species = substr(spp,1,4),
         species = case_when(spp == "ACRU" ~ "Red Maple",
                             spp == "LITU" ~ "Tulip Poplar",
                             spp == "FAGR" ~ "Beech")) %>%
  dplyr::select(plot, sapflux_id, species) %>% 
  mutate(plot = case_when(plot == "FW" ~ "Freshwater",
                          plot == "SW" ~ "Saltwater", 
                          TRUE ~ plot))

# 5. Calculate Fd --------------------------------------------------------------

#First, isolate sapflow data
sapflow_v0 <- sapflow_formatted %>% 
  filter(value >= 0.01, value <= 0.7) %>%
  dplyr::select(plot, timestamp, sensor_id, value) %>%
  rename("sapflow_2.5cm" = value) %>% 
  mutate(date = date(timestamp))

sapflow_sp <- full_join(sapflow_v0, 
                        species, 
                        by = c("sensor_id" = "sapflux_id", 
                               "plot" = "plot"))

#Calculate dTmax
sapflow_dtmax <- sapflow_sp %>% 
  drop_na(timestamp) %>% # There are 51 rows with NAs for datetime
  group_by(date, plot, species, sensor_id) %>% 
  summarize(dTmax = max(sapflow_2.5cm, na.rm = TRUE), 
            dTmax_time = timestamp[which.max(sapflow_2.5cm)])

#Calculate Fd
# convert the probe raw values (in mV) to sap flux velocity (cm/cm^2/s)
# Granier equation is Fd = (k * (deltaTmax - deltaT))^1.231
# k = 0.011899
sfd_data <- sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("plot", "species", "sensor_id", "date")) %>% 
  mutate(Fd = ((0.011899 * (((dTmax / sapflow_2.5cm) - 1)))^1.231))

#Combining it all: editing dataframes for variables to match 
tree_dat <- as_tibble(readRDS("data/from_kendal/dbh.rds"))

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

scaled <- merge(sfd_data %>% mutate(year = year(timestamp)), 
                sa_long, by.x = c("sensor_id", "year", "species"), 
                by.y = c("Sapflux_ID", "Year", "Species"), all.x = TRUE) %>% 
  as_tibble()

#final units are cubic centimeters per second
sf_scaled <- scaled %>%
  dplyr::select(sensor_id, year, species, plot, timestamp, Fd, SA) %>%
  mutate(F = SA * Fd)

sf_plot_by_tree <- sf_scaled %>% 
  mutate(Hour = hour(timestamp)) %>%
  mutate(Date = date(timestamp)) %>%
  mutate(monthyr = floor_date(timestamp, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(F <= 2, F >= 0) %>%
  group_by(plot, species, sensor_id, Date) %>% 
  summarise(F_avg = mean(F, na.rm = TRUE))
write_csv(sf_plot_by_tree, "data/250421_sapflow_by_tree.csv")


sf_plot_avg <- sf_scaled %>% 
  mutate(Hour = hour(timestamp)) %>%
  mutate(Date = date(timestamp)) %>%
  mutate(monthyr = floor_date(timestamp, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(F <= 2, F >= 0) %>%
  group_by(plot, species, Date) %>% 
  summarise(F_avg = mean(F, na.rm = TRUE))

ggplot(sf_plot_avg) + 
  geom_point(aes (x = Date, y = F_avg, color = species)) + 
  facet_wrap(~plot, ncol = 1, scales = "fixed") + 
  labs(y = "Avg Sap Flux Density", x = "Date", title = "Sap Flux Density Averaged Daily, 11 AM - 12 PM")

sf_plot_clean <- sf_plot_avg %>% 
  filter(Date %in% dates_with_good_weather$date)

roll_length = 10

sf_rollmean <- sf_plot_clean %>% 
  clean_names() %>% 
  ungroup() %>% 
  group_by(plot, species) %>% 
  #filter(species == "Be") %>% 
  mutate(f_roll = zoo::rollmean(f_avg, roll_length, fill = NA)) %>% 
  mutate(doy = yday(date),
        year = year(date)) 

sf_rollmean %>%
  ggplot(aes(x = doy, color = as.factor(year))) +
  geom_point(aes(y = f_avg), alpha = 0.2) + 
  geom_line(aes(y = f_roll)) + 
  facet_wrap(species~plot, 
             scales = "free_y", 
             nrow = 3)

sf_filtered <- sf_rollmean %>% 
  filter(year == 2023) 

write_csv(sf_filtered, "data/250310_2023_sapflow.csv")

sf_filtered %>% 
  ggplot(aes(date, color = plot)) + 
  geom_point(aes(y = f_avg), alpha = 0.1) +
  geom_line(aes(y = f_roll)) +
  geom_vline(xintercept = as_date(flood1), linetype = "dashed") + 
  facet_wrap(~species, ncol = 1) + 
  labs(x = "", y = "Sapflux (cm/s)", color = "") + 
  theme(legend.background = element_blank(), 
        legend.position = "bottom")
ggsave("figures/X_2023_sapflow.png", width = 5, height = 7)


first_flood = as_date("2023-06-06")
## Note that vegetation metrics were taken on 6-13, so let's use that as our window
##

plot_by_species <- function(spp){
  
  sf_plot_clean %>% 
    clean_names() %>% 
    #filter(species == spp) %>% 
    filter(date >= first_flood - days(7)) %>% 
    filter(date <= first_flood + days(7)) %>% 
    mutate(period = case_when(date < first_flood ~ "0_preflood", 
                              date >= first_flood ~ "2_postflood")) %>% 
    mutate(condition2 = as.factor(case_when(period == "0_preflood" ~ "Preflood", 
                                            period == "2_postflood" ~ "Postflood"))) %>% 
    mutate(condition2 = fct_relevel(condition2, "Preflood")) %>% 
    ggplot(aes(condition2, f_avg, color = condition2, fill = condition2)) + 
    geom_boxplot(alpha = 0.6, show.legend = F) +
    geom_jitter(width = 0.2, show.legend = F) + 
    facet_wrap(~plot, nrow = 1, scales = "free") + 
    geom_tukey(where = "whisker") +
    #stat_compare_means() +
    labs(x = "", y = "Sapflux") + 
    theme_linedraw(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "gray90"), 
      panel.grid.minor = element_line(color = "gray90"),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(face = "bold", color = "black"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)
    ) + 
    scale_fill_viridis_d(option = "mako", end = 0.8) + 
    scale_color_viridis_d(option = "mako", end = 0.8)
}

plot_by_species("Tulip Poplar")




sf_plot_clean %>% 
  clean_names() %>% 
  filter(date >= first_flood - days(7)) %>% 
    filter(date <= first_flood + days(7)) %>% 
  mutate(period = ifelse(date < first_flood, "0_preflood", "1_postflood")) %>% 
    ggplot(aes(period, f_avg, fill = period)) + 
    geom_boxplot() + 
  facet_wrap(species~plot, nrow = 1, scales = "free") + 
  geom_tukey(where = "whisker") 



################################################################################

################################################################################

################################################################################

sf_filtered <- sf_rollmean %>% 
  filter(year == 2023) %>% 
  filter(doy > 250 & doy < 325) %>% 
  filter(species == "Tulip Poplar") 

sf_filtered %>% 
  ggplot(aes(x = doy, y = f_avg, color = as.factor(plot))) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~year, 
             scales = "free_y", 
             nrow = 1)

sf_rollmean %>% 
  #filter(doy > 200 & doy < 300) %>% 
  ggplot(aes(x = doy, y = f_avg, color = as.factor(plot))) +
  geom_line(aes(y = f_roll)) + 
  #geom_point(alpha = 0.2) + 
  #geom_smooth(method = "lm") + 
  facet_wrap(species~year, 
             scales = "free_y")

install.packages("pracma")
p_load(pracma)

y <- sf_rollmean %>% 
  filter(year == "2022" & species == "Beech" & plot == "Control") %>% 
  filter(doy > roll_length) %>%  # remove january because of rolling mean
  drop_na(f_roll) %>% 
  mutate(f_roll = f_roll - min(f_roll, na.rm = T))

ggplot(y, aes(doy, f_roll)) + 
  geom_hline(yintercept = 0) + 
  geom_point()

#trapz(y$doy, y$f_roll)
#rm(y)

sf_rollmean %>% 
  filter(doy > roll_length) %>%  # remove january because of rolling mean
  drop_na(f_roll) %>% 
  ungroup() %>% 
  group_by(year, species, plot) %>% 
  mutate(f_rolln = f_roll - min(f_roll, na.rm = T)) %>% 
  summarize(auc = trapz(doy, f_rolln)) %>% 
  ggplot(aes(year, auc, color = plot)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~species, ncol = 1, scales = "free")

control_data <- sf_rollmean %>% 
  filter(doy > roll_length) %>%  # remove january because of rolling mean
  drop_na(f_roll) %>% 
  ungroup() %>%
  filter(plot == "Control") %>%
  dplyr::select(species, doy, year, control_f_roll = f_roll)

y <- sf_rollmean %>%
  filter(plot != "Control") %>%
  inner_join(control_data, by = c("species", "doy", "year")) %>%
  mutate(f_roll_difference = f_roll - control_f_roll)

y %>% 
  ungroup() %>% 
  filter(year != 2022) %>% 
  ggplot(aes(doy, f_roll_difference, color = plot, linetype = as.factor(year))) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  #geom_smooth(method = "lm", se = F) + 
  facet_wrap(~species, ncol = 1, scales = "free")

# Assuming `data` is your dataset
grouped_data <- sf_filtered %>%
  group_by(year) %>%
  nest()

# Define a function to fit an ANCOVA (aov with interaction) and perform Tukey's HSD test
fit_ancova_and_tukey <- function(df) {
  model <- aov(f_avg ~ doy * plot, data = df)
  tukey <- glht(model, linfct = mcp(plot = "Tukey"))
  tidy(summary(tukey))
}

# Apply the function to each nested dataframe
results <- grouped_data %>%
  mutate(tukey_results = map(data, fit_ancova_and_tukey)) %>%
  unnest(tukey_results)

# Clean up and format the results
results_clean <- results %>%
  select(year, contrast, estimate, conf.low, conf.high, adj.p.value) %>%
  arrange(year, contrast)

results_clean


## Trying tree-specific 2021 v 2023 comparisons: 
## To get statistical power, group by sensor_ID rather than species
## calculate the difference between 2023 and 2021 (F_2023 - F_2021) via AUC
## or via average during a range? Avg by month and see if things are different?
## DO IT BY MONTH

sf_plot_avg_by_tree <- sf_scaled %>% 
  mutate(Hour = hour(timestamp)) %>%
  mutate(Date = date(timestamp)) %>%
  mutate(monthyr = floor_date(timestamp, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(F <= 2, F >= 0) %>%
  group_by(plot, sensor_id, species, Date) %>% 
  summarise(F_avg = mean(F, na.rm = TRUE)) %>% 
  clean_names() %>% 
  mutate(year = year(date), 
         month = month(date))

sf_avgs <- sf_plot_avg_by_tree %>% 
  ungroup() %>% 
  #filter(year != "2022") %>% 
  mutate(doy = yday(date)) %>% 
  dplyr::select(-date) %>% 
  group_by(plot, sensor_id, species, year, month, doy) %>% 
  summarize(f_avg = mean(f_avg, na.rm = T)) %>% 
  pivot_wider(names_from = "year", values_from = "f_avg", names_prefix = "f_") %>% 
  mutate(delta_2022 = f_2022 - f_2021, 
         delta_2023 = f_2023 - f_2021) %>% 
  ungroup() %>% 
  group_by(month, plot, species) %>% 
  summarize(delta_2022 = mean(delta_2022, na.rm = T), 
            delta_2023 = mean(delta_2023, na.rm = T))



make_delta_plot <- function(var, y_lab){
  
  my_comparisons = list(c("Control", "Freshwater"), 
                        c("Control", "Saltwater"))
  
  sf_avgs %>% 
    ggplot(aes(plot, {{var}}, color = plot, fill = plot)) + 
    geom_boxplot(alpha = 0.6, show.legend = F) + 
    geom_jitter(width = 0.2, show.legend = F) + 
    geom_hline(yintercept = 0) + 
    scale_color_viridis_d() + 
    scale_fill_viridis_d() + 
    facet_wrap(~species, nrow = 1) + 
    stat_compare_means(comparisons = my_comparisons) + 
    labs(x = "", y = y_lab)
}

plot_grid(#make_delta_plot(delta_2022, "Difference in F (2022 - 2021)"), 
          make_delta_plot(delta_2023, "Difference in F (2023 - 2021)"), 
          ncol = 1)
ggsave("figures/250117_longterm_sapflow.png", width = 7, height = 4)



