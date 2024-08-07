## This script works up a bunch of different vegetation datasets that may be
## useful to the TEMPEST2 story
##
## Datasets: Tree GHGs, sapflow (as Fd, Js, and Gc), photosynthesis rates, 
## stomatal conductance
##
## 2024-06-04
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")


# 2. Sapflow -------------------------------------------------------------------

## Read in raw sapflow
sapflow_raw <- read_csv("data/240507_sapflow_final_unbinned.csv") %>% 
  mutate(datetime_est = force_tz(datetime_est, tzone = common_tz)) %>% 
  filter(datetime_est >= as.POSIXct("2023-06-05", tz = common_tz) & 
         datetime_est < as.POSIXct("2023-06-14", tz = common_tz)) %>% 
  select(datetime_est, plot, sensor_id, sapflow_mv) %>% 
  mutate(date = date(datetime_est)) 

sapflow_binned <- sapflow_raw %>%
  group_by(datetime_est, plot) %>% 
  summarize(date = first(date), 
            sapflow_avg = mean(sapflow_mv, na.rm = T))

## Calculate dTmax: maximum daily sapflow value
dtmax <- sapflow_binned %>% 
  mutate(hour = hour(datetime_est)) %>% 
  filter(hour <= 8 | hour >= 20) %>%  # Originally 0-5, now doing approximately sunset to sunrise (8p to 8a)
  group_by(date, plot) %>% 
  summarise(dtmax = max(sapflow_avg, na.rm = TRUE), dTmax_datetime = datetime_est[which.max(sapflow_avg)])

## This is sap flux density (see L125)
sapflow_fd <- sapflow_binned %>% 
  left_join(dtmax, by = c("date", "plot")) %>% 
  mutate(fd = 360000 * (0.00011899) * (((dtmax / sapflow_avg) - 1)^1.231))

ggplot(sapflow_fd, aes(datetime_est, fd, color = plot)) + 
  geom_line()

## Based on what we're seeing, things are most different during the day, let's take 
## a look at time-of-day in this context: It's clear that freshwater is doing 
## different things from control, and that estuarine is a little higher but maybe
## not much. We also see that we should focus on noon to midnight
sapflow_fd %>% 
  mutate(tod = hour(datetime_est)) %>% 
  group_by(tod, plot) %>% 
  summarize(mean_fd = mean(fd, na.rm = T), 
            sd_fd = sd(fd, na.rm = T)) %>% 
  ggplot(aes(x = tod, color = plot)) + 
  geom_line(aes(y = mean_fd))

sapflow_fd %>% 
  filter(date != "2023-06-12") %>% 
  mutate(tod = hour(datetime_est)) %>% 
  ungroup() %>% 
  filter(tod > 12 & tod <= 21) %>% #capturing peak daylight hours
  group_by(date, plot) %>% 
  summarize(mean_fd = mean(fd, na.rm = T)) %>% 
  ggplot(aes(x = date, color = plot)) + 
  geom_line(aes(y = mean_fd))

## I'm not positive what I want to do here, but we should be normalizing to something
## like Fig 3C in https://openknowledge.nau.edu/id/eprint/2911/7/McDowell_N_etal_2006_Homeostatic_Maintenance_of_Ponderosa_Pine_Gas_Exchange(1).pdf
## per NM's comment
## I see this as: 1) 

## Now, let's calculate Js
## .... to be continued

# 3. Photosynthesis and stomatal conductance -----------------------------------

## ci = intercellular CO2 (ppm)
## gs = stomatal conductance (mol m-2 s-1)
## A (clean_names = a) = photosynthesis rate (umol m-2 s-1)
veg_raw <- read_csv("data/raw_data/vegetation/Compiled data_TEMPEST veg 2023.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(date)))

p_photosynthesis <- ggplot(veg_raw, aes(plot, a)) + 
  geom_boxplot(aes(fill = plot), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Photosynthesis rate (umol/m2/min)") + 
  scale_fill_viridis_d() 

p_stomatal_conductance <- ggplot(veg_raw, aes(plot, gs)) + 
  geom_boxplot(aes(fill = plot), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Stomatal conductance (mol/m2/min)") + 
  scale_fill_viridis_d() 
  
p_intercellular_co2 <- ggplot(veg_raw, aes(plot, ci)) + 
  geom_boxplot(aes(fill = plot), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  geom_tukey(where = "whisker") + 
  labs(x = "", y = "Intercellular CO2 (ppm)") + 
  scale_fill_viridis_d()

plot_grid(p_intercellular_co2, p_photosynthesis, p_stomatal_conductance, 
          nrow = 1)
ggsave("figures/5_Fig5_vegetation_responses.png", width = 9, height = 4)
ggsave("figures/5_Fig5_vegetation_responses.pdf", width = 9, height = 4)

# 4. Tree GHG fluxes and sapflow (immediate responses) -------------------------

ghg_path = "data/raw_data/ghgs/"

tree_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(collection_date = as_date(parsedate::parse_date(collection_date)))

ggplot(tree_ghg, aes(collection_date, flux_co2_umol_m2_min)) + 
  geom_point()












