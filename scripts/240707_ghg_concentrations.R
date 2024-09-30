## This script works up GHG concentrations from SWilson. We are using these to test
## what is driving changes in fluxes: diffusion (not measured) or concentration?
##
## 2024-07-07
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")

## List all files to read in
conc_files <- list.files("data/raw_data/swilson_ghg_concentrations", 
                         pattern = ".csv", full.names = T)

# 2. Read in soil gas well data ------------------------------------------------

## First, only one file for soil gas wells, so read that in by itself
sgw_raw <- read_csv(conc_files[grepl("SGW", conc_files)]) %>% 
  clean_names() %>% 
  mutate(date = parsedate::parse_date(paste(sample_year, sample_month, sample_day))) %>% 
  mutate(hours = as.numeric(str_sub(sample_time, 1, 2)),
         minutes = as.numeric(str_sub(sample_time, 3, 4))) %>%
  mutate(datetime_edt = date + hours(hours) + minutes(minutes)) %>% 
  mutate(datetime_est = with_tz(datetime_edt, tz = common_tz)) %>% 
  # mutate(time = hms(hours = hours, minutes = minutes, seconds = 0))
  dplyr::select(date, datetime_est, 
                sample_plot, g_w, 
                sample_grid, sample_depth, 
                event_stamp, 
                co2_conc_umol, 
                co2_conc_ppm_dilcorr,
                ch4_conc_nmol, 
                co2_flag)


# 3. Read in tree gas well data ------------------------------------------------

tgw_files <- conc_files[grepl("TGW", conc_files)]

read_tgw <- function(path){
  read_csv(path) %>% 
    clean_names() %>% 
    mutate(date = parsedate::parse_date(paste(sample_year, sample_month, sample_day))) %>% 
    mutate(hours = as.numeric(str_sub(sample_time, 1, 2)),
           minutes = as.numeric(str_sub(sample_time, 3, 4))) %>%
    mutate(datetime_edt = date + hours(hours) + minutes(minutes)) %>% 
    mutate(datetime_est = with_tz(datetime_edt, tz = common_tz)) %>% 
    mutate(event_stamp = as.character(event_stamp)) %>% 
    # mutate(time = hms(hours = hours, minutes = minutes, seconds = 0))
    dplyr::select(date, datetime_est, 
                  sample_plot, g_w, 
                  sample_grid, 
                  event_stamp, 
                  co2_conc_ppm_dilcorr, 
                  co2_flag)
}

tgw_raw <- tgw_files %>% 
  map(read_tgw) %>% 
  bind_rows()


# 4. Make some SGW plots to see what we have -----------------------------------

sgw <- sgw_raw %>% 
  filter(co2_flag == "Within Range") %>% 
  filter(!is.na(sample_plot)) %>% 
  filter(co2_conc_umol < 1e7) #%>% # manually remove one outlier
  #filter(sample_depth == 10) 

sgw %>% 
  ggplot(aes(datetime_est, co2_conc_umol, 
             color = as.factor(event_stamp))) + 
  geom_point() + 
  facet_wrap(~sample_plot, ncol = 1)

## Looks like the gas v water thing is going to be a problem
my_comparisons = list(c("0_preflood", "1_flood"), 
                      c("1_flood", "2_postflood"), 
                      c("0_preflood", "2_postflood"))

sgw_final <- sgw %>% 
  #filter(sample_depth == "10") %>% 
  mutate(period = case_when(date < dump_start1 ~ "0_preflood", 
                            date > dump_end2 ~ "2_postflood", 
                            TRUE ~ "1_flood")) 

sgw_final %>% 
  ggplot(aes(period, co2_conc_ppm_dilcorr, fill = period)) + 
  geom_boxplot(show.legend = F, alpha = 0.5) + 
  stat_compare_means(comparisons = my_comparisons) + 
  facet_wrap(~sample_plot) + 
  scale_fill_viridis_d()
ggsave("figures/240722_soil_co2_concentrations.png", width = 7, height = 3.5)

write_csv(sgw, "data/240909_soil_ghg_concentrations.csv")

# 5. Make some TGW plots to see what we have -----------------------------------

tgw <- tgw_raw %>% 
  filter(co2_flag == "Within Range") %>% 
  filter(!is.na(sample_plot))

tgw %>% 
  mutate(period = case_when(date < dump_start1 ~ "0_preflood", 
                            date > dump_end2 ~ "2_postflood", 
                            TRUE ~ "1_flood")) %>% 
  ggplot(aes(period, co2_conc_ppm_dilcorr, fill = period)) + 
  geom_boxplot(show.legend = F, alpha = 0.5) + 
  stat_compare_means(comparisons = my_comparisons) + 
  facet_wrap(~sample_plot) + 
  scale_fill_viridis_d()







