## This script will (hopefully) contain all supplemental figures in one place.
## It will be long since it will require redoing many analyses, but at least
## things will be consolidated
##
## 2024-04-15
## Peter Regier 
## 
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")

require(ggtukey)

## Make a small helper script to format each dataset when reading in

prep_csvs <- function(path){
  read_csv(path) %>% 
    mutate(datetime_est = force_tz(datetime_est, tzon = common_tz)) %>% 
    mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                            TRUE ~ plot)) %>% 
    label_flood_periods() %>% 
    filter(!is.na(period))
}

## Load processed datasets. Note that we should only use datetime_est
teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm)


# 2. Make boxplots (Figure SA) -------------------------------------------------

## Create a function, which relies heavily on ggtukey::geom_tukey() to do the heavy
## lifting described in https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots
make_tukey_boxplots <- function(data, var, y_label){
  ggplot(data, aes(x = period, y = {{var}}, fill = period)) + 
    geom_boxplot(width = 0.8, show.legend = F) + 
    facet_wrap(~plot) + 
    geom_tukey(where = "whisker") + 
    scale_x_discrete(labels = unique(data$period_relabel)) + 
    scale_fill_viridis_d() + 
    labs(x = "", y = y_label)
}

plot_grid(make_tukey_boxplots(teros, vwc, bquote("VWC (m"^{3}/m^{3}*")")), 
          make_tukey_boxplots(teros, ec, "EC (uS/cm)"), 
          make_tukey_boxplots(firesting, do_percent_sat, "DO (mg/L)"), 
          make_tukey_boxplots(swap, eh_mv, "Eh (mV)"), 
          ncol = 1)
ggsave("figures/supplemental/sa_boxplots.png", width = 12, height = 13)
ggsave("figures/supplemental/sa_boxplots.pdf", width = 12, height = 13)

## Stats accompanying Figure SA
teros %>% 
  group_by(plot, depth) %>% 
  summarize(vpre = mean(vwc[period_relabel == "Pre-Flood"], na.rm = T), 
            vdist = max(vwc[grepl("Flood #", period_relabel)], na.rm = T)) %>% 
  mutate(diff = vdist - vpre) %>% 
  mutate(per_diff = (diff/vpre) * 100)

teros %>% 
  group_by(plot, depth) %>% 
  summarize(vpre = mean(ec[period_relabel == "Pre-Flood"], na.rm = T), 
            vdist = max(ec[grepl("Flood #", period_relabel)], na.rm = T)) %>% 
  mutate(diff = vdist - vpre) %>% 
  mutate(per_diff = (diff/vpre) * 100)
  
firesting %>% 
  group_by(plot, depth) %>% 
  summarize(vpre = mean(do_percent_sat[period_relabel == "Pre-Flood"], na.rm = T), 
            vdist = min(do_percent_sat[grepl("Flood #|Inter", period_relabel)], na.rm = T), 
            vpost = min(do_percent_sat[grepl("Post", period_relabel)], na.rm = T)) %>% 
  mutate(diff = vdist - vpre) %>% 
  mutate(per_diff = (diff/vpre) * 100)

################################################################################

# DO consumption rates (Figure SB) ---------------------------------------------

## Code taken from 231019_Figure4_anoxia_hypoxia_rates.R then modified
anoxia_by_plot_and_depth <- firesting %>% 
  filter(grepl("Flood #", period_relabel)) %>% 
  group_by(period_relabel, plot, depth) %>% 
  summarize(period = first(period), 
            perc_anoxic = length(do_percent_sat[do_percent_sat < 1]) / length(do_percent_sat) * 100,
            perc_hypoxic = length(do_percent_sat[do_percent_sat < 20]) / length(do_percent_sat) * 100)

## Calculating consumption rates is a manual process and a little painful

## First, visualize the data we want to look at
firesting %>% 
  filter(plot != "Control") %>% 
  ggplot(aes(datetime, do_percent_sat, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

## We can probably do two things: calculate the rate of decrease and the rate of 
## increase but let's focus on the consumption first. We'll follow code from the
## EC1 GHG paper. We ended up manually IDing where we cut. And that's not ideal,
## but we realistically have 4 depths and two events, and no recovery from 30 cm
## so a total of 6 rates to examine and calculate. Let's make a nice clean dataset
## that's easy to work with
firesting_rates_raw <- firesting %>% 
  filter(plot != "Control") %>% 
  filter(datetime_est > as.POSIXct("2023-06-06 04:00", tz = common_tz)) %>% 
  group_by(plot, depth) %>% 
  mutate(index = 1:n()) %>% 
  ungroup()

## Set up a matrix with all the plots we'll need indices for
trim_start_raw <- expand_grid(plot = unique(firesting_rates_raw$plot), 
                              depth = unique(firesting_rates_raw$depth), 
                              period_relabel = c("Flood #1", 
                                                 "Flood #2")) 

i = 16
## Plotly plot to examine and ID points where we want to trim each value
p <- firesting_rates_raw %>% 
  filter(plot == trim_start_raw$plot[[i]] & 
           depth == trim_start_raw$depth[[i]] & 
           grepl(trim_start_raw$period_relabel[[i]], period_relabel)) %>% 
  ggplot(aes(index, do_percent_sat, label = datetime_est)) + 
  geom_line() + 
  geom_point(alpha = 0.5)

ggplotly(p)

## really good news here is that all Flood #1 events are somewhat linear
## decreasing patterns that don't need to be trimmed, so let's just take the
## Flood #1 time-period and calculate rates! No guessing, no manual anything, 
## just taking the data, finding DOstart and DOend then rates based on the 10
## hours of the events

pull_data <- function(plot, 
                      depth = depth, 
                      period_relabel = period_relabel){ 
  
  x <- firesting_rates_raw %>% 
    filter(plot == plot, 
           depth == depth, 
           period_relabel = period_relabel)
  
  do_max = max(x$do_percent_sat, na.rm = T)
  do_min = max(x$do_percent_sat, na.rm = T)
  
  rate = (do_max - do_min) / 10
  
  return(rate)
}

# trim_start_raw %>% 
#   filter(period_relabel == "Flood #1") %>% 
#   #dplyr::slice(1) %>% 
#   pmap(pull_data)


# ## What does the relationship between soil CO2 concentration and flux look like?
ghg_path = "data/raw_data/ghgs/"

soil_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_soil_nickworking.csv")) %>%
  clean_names() %>%
  filter(!is.na(timepoint)) %>%
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>%
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>%
  mutate(condition = fct_relevel(condition, "Preflood"))

ggplot(soil_ghg, aes(condition, co2_umol_m2_s, fill = condition)) +
  geom_boxplot() +
  facet_wrap(~plot, nrow = 1)

baseline <- soil_ghg %>% 
  filter(plot != "Control") %>% 
  filter(condition == "Preflood") %>% 
  summarize(umol_m2_s = mean(co2_umol_m2_s, na.rm = T)) %>% 
  mutate(umol_m2_yr = umol_m2_s * 3600 * 365.25)
  
flood_duration <- soil_ghg %>% 
  filter(condition == "Flooded") %>% 
  mutate(datetime = as_datetime(paste(parse_date(date), time))) %>% 
  summarize(duration = as.numeric(max(datetime) - min(datetime))) %>% 
  pull()

reduction <- soil_ghg %>% 
  filter(plot != "Control") %>% 
  filter(condition == "Flooded") %>% 
  summarize(decline_s = mean(co2_umol_m2_s, na.rm = T)) %>% 
  mutate(diff = baseline$umol_m2_s - decline_s) %>% 
  mutate(diff_yr = diff * 3600 * flood_duration)

## So basically, it's really not that important...
(reduction$diff_yr / baseline$umol_m2_yr) * 100


# sgw <- read_csv("data/240909_soil_ghg_concentrations.csv")


## Calculate the percent of average daily fluxes associated with the decreased
## respiration




  
## For Flood #2, we have a couple patterns: 
### Normal rate, then plateauing near 0: i = 2, 4/6 (ish), 10, 12, 14 
### Really low (stays hypoxic/anoxic): i = 8, 16
## I think the most impartial would be 1) if max < 20, don't calculate a rate, 
## and if max > 20, remove all values < 1, or else use some sort of breakpoint
## identifier

  
  
  
  
