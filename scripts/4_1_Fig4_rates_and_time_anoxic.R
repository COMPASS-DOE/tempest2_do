## This script calculates oxygen consumption rates and time hypoxic/anoxic for 
## each plot at each depth. 
##
## NOTE: estuarine rates aren't being calculated right, culprit is delta_time
## 2024-05-01
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")

## Make a small helper script to format each dataset when reading in

prep_csvs <- function(path){
  read_csv(path) %>% 
    mutate(datetime_est = force_tz(datetime_est, tzon = common_tz)) %>% 
    mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                            TRUE ~ plot))
}

## Load processed datasets. Note that we should only use datetime_est
firesting <- prep_csvs("data/240318_firesting_final.csv")


# 2. Initial visualization and description of approach to calculate rates ------

## For rate calculations, 30 cm depths both stayed anoxic for the second event, 
## so we won't have rates. To make things as unbiased as possible, we'll use
## the earliest response (4:45 both days)
firesting %>% 
  filter(plot != "Control") %>% 
  filter(datetime > "2023-06-06" & datetime < "2023-06-07 12:00:00") %>% 
  ggplot(aes(datetime_est, do_percent_sat, color = as.factor(depth))) + 
  geom_vline(xintercept = force_tz(as_datetime("2023-06-06 04:45:00"), tz = common_tz), linetype = "dashed") +
  geom_vline(xintercept = force_tz(as_datetime("2023-06-07 04:45:00"), tz = common_tz), linetype = "dashed") + 
  geom_line() + 
 facet_wrap(~plot, ncol = 1)


# 3. Parse flood events --------------------------------------------------------

## Code in 230610_analyze_firesting.R works, but I'd like to make things as 
## automated as possible to remove potential reviewer comments on subjectivity
## We'll treat 4:45 as the start of each event, cut rates at 24 hours, then trim
## the end to the first instance of the minimum value. Hopefully we can do this 
## all within a single function

## Create start/end datetime pairings for each flood
flood1 <- c(dump_start1, dump_end1)
flood2 <- c(dump_start2, dump_end2)

calculate_do_consumption_rates <- function(flood){
  
  ## This dataset trims to data used for rate calculations
  x <- firesting %>% 
    dplyr::select(-datetime) %>% 
    ungroup() %>% 
    filter(plot != "Control") %>% # exclude control
    filter(datetime_est >= flood[[1]] &
             datetime_est < flood[[2]]) %>% 
    group_by(plot, depth) %>% 
    mutate(min_do_time = datetime_est[which.min(do_percent_sat)]) %>%
    #mutate(cut = ifelse(datetime <= min_do_time, "keep", "cut"))
    filter(datetime_est <= min_do_time) %>% 
    filter(do_percent_sat > 1) # Based on sensor accuracy
  
  rates <- x %>%
    summarize(start_time = first(datetime_est),
              end_time = last(datetime_est),
              start_do = first(do_percent_sat),
              end_do = last(do_percent_sat))
  return(rates)
}

## Diagnostic plot (return x instead of rates)
# calculate_do_consumption_rates(flood2) %>%
#   #select(delta_do_perc_per_hr)
#   ggplot(aes(datetime_est, do_percent_sat, color = as.factor(depth))) +
#   geom_line() +
#   facet_wrap(~plot)

## I'm calculating down here since there were rates issues
do_rates <- bind_rows(calculate_do_consumption_rates(flood1) %>% 
                        mutate(event = "Flood #1"), 
                      calculate_do_consumption_rates(flood2) %>% 
                        mutate(event = "Flood #2")) %>% 
  mutate(delta_time = as.numeric((end_time - start_time), unit = "hours"),
         delta_do = start_do - end_do,
         delta_do_perc_per_hr = delta_do / delta_time) ## DO consumption rate in percent/L/hr

p_rates <- do_rates %>% 
  filter(delta_do_perc_per_hr > 0) %>% #remove Estuarine 30cm Flood2
  ggplot(aes(delta_do_perc_per_hr, as.factor(depth), fill = event)) + 
  geom_col(position = position_dodge(preserve = "single")) + 
  scale_y_discrete(limits=rev) + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "DO consumption (%/hr)", y = "Soil depth (cm)")


# 3. Calculate time hypoxic/anoxic ---------------------------------------------

## This is a whole thing. I wanted a definition based on the literature, and
## https://doi.org/10.1111/nph.14519 was an obvious place to start. They suggest
## that anoxic is total absence of oxygen, which is unlikely, so I think we should
## focus on hypoxia, which is often defined as a level below which a process is
## altered (for us, plants are harmed). However, that's subjective, and is use-
## dependent. Sasidharan et al. referenced https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2707311/
## which states root cortex tip respiration declined when partial pressures of O2
## fell below 05-4.5 kPa, so, assuming their plants are like our plants, that would
## give us a threshold fo 4.5 kPa. Now we need to convert that into % air sat. 
## https://www.ncbi.nlm.nih.gov/books/NBK493219/ suggests in ambient conditions, 
## 100% air sat is 160 mmHg, which is equivalent to 21.3316 kPa. That means, if
## this relationship is linear, anything below ((4.5/21.3) * 100) = 21% is 
## hypoxic. that's suuuuuper close to what we were using (20%)!!!. Great. Run 
## with it.

## see explanation above...
hypoxia_threshold = ((4.5/21.3) * 100)

p_hypoxic <- firesting %>% 
  dplyr::select(-datetime) %>% 
  ungroup() %>% 
  filter(plot != "Control") %>% # exclude control
  label_flood_periods() %>% 
  filter(!is.na(period_relabel) & 
           period_relabel != "Pre-Flood") %>%
  mutate(period_relabel = fct_relevel(period_relabel, "Flood #1", "Inter-Flood", "Flood #2", "Post-Flood")) %>% 
  #group_by(plot, depth, period_relabel) %>% 
  group_by(plot, depth, period_relabel) %>% 
  summarize(count_below_threshold = sum(do_percent_sat < hypoxia_threshold),
            delta_hrs = round(as.numeric((max(datetime_est) - min(datetime_est)), unit = "hours")), 1) %>% 
  mutate(hrs_hypoxic = (count_below_threshold * 5)/60, 
         perc_hypoxic = (hrs_hypoxic/delta_hrs) * 100) %>% 
  ggplot(aes(perc_hypoxic, as.factor(depth), fill = period_relabel)) + 
  geom_col(position = position_dodge(preserve = "single")) + 
  scale_y_discrete(limits=rev) + 
  facet_wrap(~plot, ncol = 1) + 
  labs(x = "% of period hypoxic", y = "Soil depth (cm)")

plot_grid(p_rates, p_hypoxic, nrow = 1)
ggsave("figures/5_do_rates_and_hypoxica.png", width = 10, height = 6)

  
# Some stats

do_rates %>% 
  filter(delta_do_perc_per_hr > 0) %>% 
  ungroup() %>% 
  group_by(event) %>% 
  summarize(mean(delta_do_perc_per_hr), 
            sd(delta_do_perc_per_hr))



