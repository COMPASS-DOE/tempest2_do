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

require(ggtukey, 
        rstatix)

anyas_colors = c("springgreen2", "cyan2", "violetred2")

## Make a small helper script to format each dataset when reading in

prep_csvs <- function(path){
  read_csv(path) %>% 
    mutate(datetime_est = force_tz(datetime_est, tzone = common_tz)) %>% 
    mutate(plot = case_when(plot == "Seawater" ~ "Saltwater", 
                            plot == "Estuarine" ~ "Saltwater",
                            TRUE ~ plot)) %>% 
    label_flood_periods() %>% 
    filter(!is.na(period))
}

## Load processed datasets. Note that we should only use datetime_est
teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm) %>% 
  mutate(eh_cat = case_when(eh_mv >= 400 ~ "1. oxidizing", 
                            eh_mv < 400 & eh_mv >= 200 ~ "2. weakly reducing", 
                            eh_mv < 200 & eh_mv >= -100 ~ "3. moderately reducing", 
                            eh_mv < -100 ~ "4. strongly reducing"))


################################################################################

# Site map and flooding ts (Figure S1) -----------------------------------------

flood_ts <- tibble(datetime = seq(from = flood1[1] - hours(29), 
                                  to = flood2[1] + hours(67), by = "5 min"), 
                   water_applied = case_when(datetime >= flood1[1] & 
                                               datetime < flood1[1] + hours(10) ~ (15/10)/12,
                                             datetime >= flood2[1] & 
                                               datetime < flood2[1] + hours(10) ~ (15/10)/12,
                   TRUE ~ 0), 
                   cumulative_flooding = cumsum(water_applied))

ggplot(flood_ts, aes(datetime, cumulative_flooding)) + 
  geom_line() +
  annotate(geom = "rect", xmin = flood1[1], xmax = flood1[2], 
           ymin = 0, ymax = 30,
           fill = "blue", alpha = 0.1) + 
  annotate(geom = "rect", xmin = flood2[1], xmax = flood2[2], 
           ymin = 0, ymax = 30,
           fill = "blue", alpha = 0.1) + 
  # geom_segment(aes(y = 32, yend = 32, 
  #                  x = flood1[1] - hours(29), xend = flood1[1]), color = "gray") + 
  # annotate(geom = "text", x = flood1[1] - hours(15), y = 35, label = "Pre-flood") + 
  # geom_segment(aes(y = 32, yend = 32, 
  #                  x = flood1[1], xend = flood2[2]), color = "blue") + 
  # annotate(geom = "text", x = flood2[1] - hours(6), y = 35, label = "Flood") + 
  # geom_segment(aes(y = 32, yend = 32, 
  #                  x = flood2[2], xend = flood2[2] + hours(48)), color = "gray") + 
  # annotate(geom = "text", x = flood2[1] + hours(40), y = 35, label = "Post-flood") + 
  # ylim(0, 38) + 
  labs(x = "", y = "Total water \n added (cm)")
ggsave("figures/supplemental/S1_ts.png", width = 8, height = 2)
ggsave("figures/supplemental/S1_ts.pdf", width = 8, height = 2)


################################################################################

# DO consumption rates (Figure S2) ---------------------------------------------

## Code taken from 231019_Figure4_anoxia_hypoxia_rates.R then modified
anoxia_by_depth <- firesting %>% 
  filter(grepl("Flood #", period_relabel)) %>% 
  group_by(plot, depth) %>% 
  summarize(period = first(period), 
            perc_anoxic = length(do_percent_sat[do_percent_sat < 1]) / length(do_percent_sat) * 100,
            perc_hypoxic = length(do_percent_sat[do_percent_sat < 20]) / length(do_percent_sat) * 100)

sb_p2 <- ggplot(anoxia_by_depth %>% 
         filter(plot != "Control"), 
       aes(perc_hypoxic, as.factor(depth), fill = plot)) + 
  geom_col(position = "dodge", alpha = 0.7) + 
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values = anyas_colors[2:3]) + 
  labs(x = "% Hypoxic", y = "Depth (cm)", fill = "")

sb_p3 <- ggplot(anoxia_by_depth %>% 
                  filter(plot != "Control"), 
                aes(perc_anoxic, as.factor(depth), fill = plot)) + 
  geom_col(position = "dodge", alpha = 0.7) + 
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values = anyas_colors[2:3]) + 
  labs(x = "% Anoxic", y = "Depth (cm)", fill = "")


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
  ungroup() %>% 
  filter(do_percent_sat > 0.1)

## Set up a matrix with all the plots we'll need indices for
consumption_event <- expand_grid(selected_plot = unique(firesting_rates_raw$plot), 
                              selected_depth = unique(firesting_rates_raw$depth), 
                              selected_period = c("Flood #1", 
                                                 "Flood #2")) 

i = 16
## Plotly plot to examine and ID points where we want to trim each value
p <- firesting_rates_raw %>% 
  filter(plot == consumption_event$selected_plot[[i]] & 
           depth == consumption_event$selected_depth[[i]] & 
           grepl(consumption_event$selected_period[[i]], period_relabel)) %>% 
  ggplot(aes(index, do_percent_sat, label = datetime_est)) + 
  geom_line() + 
  geom_point(alpha = 0.5)

ggplotly(p)

## FW 30cm Flood #2 (i = 8) is a problem, scrub. same for i=16

calculate_rate <- function(selected_plot, 
                           selected_depth, 
                           selected_period){
  
  x <- firesting_rates_raw %>% 
    filter(plot == selected_plot, 
           depth == selected_depth, 
           period_relabel == selected_period)

  x %>% 
    summarize(start_do = first(do_percent_sat), 
              min_do = min(do_percent_sat), 
              time = as.numeric(max(datetime_est) - min(datetime_est))) %>%
    mutate(plot = selected_plot, 
           depth = selected_depth, 
           period_relabel = selected_period) %>% 
    mutate(do_perc_per_hour = (start_do - min_do) / time)
}

calculate_rate("Freshwater", 5, "Flood #1")

do_rates <- consumption_event %>%
  pmap_dfr(function(selected_plot, selected_depth, selected_period) {
    calculate_rate(selected_plot, selected_depth, selected_period)
  }) 

sb_p1 <- ggplot(do_rates, aes(do_perc_per_hour, as.factor(depth), fill = period_relabel)) + 
  geom_col(position = "dodge", alpha = 0.7) + 
  facet_wrap(~plot, ncol = 1) + 
  scale_y_discrete(limits=rev) +
  labs(x = "DO consumption (%/hour)", y = "Depth (cm)", fill = "") 

plot_grid(sb_p1, plot_grid(sb_p2, sb_p3, ncol = 1, labels = c("B", "C")), 
          nrow = 1, labels = c("A", ""))
ggsave("figures/supplemental/S2_DO_stats.png", width = 8, height = 5)



################################################################################


# Leaf stress metrics (Figure S3) ----------------------------------------------

## Read in vegetation metrics
## ci = intercellular CO2 (ppm)
## gs = stomatal conductance (mol m-2 s-1)
## A (clean_names = a) = photosynthesis rate (umol m-2 s-1)
veg <- read_csv("data/raw_data/vegetation/Compiled data_TEMPEST veg 2023.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(date)))

## All three are different
veg %>% 
  rstatix::wilcox_test(a ~ plot)

## both different than Control
veg %>% 
  rstatix::wilcox_test(gs ~ plot)

plot_veg <- function(var, y_label){
  ggplot(veg_raw, aes(plot, {{var}})) + 
    geom_boxplot(aes(fill = plot), show.legend = F, outlier.alpha = 0, alpha = 0.5) + 
    geom_jitter(alpha = 0.5, width = 0.1) + 
    stat_compare_means(comparisons = list(c("Control", "Fresh"), 
                                          c("Control", "Salt"), 
                                          c("Fresh", "Salt")), 
                       label = "p.signif") + 
    labs(x = "", y = y_label) + 
    scale_fill_manual(values = anyas_colors)
}

plot_grid(plot_veg(a, "Photosynthesis rate (umol/m2/min)"), 
                        plot_veg(gs, "Stomatal conductance (mol/m2/min)"), 
                        nrow = 1, labels = c("A", "B"))
ggsave("figures/supplemental/S3_vegetation_responses.png", width = 7, height = 4)


################################################################################

# Belowground conductance (Figure S4) ------------------------------------------

p_load(readxl)


# 2. Read water potentials -----------------------------------------------------

Y_raw <- read_xlsx("data/raw_data/vegetation/241111_TEMPEST2_water_potentials.xlsx") %>% 
  clean_names() %>% 
  mutate(time_str = sprintf("%04d", time),                      # Ensure time is at least 4 digits
         time_hms = hms::as_hms(sprintf("%02d:%02d:00",
                                        as.integer(substr(time_str, 1, nchar(time_str)-2)),
                                        as.integer(substr(time_str, nchar(time_str)-1, nchar(time_str)))))) %>% 
  mutate(datetime = update(date, hours = hour(time_hms), minutes = minute(time_hms))) %>% 
  mutate(type = case_when(time < 600 ~ "Ypd", 
                          time >= 1100 & time <= 1215 ~ "Ymd", 
                          TRUE ~ "other")) %>% 
  mutate(hour = hour(time_hms))

Y_raw %>% 
  filter(type != "other") %>% 
  ggplot(aes(as.factor(hour), water_potential, color = plot)) + 
  geom_point() + 
  facet_wrap(~stem_id)


# 3. Calculate change in water potential (Ypd - Ymd) ---------------------------

Ypd_Ymd <- Y_raw %>% 
  filter(type != "other") %>% 
  group_by(plot, stem_id, type) %>% 
  mutate(water_potential = water_potential * -1) %>% 
  summarize(water_potential = mean(water_potential, na.rm = T)) %>% 
  pivot_wider(names_from = "type", values_from = "water_potential") %>% 
  mutate(Ypd_Ymd = Ypd - Ymd) %>% 
  ungroup()

ggplot(Ypd_Ymd, aes(plot, Ypd_Ymd)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2)


# 4. Read in sapflow -----------------------------------------------------------

sapflow_raw <- read_csv("data/250421_sapflow_by_tree.csv")

sapflow_trim <- sapflow_raw %>% 
  filter(Date > "2023-06-08" & 
           Date < "2023-06-14") %>% 
  group_by(plot, species, sensor_id) %>% 
  summarize(F_avg = mean(F_avg, na.rm = T))


# 5. Calculate k! --------------------------------------------------------------

## Join datasets and calculate k
bgc <- inner_join(sapflow_trim, 
                 Ypd_Ymd  %>% dplyr::select(-plot), 
                 by = c("sensor_id" = "stem_id")) %>% 
  mutate(k = F_avg / Ypd_Ymd) #%>% 
# filter(k > -4)

ggplot(bgc, aes(plot, k, fill = plot)) + 
  geom_boxplot(alpha = 0.7, show.legend = F) + 
  stat_compare_means(comparisons = list(c("Control", "Freshwater"), 
                                        c("Control", "Saltwater"), 
                                        c("Freshwater", "Saltwater")), 
                     label = "p.signif") + 
  labs(x = "") + 
  scale_fill_manual(values = anyas_colors)
ggsave("figures/supplemental/S4_belowground_conductance.png", width = 4, height = 4)


################################################################################

# Eh categories (Figure S5) ----------------------------------------------------

swap %>% 
  filter(datetime >= flood1 &
           datetime <= flood1 + days(5)) %>%
  group_by(plot, depth, eh_cat) %>% 
  summarize(n_eh_cat = n()) %>% 
  ungroup() %>% 
  group_by(plot, depth) %>% 
  mutate(perc = (n_eh_cat / sum(n_eh_cat)) * 100) %>% 
  ggplot(aes(as.factor(depth), perc, fill = eh_cat)) + 
  geom_col(position = "stack", width = 0.8, alpha = 0.7) + 
  facet_wrap(~plot, ncol = 1)  + 
  scale_fill_viridis_d(option = "D", direction = 1) + 
  labs(x = "Depth (cm)", y = "Percent", fill = "")
ggsave("figures/supplemental/S5_eh_categories.png", width = 5, height = 4)







