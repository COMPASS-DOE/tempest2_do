## This script creates the dataset we'll use for TEROS, and is based on decisions
## that are documented in 230710_clean_teros.Rmd.
##
## 2023-07-14
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


# 2. Read in raw data ----------------------------------------------------------

teros_raw <- read_csv("data/230710_teros_raw_15min.csv") %>% 
  mutate(datetime_raw = datetime, 
         datetime = force_tz(datetime, tz = common_tz))


# 3. QC ------------------------------------------------------------------------

## 1. Remove VWC sensors with spuriously high (and static) VWC
vwc_upper_threshold = 0.75
teros_trim1 <- teros_raw %>% filter(vwc < vwc_upper_threshold)

## 2. Remove spuriously high temperature values (potential sensor out-of-soil)
tsoil_upper_threshold = 25
teros_trim2 <- teros_trim1 %>% filter(tsoil < tsoil_upper_threshold)

## 3. Remove shift in control data when several sensors cut out
control_gap_start = "2023-06-06"
control_gap_end = "2023-06-08 12:00"

sensors_to_scrub <- teros_trim2 %>% 
  filter(datetime > control_gap_start & 
           datetime < control_gap_end) %>%
  group_by(plot, sensor_id) %>% 
  count() %>% 
  filter(n < 200)

teros_trim3 <- teros_trim2 %>% 
  filter(!(sensor_id %in% sensors_to_scrub$sensor_id))


# 4. Calculate means -----------------------------------------------------------

tic("calculate means") ##112s
teros_means <- teros_trim3 %>% 
  select(-c(contains("_id"), grid_square)) %>% 
  group_by(datetime, plot, depth) %>% 
  summarize(across(where(is.numeric), mean, na.rm = T), 
            datetime_raw = first(datetime_raw))
toc()


# 5. Check plots ---------------------------------------------------------------

check_plot <- function(var){
  ggplot(teros_means %>% filter(plot == "Seawater"), aes(datetime, {{var}}, color = as.factor(depth))) + 
    geom_line() + 
    geom_vline(xintercept = dump_start1) + 
    geom_vline(xintercept = dump_start2)
  #facet_wrap(~plot, nrow = 1)
}

plot_grid(check_plot(vwc), 
          check_plot(ec), 
          check_plot(tsoil), 
          ncol = 1)


# 6. Write out -----------------------------------------------------------------

write_csv(teros_means %>% 
            mutate(datetime = as.character(datetime)) %>% 
            filter(datetime <= post_event_end), "data/230712_teros_means.csv")

