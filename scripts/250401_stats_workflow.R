## This is an updated (and maybe final? Let's not get our hopes up yet) approach
## for comparing means of time-series periods. I've tried many different methods
## and the closest I've gotten is a general additive model

source("scripts/0_0_setup.R")

p_load(mgcv)

teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm)

## New label_flood_periods that's simplified
label_flood_periods2 <- function(data){ 
  data %>% 
    #mutate(datetime = force_tz(datetime_est, tz = common_tz)) %>% 
    mutate(period = case_when(datetime_est >= dump_start1 - hours(24) & datetime_est < dump_start1 ~ "0_preflood", 
                              datetime_est >= dump_start1 & datetime_est < dump_end2 ~ "1_flood", 
                              datetime_est >= dump_end2 & datetime_est < dump_end2 + hours(24) ~ "2_postflood", 
                              TRUE ~ NA)) 
}

x <- teros %>% 
  filter(plot == "Freshwater") %>% 
  filter(depth == 5) %>% 
  label_flood_periods2() %>% 
  filter(!is.na(period)) %>% 
  mutate(date_round = round_date(datetime_est, "1 hour")) %>% 
  group_by(date_round) %>% 
  summarize(period = first(period), 
            ec = mean(ec, na.rm =T),
            vwc = mean(vwc, na.rm =T)) 

## GAM - generalized additive model
## 

# Fit GAM model
gam_model <- gam(vwc ~ s(as.numeric(date_round)) + period, data = x)

# Summary of the new model
summary(gam_model)

# Normality Check
gam_residuals <- residuals(gam_model)
qqnorm(gam_residuals)
qqline(gam_residuals)
shapiro.test(gam_residuals)

# Independence and Autocorrelation Check
plot(x$date_round, gam_residuals, type = "b", main = "Residuals Over Time - GAM")



