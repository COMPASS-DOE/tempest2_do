## This script is (hopefully) going to calculate a couple key metrics to understand
## how each variable responded to disturbance and recovered, in a quantitative,
## inter-comparable way. There are 3 metrics: x0, x1, and xf:
## x0: mean value for variable for 24 hours prior to Flood #1
## x1: min (variable decreased) or max (variable increased) during Floods (#1 & #2)
## xf: max (variable decreased) or min (variable increased) 48 hrs after Flood #2
## See 7/24 notes in notebook for drawings and formulae!

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")

## Set up a helper function
read_csv_ <- function(...){
  read_csv(...) %>% 
    mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
    filter(datetime > pre_event_start & 
             datetime < post_event_end)
  }

## Load firesting data
firesting <- read_csv_("data/230712_firesting.csv") %>% 
  filter(depth != 20) %>% 
  mutate(depth = case_when(depth == 10 | depth == 20 ~ 15, 
         TRUE ~ depth)) %>% 
  label_flood_periods()

## Load TEROS data
teros <- read_csv_("data/230712_teros_means.csv") %>% 
  select(-datetime_raw)

## Load SWAP data
swap <- read_csv_("data/230618_swap_redox_raw.csv") %>% 
  rename("depth" = depth_cm) %>% 
  filter(depth != 50) %>% 
  select(-datetime_raw)

df <- inner_join(firesting, teros, by = c("datetime", "plot", "depth")) %>% 
  inner_join(swap, by = c("datetime", "plot", "depth")) 


# 3. Calculate metrics

calculate_metrics <- function(var, direction){
  
  ## See comments at top for explanation of metrics
  
  ## First, subset data and group by plot and depth
  data <- df %>% 
    ungroup() %>% 
    group_by(plot, depth) %>% 
    dplyr::select(datetime, plot, depth, {{var}}, period_relabel)
  
  ## Calculate values for 
  x0 <- data %>% 
    filter(period_relabel == "Pre-Flood") %>% 
    summarize(mean_x0 = mean({{var}}), 
              sd_x0 = sd({{var}}))
  
  x1 <- data %>% 
    filter(period_relabel == "Flood #1" | period_relabel == "Flood #2") %>% 
    summarize(min_x1 = min({{var}}), 
              max_x1 = max({{var}}))
  
  xf <- data %>% 
    filter(period_relabel == "Post-Flood") %>% 
    summarize(min_xf = min({{var}}), 
              max_xf = max({{var}}))
  
  inner_join(x0, x1, by = c("plot", "depth")) %>%
    inner_join(xf, by = c("plot", "depth")) %>% 
    mutate(direction = direction) %>% 
    mutate(initial_change = case_when(direction == "increase" ~ (max_x1 - mean_x0)/(sd_x0),
                              direction == "decrease" ~ (min_x1 - mean_x0)/(sd_x0)))
  
}

change_df <- bind_rows(calculate_metrics(vwc, "increase") %>% mutate(var = "VWC"), 
          calculate_metrics(ec, "increase") %>% mutate(var = "EC"), 
          calculate_metrics(do_percent_sat, "decrease") %>% mutate(var = "DO"), 
          calculate_metrics(redox_mv, "decrease") %>% mutate(var = "Redox"))

change_df %>% 
  dplyr::select(var, initial_change, plot, depth) %>% 
  filter(plot != "Control") %>% 
  #pivot_wider(values_from = "initial_change", names_from = "var") %>% 
  ggplot(aes(var, initial_change, fill = as.factor(depth))) + 
  geom_col(position = "dodge") + 
  facet_wrap(~plot) + 
  scale_y_continuous(trans = pseudolog10_trans)


## This is interesting... need to translate this into an Rmd, and it should be based
## not on just exploring, but on the hypotheses...




wc_df <- df %>% 
  filter(depth == 5 & plot == "Freshwater") %>% 
  as.data.frame()

## The theory here is that vwc and do will correlate, but with a lag time, which should
## be minimal at 5cm. We're looking for similar behavior, particularly with the onset
## of the initial flood event.
x <- analyze.coherency(my.data = wc_df, my.pair = c("vwc", "do_percent_sat"))

wc.image(x)


plot_grid(df %>% 
            filter(depth == 5 & plot == "Seawater") %>%  
  ggplot(aes(datetime, vwc)) + 
  geom_line(), 
  df %>% 
    filter(depth == 5 & plot == "Seawater") %>% 
    ggplot(aes(datetime, do_percent_sat)) + 
    geom_line(), 
  ncol = 1)



## Create a dummy dataset
df %>% 
  filter(depth == 5 & plot == "Seawater") %>% 
  mutate(index = 1:n()) %>% 
  rename("var1" = vwc, 
         "var2" = do_percent_sat) %>% 
  select(index, contains("var")) %>% 
  write_csv(., "data/230725_dummy_data.csv")


## This is an interesting potential angle... still unclear what the VAR does
## (Vector Autoregression), but the code below let's us test Granger causality,
## which could either be a good first step for screening, or potentially VAR
## allows us to better interpret when things are correlating / causing each other
## Initial results (Granger / Instant)
### 5 FW: >0.5 for both
### 15 FW: >0.5 for both
## 30 FW: Granger < 0.5
## 5 SW: Both < 0.5
## 15 SW: Instant < 0.5
## 30 SW: Both < 0.5

# #p_load("vars")
#
# y <- df %>% 
#   filter(depth == 30 & plot == "Seawater") %>% 
#   mutate(index = 1:n()) %>% 
#   rename("var1" = vwc, 
#          "var2" = do_percent_sat) %>% 
#   dplyr::select(index, contains("var"))
# 
# var_model <- VAR(y[, c("var1", "var2")], p = 1)
# 
# causality(var_model, cause = "var1")







