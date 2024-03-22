## This script explores a potential final figure, and how it might lay out
## The goal is a control plot using range of Control as the reference
## 
## Peter Regier
## 2023-12-23
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")

p_load(scales)


# 2. Read in data --------------------------------------------------------------

## First, bring in TEROS data and set time-zone
teros <- read_csv("data/231102_teros_final.csv") %>% 
  mutate(datetime_est = with_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

firesting <- read_csv("data/230712_firesting.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot))

swap <- read_csv("data/230618_swap_redox_raw.csv") %>% 
  mutate(datetime_est = force_tz(datetime, tzone = common_tz)) %>% 
  mutate(plot = case_when(plot == "Seawater" ~ "Estuarine", 
                          TRUE ~ plot)) %>% 
  #label_flood_periods() %>% 
  # mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood")) %>% 
  rename("depth" = depth_cm) %>% 
  #filter(!(plot == "Control" & depth == 5)) %>% # Filter out so it's comparable to VWC/DO
  filter(!(plot == "Control" & redox_mv == 0)) %>% # manually remove errors
  filter(!(plot == "Seawater" & redox_mv == 0)) %>% 
  filter(!is.na(eh_mv))

vwc_do_cols <- c("datetime_est", "plot", "depth")

df <- full_join(teros %>% select(all_of(vwc_do_cols), vwc), 
                           firesting %>% select(all_of(vwc_do_cols), do_percent_sat), 
                           by = c(vwc_do_cols)) %>% 
  full_join(swap %>% select(all_of(vwc_do_cols), eh_mv),  by = c(vwc_do_cols)) %>% 
  mutate(datetime = datetime_est)


# 3. Construct control plots ---------------------------------------------------

half_range <- function(x){(max(x, na.rm = T) - min(x, na.rm = T)) / 2}
range_ <- function(x){max(x, na.rm = T) - min(x, na.rm = T)}


df30 <- df %>% 
  filter(depth == 30) %>% 
  drop_na()

control_plot <- function(var){
  
  control_range <- df30 %>% filter(plot == "Control") %>% 
    summarize(control_range = range_({{var}})) %>% 
    pull(control_range)

  var_min = min(df30 %>% pull({{var}}), na.rm = T)
  var_max = min(df30 %>% pull({{var}}), na.rm = T)
  
  plot_plot <- function(selected_plot){
    
    first_value = df30 %>% 
      filter(plot == selected_plot) %>% 
      slice(1) %>% 
      pull({{var}})
    
    ggplot(df30 %>% filter(plot == selected_plot), 
           aes(datetime_est, {{var}})) + 
      geom_line() + 
      annotate(geom = "rect", xmin = min(df30$datetime_est), 
               xmax = max(df30$datetime_est), 
               ymin = first_value + control_range, 
               ymax = first_value - control_range, 
               fill = "forestgreen", color = NA, alpha = 0.1) + 
     geom_hline(yintercept = first_value + control_range, color = "gray", linetype = "dashed") + 
      geom_hline(yintercept = first_value - control_range, color = "gray", linetype = "dashed") + 
      labs(x = "", title = selected_plot)
      
  }
  
  plot_grid(plot_plot("Control"), 
            plot_plot("Freshwater"), 
            plot_plot("Estuarine"), 
            ncol = 1)
}

plot_grid(control_plot(vwc), 
          control_plot(do_percent_sat), 
          control_plot(eh_mv), 
          nrow = 1)
ggsave("figures/231223_control_plots_v1.png", 
       width = 12, height = 8)


