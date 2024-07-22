## One-off script to work up initial redox data from Roy
##
## Peter Regier
## 2024-05-10
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_0_setup.R")


# 2. Read in data --------------------------------------------------------------

swap_list <- list.files(path = "data/240510_redox_data_from_roy", pattern = ".dat", full.names = "T")


read_swap <- function(path){
  read_delim(path, skip = 1) %>% 
    slice(3:n()) %>% 
    mutate(across(c(contains("Batt"), contains("Redox")), as.numeric)) %>% 
    clean_names() %>% 
    mutate(datetime_est = lubridate::as_datetime(timestamp, tz = common_tz)) 
}

control <- read_swap(swap_list[grepl("control", swap_list)]) %>%
  mutate(plot = "Control")

fw <- read_swap(swap_list[grepl("fresh", swap_list)]) %>%
  mutate(plot = "Freshwater")

sw <- read_swap(swap_list[grepl("salt", swap_list)]) %>%
  mutate(plot = "Estuarine")


# 3. Format data ---------------------------------------------------------------

set_depths <- function(data){
  data %>% 
    pivot_longer(cols = contains("redox_"), names_to = "sensor", values_to = "redox_mv") %>% 
    separate(sensor, into = c("scrap", "ref", "sensor"), sep = "_") %>% 
    mutate(depth_cm = case_when(sensor == "1" | sensor == "5" | sensor == "9" | sensor == "13" | sensor == "17" ~ 5, 
                                sensor == "2" | sensor == "6" | sensor == "10" | sensor == "14" | sensor == "18" ~ 15, 
                                sensor == "3" | sensor == "7" | sensor == "11" | sensor == "15" | sensor == "19" ~ 30, 
                                sensor == "4" | sensor == "8" | sensor == "12" | sensor == "16" | sensor == "20" ~ 50,
                                TRUE ~ 0)) %>% 
    select(-c(statname, scrap, timestamp))
}

df_raw <- bind_rows(set_depths(control), 
                    set_depths(fw), 
                    set_depths(sw))

df_means <- df_raw %>% 
  ungroup() %>% 
  group_by(datetime_est, plot, depth_cm, ref) %>% 
  summarize(mean_redox = mean(redox_mv, na.rm = T))

plot_by_plot <- function(selected_plot){
  ggplot() + 
    geom_line(data = df_raw %>% filter(plot == selected_plot), 
              aes(datetime_est, redox_mv, group = sensor), color = "gray") + 
    geom_line(data = df_means %>% filter(plot == selected_plot), 
              aes(datetime_est, mean_redox)) + 
    facet_grid(ref~depth_cm) + 
    labs(x = "", title = paste0("Plot: ", selected_plot, " (black = mean)"))
  
  # ggsave(paste0("data/240510_redox_data_from_roy/plots/redox_", selected_plot, ".png"), 
  #        width = 8, height = 4.5)
}

plot_by_plot("Control")
plot_by_plot("Estuarine")
plot_by_plot("Freshwater")





  