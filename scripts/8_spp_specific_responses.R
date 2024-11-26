## This script creates supplemental figures (or main text??) styled as geom_tile
## that can be used to show species-specific responses to vegetation metrics
##
## Peter Regier
## 2024-11-25
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")
p_load(broom) # loading just for this script in case it messes with other code
library(rlang)

# 2. Read in datasets ----------------------------------------------------------

## Read in raw sapflow
sapflow <- read_csv("data/241125_sapflow_by_spp.csv") %>% 
  mutate(period_fct = factor(case_when(period == "0_preflood" ~ "Preflood", 
                                       period == "2_postflood" ~ "Postflood"), 
                             levels = c("Preflood", "Postflood")))

## Read in tree GHG fluxes
tree_ghg <- read_csv(paste0(ghg_path, "ghg_fluxes_trees_processed.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(period = case_when(condition == "Preflood" ~ "0_preflood", 
                            condition == "Postflood" ~ "2_postflood")) %>% 
  select(plot, species, period, flux_co2_umol_m2_min) %>% 
  filter(plot != "Control") %>% 
  drop_na()

spp <- read_csv("data/TEMPEST_TreeChamberInstallation_11272023.csv") %>% 
  clean_names() %>% select(id, species)

veg <- read_csv("data/raw_data/vegetation/Compiled data_TEMPEST veg 2023.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(parsedate::parse_date(date))) %>% 
  mutate(plot = case_when(plot == "Fresh" ~ "Freshwater", 
                          plot == "Salt" ~ "Estuarine", 
                          TRUE ~ plot)) %>% 
  mutate(id = paste0(plot_id, stem_id)) %>% 
  left_join(., spp, by = "id") %>% 
  select(plot, species, a, gs, ci) %>% 
  drop_na()


# 3. Process effects into a plottable format -----------------------------------

## Function must have plot, species, and period columns, where period must have 
## two levels: 0_preflood and 2_postflood
calculate_stats <- function(data, var){
  
  var <- ensym(var)  # Capture the variable as a symbol
  
  stats <- data %>%
    group_by(plot, species) %>%
    summarize(mean_preflood = mean(data[[as.name(quo_text(var))]][data[["period"]] == "0_preflood"]), 
              mean_postflood = mean(data[[as.name(quo_text(var))]][data[["period"]] == "2_postflood"])) %>% 
    mutate(change = mean_postflood - mean_preflood) %>% 
    mutate(change_cat = case_when(change > 0 ~ "Increase", 
                                  change < 0 ~ "Decrease", 
                                  TRUE ~ "No change")) %>% 
    select(plot, species, change, change_cat)
  
  hsd <- data %>%
    group_by(plot, species) %>%
    do({model <- aov(UQ(var) ~ period, data = .)
    tidy(TukeyHSD(model))}) %>%
    rename("p_val" = adj.p.value) %>%
    mutate(p_cat = case_when(p_val < 0.01 ~ "**",
                             p_val < 0.05 ~ "*",
                             TRUE ~ "ns")) %>%
    select(plot, species, p_val, p_cat)
  
  inner_join(stats, hsd, by = c("plot", "species")) %>% 
    mutate(variable = as_string(var))
}


# 4. Leaf stats ----------------------------------------------------------------
## Because we don't have before/after on leaf metrics (and we only have one spp)
## the leaf metrics need to be done differently

veg_stats <- function(data, var){
  
  var <- ensym(var)  # Capture the variable as a symbol
  
  x <- data %>%
    summarize(Control = mean(data[[as.name(quo_text(var))]][data[["plot"]] == "Control"], na.rm = T), 
              Freshwater = mean(data[[as.name(quo_text(var))]][data[["plot"]] == "Freshwater"], na.rm = T), 
              Estuarine = mean(data[[as.name(quo_text(var))]][data[["plot"]] == "Estuarine"], na.rm = T))
  
  stats <- data.frame(plot = c("Freshwater", "Estuarine"),
    change = c(x$Freshwater - x$Control, x$Estuarine - x$Control)) %>% 
    mutate(change_cat = case_when(change > 0 ~ "Increase", 
                                  change < 0 ~ "Decrease", 
                                  TRUE ~ "No change"))
  
  model <- aov(eval_tidy(var) ~ plot, data = data)
  hsd <- hsd_results <- tidy(TukeyHSD(model)) %>% 
                         filter(grepl("Control", contrast)) %>% 
                         rename("p_val" = adj.p.value) %>%
    mutate(plot = str_replace_all(contrast, "-Control", "")) %>% 
    select(plot, p_val) %>% 
                         mutate(p_cat = case_when(p_val < 0.01 ~ "**",
                                                  p_val < 0.05 ~ "*",
                                                  TRUE ~ "ns")) %>%
    mutate(variable = as_string(var))
    
  inner_join(hsd, stats, by = "plot")
}

veg <- bind_rows(veg_stats(veg, a), 
          veg_stats(veg, ci), 
          veg_stats(veg, gs)) %>% 
  mutate(species = "Beech") %>% 
  select(plot, species, change_cat, p_val, p_cat, variable)


df <- bind_rows(calculate_stats(sapflow, mean_fd), 
               calculate_stats(tree_ghg, flux_co2_umol_m2_min)) %>% 
  bind_rows(veg) %>% 
  mutate(prediction = case_when(variable == "mean_fd" ~ "Decrease", 
                              variable == "gs" ~ "Decrease",
                              variable == "flux_co2_umol_m2_min" ~ "Decrease",
                              variable == "ci" ~ "Decrease",
                              variable == "a" ~ "Decrease")) %>% 
  mutate(variable = case_when(variable == "mean_fd" ~ "Sapflux density", 
                              variable == "gs" ~ "Stomatal cond.", 
                              variable == "flux_co2_umol_m2_min" ~ "Stem CO2 flux", 
                              variable == "ci" ~ "Intercellular CO2", 
                              variable == "a" ~ "Photosynthesis"))

df %>% 
  ggplot(aes(species, variable, fill = change_cat)) + 
  geom_tile(aes(color = prediction), 
            alpha = 0.5, 
            width = 0.9, height = 0.9, lwd = 2) + 
  geom_text(aes(label = p_cat), 
            vjust = 0.5, hjust = 0.5, 
            color = "black", size = 5) + 
  facet_wrap(~plot) + 
  scale_fill_manual(values = c("red", "forestgreen")) + 
  labs(x = "Tree Species", y = "Measurement", 
       color = "Prediction", fill = "Response")
ggsave("figures/241126_veg_responses_by_spp.png", width = 7, height = 6)



