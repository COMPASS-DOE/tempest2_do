## This script calculates statistics and constructs all supplemental tables
## Datasets are written from scripts/1_4_sensor_statistics.Rmd
##
## Peter Regier
## 2025-04-11
##
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup script
source("scripts/0_0_setup.R")

p_load(gt, 
       kable, 
       kableExtra, 
       webshot)

# 2. Read in data --------------------------------------------------------------

means_and_ranges <- read_csv("data/stats_by_depth_and_period.csv")
friedman <- read_csv("data/250410_sensor_friedman_results.csv")
wilcoxon <- read_csv("data/250410_sensor_wilcoxon_results.csv")


# 4. Table S1 - pairwise comparisons -------------------------------------------

## Filter to combinations of variable, depth, and period with significance
significant_friedman <- friedman %>% 
  filter(p_friedman < 0.05)

## Turns out it's all of them, but glad we checked still
filtered_wilcoxon <- wilcoxon %>%
  semi_join(significant_friedman, by = c("variable", "depth", "period2"))


# 4. Table S2: Means and ranges by period --------------------------------------

table_s2_data <- means_and_ranges %>% 
  dplyr::select(-depth) %>% 
  group_by(plot, period2, variable) %>% 
  summarize(min = min(min, na.rm = T), 
            mean = mean(mean, na.rm = T), 
            max = max(max, na.rm = T)) %>% 
  mutate(min = sprintf('%.2f',min), 
         mean = sprintf('%.2f',mean), 
         max = sprintf('%.2f',max)) %>% 
  mutate(mean_range = paste0(mean, " (", min, "-", max, ")")) %>% 
  mutate(period = factor(case_when(period2 == "1_preflood" ~ "Pre-flood", 
                                   period2 == "2_flood" ~ "Flood", 
                                   period2 == "3_postflood" ~ "Post-flood"))) %>% 
  mutate(period_fct = fct_relevel(period, "Pre-flood")) %>% 
  mutate(variable = factor(case_when(variable == "vwc" ~ "VWC (m3/m3)", 
                                     variable == "ec" ~ "EC (uS/cm)", 
                                     variable == "do_percent_sat" ~ "DO (%sat)", 
                                     variable == "eh_mv" ~ "Eh (mV)"))) %>% 
  mutate(variable = fct_relevel(variable, "VWC (m3/m3)", "EC (uS/cm)"))


pivot_data <- table_s2_data %>%
  pivot_wider(id_cols = c(variable), names_from = c(period, plot), values_from = mean_range) %>%
  arrange(variable)

# Extract unique period and plot combinations
periods <- unique(table_s2_data$period)
plots <- unique(table_s2_data$plot)

# Number of columns per period for header spans
period_and_counts <- sapply(periods, function(period) {
  length(unique(table_s2_data %>% filter(period == !!period) %>% pull(plot)))
})

# Display column names without period prefixes
display_col_names <- c("Variable", rep(plots, times = length(periods)))

formatted_data <- pivot_data %>% 
  mutate(across(everything(), ~ str_replace_all(.x, "(\\(.*?\\))", "<br>\\1")))

# Generate the kable table with nested headers
table_s2 <- kable(formatted_data, format = "html", escape = FALSE, col.names = display_col_names) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "10em", extra_css = "text-align: center;") %>%
  column_spec(2:ncol(pivot_data), width = "15em", extra_css = "text-align: center;") %>%
  add_header_above(c(" " = 1, setNames(period_and_counts, periods))) %>% 
  add_header_above(c("Table S2 - means and ranges of sensor measurements" = length(display_col_names)), bold = TRUE, align = "c")

# Save the table as an HTML file
save_kable(table_s2, "table_s2.html")

# Convert the HTML table to an image
webshot("table_s2.html", "figures/supplemental/table_s2.png", selector = "table", 
        vwidth = 1500, vheight = 600)












# X. Table S1 by depth: Means and ranges by period (not included)  -------------

table_s1_by_depth_data <- table_s1_stats %>% 
  mutate(min = sprintf('%.2f',min), 
         mean = sprintf('%.2f',mean), 
         max = sprintf('%.2f',max)) %>% 
  mutate(mean_range = paste0(mean, " (", min, "-", max, ")")) %>% 
  mutate(period = factor(case_when(period2 == "1_preflood" ~ "Pre-flood", 
                period2 == "2_flood" ~ "Flood", 
                period2 == "3_postflood" ~ "Post-flood"))) %>% 
  mutate(period_fct = fct_relevel(period, "Pre-flood")) %>% 
  mutate(variable = factor(case_when(variable == "vwc" ~ "VWC (m3/m3)", 
                              variable == "ec" ~ "EC (uS/cm)", 
                              variable == "do_percent_sat" ~ "DO (%sat)", 
                              variable == "eh_mv" ~ "Eh (mV)"))) %>% 
  mutate(variable = fct_relevel(variable, "VWC (m3/m3)", "EC (uS/cm)")) 


pivot_data_by_depth <- table_s1_by_depth_data %>%
  pivot_wider(id_cols = c(variable, depth), names_from = c(period, plot), values_from = mean_range) %>%
  arrange(variable, depth)

# Extract unique period and plot combinations
periods_by_depth <- unique(table_s1_by_depth_data$period)
plots_by_depth <- unique(table_s1_by_depth_data$plot)

# Number of columns per period for header spans
period_and_counts_by_depth <- sapply(periods_by_depth, function(period) {
  length(unique(table_s1_by_depth_data %>% filter(period == !!period) %>% pull(plot)))
})

# Display column names without period prefixes
display_col_names_by_depth <- c("Variable", "Depth", rep(plots_by_depth, times = length(periods_by_depth)))

# Generate the kable table with nested headers
table_s1_by_depth <- kable(pivot_data_by_depth, format = "html", escape = FALSE, col.names = display_col_names_by_depth) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3:ncol(pivot_data), width = "15em") %>%
  add_header_above(c(" " = 2, setNames(period_and_counts_by_depth, periods_by_depth))) %>% 
  add_header_above(c("Table S1 - means and ranges of sensor measurements" = length(display_col_names_by_depth)), bold = TRUE, align = "c")

# Save the table as an HTML file
save_kable(table_s1_by_depth, "table_s1_by_depth.html")

# Convert the HTML table to an image
webshot("table_s1_by_depth.html", "figures/supplemental/table_s1_by_depth.png", selector = "table", 
        vwidth = 2100, vheight = 900)











pivot_data <- table_s1_data %>%
  pivot_wider(id_cols = c(variable, depth), names_from = c(period_fct, plot), values_from = mean_range) %>%
  arrange(variable, depth)

period_plot_combinations <- unique(table_s1_data %>% 
                                     unite(period_plot, period_fct, plot, sep = "_") %>%
                                     pull(period_plot))

# Generate the appropriate column names
col_names <- c("Variable", "Depth", period_plot_combinations)

# Generate the header names for kableExtra
period_headers <- unique(table_s1_data$period_fct)
plot_counts <- table_s1_data %>%
  group_by(period_fct, plot) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  count(period_fct) %>%
  pull(n)

# Generate the kable table with nested headers
kable(pivot_data, format = "html", escape = FALSE, col.names = col_names) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 2, setNames(plot_counts, period_headers)))




ggplot(x, aes(period_fct, mean)) + geom_boxplot()

kbl(table_s1_data)







calculate_stats <- function(data, var){
  
  var_name <- as.character(substitute(var))
  var <- enquo(var) # Capture the variable name
  
  data %>% 
    filter(!is.na(period)) %>% 
    group_by(plot, depth, period) %>% 
    summarize(mean_var = median(!!var, na.rm = TRUE)) %>%
    pivot_wider(names_from = "period", values_from = mean_var) %>%
    ungroup() %>%
    mutate(Change = ((Flood - ` Pre-flood`)/` Pre-flood`) * 100) %>%
    dplyr::select(-depth) %>%
    group_by(plot) %>% 
    summarize(across(where(is.numeric), median)) %>% 
    mutate(variable = var_name)
}

bind_rows(calculate_stats(teros, vwc), 
          calculate_stats(teros, ec), 
          calculate_stats(firesting, do_percent_sat), 
          calculate_stats(swap, eh_mv))


teros %>% 
  filter(!is.na(period)) %>% 
  group_by(plot, depth, period) %>% 
  summarize(vwc = mean(vwc)) %>% 
  pivot_wider(names_from = "period", values_from = vwc) %>% 
  ungroup() %>% 
  mutate(Change = ((Flood - ` Pre-flood`)/` Pre-flood`) * 100) %>% 
  dplyr::select(-depth) %>% 
  group_by(plot) %>% 
  summarize(across(where(is.numeric), mean))



teros %>% 
  filter(!is.na(period)) %>% 
  group_by(plot, depth, period) %>% 
  summarize(vwc = mean(vwc)) 




