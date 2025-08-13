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

medians_and_ranges <- read_csv("data/stats_by_depth_and_period.csv")
friedman <- read_csv("data/250410_sensor_friedman_results.csv")
wilcoxon <- read_csv("data/250410_sensor_wilcoxon_results.csv")
sapflux_slopes_roll <- read_csv("data/250502_sapflux_slopes_roll.csv")
#sapflux <- read_csv("data/250502_2023_sapflow.csv")


# 3. Table S1 - pairwise comparisons -------------------------------------------

## Filter to combinations of variable, depth, and period with significance
significant_friedman <- friedman %>% 
  filter(p_friedman < 0.05)

friedman_wide <- friedman %>% 
  mutate(p_cat = case_when(p_friedman > 0.05 ~ "ns", 
                           p_friedman < 0.001 ~ "***", 
                           p_friedman < 0.01 ~ "**", 
                           p_friedman < 0.05 ~ "*")) %>%
  dplyr::select(variable, depth, period2, p_cat) %>%
  pivot_wider(names_from = period2, values_from = p_cat) %>% 
  mutate(variable = factor(case_when(variable == "vwc_n" ~ "VWC (m3/m3)", 
                                     variable == "ec_n" ~ "EC (uS/cm)", 
                                     variable == "do_percent_sat_n" ~ "DO (%sat)", 
                                     variable == "eh_mv_n" ~ "Eh (mV)"))) %>% 
  mutate(variable = fct_relevel(variable, "VWC (m3/m3)", "EC (uS/cm)"))

# Define display column names
friedman_display_col_names <- c("Variable", "Depth", "Pre-flood", "Flood", "Post-flood")

# Define periods and header
#friedman_period_and_counts <- c(3, 3, 3) # Example values, adjust accordingly
#friedman_periods <- c("1_preflood", "2_flood", "3_postflood")

# Create styled table
table_s1 <- kable(friedman_wide, format = "html", escape = FALSE, col.names = friedman_display_col_names) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "10em", extra_css = "text-align: center;") %>%
  column_spec(2:ncol(friedman_wide), width = "15em", extra_css = "text-align: center;") %>%
  row_spec(0, extra_css = "text-align: center;") %>%
  add_header_above(c("Table S1 - Friedman test results" = length(friedman_display_col_names)), bold = TRUE, align = "c")

# Save the table as an HTML file
save_kable(table_s1, "table_s1.html")

# Convert the HTML table to an image
webshot("table_s1.html", "figures/supplemental/table_s1.png", selector = "table", 
        vwidth = 1500, vheight = 600)


# 4. Table S2: Wilcoxon results for selected pairs -----------------------------

## L103 - VWC, flooded
## L109 - EC, flooded
## L138 - DO, flooded

wilcoxon_wide <- wilcoxon %>% 
  filter(variable == "vwc_n" & period2 == "2_flood" | 
           variable == "ec_n" & period2 == "2_flood" |
           variable == "do_percent_sat_n" & period2 == "2_flood" |
           variable == "eh_mv_n" & period2 == "2_flood") %>% 
  mutate(p_cat = case_when(p_adj > 0.05 ~ "ns", 
                           p_adj < 0.001 ~ "***", 
                           p_adj < 0.01 ~ "**", 
                           p_adj < 0.05 ~ "*")) %>% 
  mutate(group2 = case_when(group2 == "Estuarine" ~ "Saltwater", 
                            TRUE ~ group2)) %>% 
  dplyr::select(variable, depth, period2, group1, group2, p_cat) %>%
  pivot_wider(names_from = period2, values_from = p_cat) %>% 
  mutate(variable = factor(case_when(variable == "vwc_n" ~ "VWC (m3/m3)", 
                                     variable == "ec_n" ~ "EC (uS/cm)", 
                                     variable == "do_percent_sat_n" ~ "DO (%sat)", 
                                     variable == "eh_mv_n" ~ "Eh (mV)"))) %>% 
  mutate(variable = fct_relevel(variable, "VWC (m3/m3)", "EC (uS/cm)")) %>% 
  filter(group1 == "Control") 

# Define display column names
wilcoxon_display_col_names <- c("Variable", "Depth", "Group 1", "Group 2", "Flood")

# Create styled table
table_s2 <- kable(wilcoxon_wide, format = "html", escape = FALSE, col.names = wilcoxon_display_col_names) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "10em", extra_css = "text-align: center;") %>%
  column_spec(2:ncol(wilcoxon_wide), width = "15em", extra_css = "text-align: center;") %>%
  row_spec(0, extra_css = "text-align: center;") %>%
  add_header_above(c("Table S2 - Wilcoxon test results" = length(wilcoxon_display_col_names)), bold = TRUE, align = "c")

# Save the table as an HTML file
save_kable(table_s2, "table_s2.html")

# Convert the HTML table to an image
webshot("table_s2.html", "figures/supplemental/table_s2.png", selector = "table", 
        vwidth = 1500, vheight = 600)


# 5. Table S3: Means and ranges by period --------------------------------------

table_s3_data <- medians_and_ranges %>% 
  mutate(plot = ifelse(plot == "Estuarine", "Saltwater", plot)) %>% 
  dplyr::select(-depth) %>% 
  group_by(plot, period2, variable) %>% 
  summarize(min = min(min, na.rm = T), 
            median = median(median, na.rm = T), 
            max = max(max, na.rm = T)) %>% 
  mutate(min = sprintf('%.2f',min), 
         median = sprintf('%.2f',median), 
         max = sprintf('%.2f',max)) %>% 
  mutate(median_range = paste0(median, " (", min, "-", max, ")")) %>% 
  mutate(period = factor(case_when(period2 == "1_preflood" ~ "Pre-flood", 
                                   period2 == "2_flood" ~ "Flood", 
                                   period2 == "3_postflood" ~ "Post-flood"))) %>% 
  mutate(period_fct = fct_relevel(period, "Pre-flood")) %>% 
  mutate(variable = factor(case_when(variable == "vwc" ~ "VWC (m3/m3)", 
                                     variable == "ec" ~ "EC (uS/cm)", 
                                     variable == "do_percent_sat" ~ "DO (%sat)", 
                                     variable == "eh_mv" ~ "Eh (mV)"))) %>% 
  mutate(variable = fct_relevel(variable, "VWC (m3/m3)", "EC (uS/cm)"))


pivot_data <- table_s3_data %>%
  pivot_wider(id_cols = c(variable), names_from = c(period, plot), values_from = median_range) %>%
  arrange(variable)

# Extract unique period and plot combinations
periods <- unique(table_s3_data$period)
plots <- unique(table_s3_data$plot)

# Number of columns per period for header spans
period_and_counts <- sapply(periods, function(period) {
  length(unique(table_s3_data %>% filter(period == !!period) %>% pull(plot)))
})

# Display column names without period prefixes
display_col_names <- c("Variable", rep(plots, times = length(periods)))

formatted_data <- pivot_data %>% 
  mutate(across(everything(), ~ str_replace_all(.x, "(\\(.*?\\))", "<br>\\1")))

# Generate the kable table with nested headers
table_s3 <- kable(formatted_data, format = "html", escape = FALSE, col.names = display_col_names) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "10em", extra_css = "text-align: center;") %>%
  column_spec(2:ncol(pivot_data), width = "15em", extra_css = "text-align: center;") %>% 
  row_spec(0, extra_css = "text-align: center;") %>%
  add_header_above(c(" " = 1, setNames(period_and_counts, periods))) %>% 
  add_header_above(c("Table S3 - medians and ranges of sensor measurements" = length(display_col_names)), bold = TRUE, align = "c")

# Save the table as an HTML file
save_kable(table_s3, "table_s3.html")

# Convert the HTML table to an image
webshot("table_s3.html", "figures/supplemental/table_s3.png", selector = "table", 
        vwidth = 1500, vheight = 600)


# 6. Table S4: Sapflow slopes --------------------------------------------------

sapflux_slopes_table <- sapflux_slopes_roll %>% 
  mutate(p_value = case_when(p > 0.05 ~ "ns", 
                           p < 0.001 ~ "***", 
                           p < 0.01 ~ "**", 
                           p < 0.05 ~ "*")) %>% 
  mutate(slope = round(slope, 6)) %>% 
  dplyr::select(plot, species, slope, p_value)

sapflux_colnames <- c("Plot", "Species", "Sen-Theil slope", "P-value")

table_s4 <- kable(sapflux_slopes_table, format = "html", escape = FALSE, col.names = sapflux_colnames) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(1, width = "10em", extra_css = "text-align: center;") %>%
  column_spec(2:4, width = "15em", extra_css = "text-align: center;") %>% 
  row_spec(0, extra_css = "text-align: center;")

# Save the table as an HTML file
save_kable(table_s4, "table_s4.html")

# Convert the HTML table to an image
webshot("table_s4.html", "figures/supplemental/table_s4.png", selector = "table", 
        vwidth = 900, vheight = 600)



