## This script calculates metrics from the DO data for a couple things: 
## 1. Time anoxic (<1%) by plot and depth - how long were things anoxic?
## 2. DO consumption rate by plot and depth - how fast was O2 consumed?
## 3. TBD, but a third metric is likely interesting
##
## 2023-10-17
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_0_setup.R")


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/230610_firesting.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))

# 3. Calculate the time hypoxic and anoxic by flood ----------------------------

anoxia_by_plot_and_depth <- df %>% 
  filter(plot != "Control") %>% 
  group_by(period_relabel, plot, depth) %>% 
  summarize(period = first(period), 
            perc_anoxic = length(do_percent_sat[do_percent_sat < 1]) / length(do_percent_sat) * 100,
            perc_hypoxic = length(do_percent_sat[do_percent_sat < 20]) / length(do_percent_sat) * 100)


write_csv(anoxia_by_plot_and_depth, "data/231116_anoxia_by_plot_and_depth.csv")

# 4. Calculate DO consumption rates --------------------------------------------

## First, visualize the data we want to look at
df %>% 
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
df_rates_raw <- df %>% 
  filter(plot != "Control") %>% 
  filter(datetime >"2023-06-06 04:00") %>% 
  group_by(plot, depth) %>% 
  mutate(index = 1:n()) %>% 
  ungroup()

## Set up a matrix with all the plots we'll need indices for
trim_start_raw <- expand_grid(plot = unique(df_rates_raw$plot), 
                              depth = unique(df_rates_raw$depth), 
                              period_relabel = c("Flood #1", "Flood #2")) 

i = 15
## Plotly plot to examine and ID points where we want to trim each value
p <- df_rates_raw %>% 
  filter(plot == trim_start_raw$plot[[i]] & 
           depth == trim_start_raw$depth[[i]]) %>% 
  ggplot(aes(index, do_percent_sat, label = datetime)) + 
  geom_line() + 
  geom_point(alpha = 0.5)

ggplotly(p)

## Comment out when to start and stop each: 1) Start (inclusive), 2) Stop (inclusive)
### FW-5-1: 58, 173 (115)
### FW-5-2: 350, 451 (101)
### FW-10-1: 57, 197 (140)
### FW-10-2: 349, 413 
### FW-20-1: 57, 177
### FW-20-2: 347, 409
### FW-30-1: 57, 201
### FW-30-2: NA, NA
### SW-5-1: 57, 199
### SW-5-2: 347, 430
### SW-10-1: 57, 225
### SW-10-2: 347, 501
### SW-20-1: 57 (based partially on previous values), 272 
### SW-20-2: 347, 561 (this one is an unclear judgement call)
### SW-30-1: 58, 457
### SW-30-2: NA, NA 

## vectorize notes above
start_index = c(58, 350, 57, 349, 57, 347, 57, NA,
                57, 347, 57, 347, 57, 347, 58, NA)
end_index = c(173, 451, 197, 413, 177, 409, 201, NA, 
              199, 430, 225, 501, 272, 561, 457, NA)

## Combine for a list we can use to trim our dataset
trim_starts <- trim_start_raw %>% 
  mutate(start_index = start_index, 
         end_index = end_index) %>% 
  drop_na()

## Create dataset to visualize (note: inner_join is doubling data because we are
## not joining by period_relabel, which we can't do because then it cuts off the
## the 30cm DO). It's okay, because we're summarizing it below so it never actually
## has an impact on the data
df_rates <- inner_join(df_rates_raw %>% select(-period_relabel), 
                        trim_starts, 
                        by = c("plot", "depth")) %>% 
  group_by(plot, depth) %>% 
  mutate(consumption = ifelse(index >= start_index & index <= end_index, TRUE, FALSE)) %>% 
  ungroup() %>% 
  unique()



write_csv(df_rates, "data/231116_do_consumption_rates.csv")

ggplot(data = df_rates, aes(datetime, do_percent_sat)) + 
  geom_line() + 
  geom_point(data = df_rates %>% filter(consumption == TRUE), aes(color = period_relabel)) + 
  facet_wrap(depth~plot)

## Now, let's create rates
rate_stats <- df_rates %>% 
  filter(consumption == TRUE) %>% 
  group_by(plot, depth, period_relabel) %>% 
  dplyr::summarize(max_do = max(do_percent_sat), 
                   min_do = min(do_percent_sat), 
                   start = first(index), 
                   end = last(index)) %>% 
  ungroup() %>% 
  add_row(plot = "Freshwater", ## Add 30cm FW for Flood #2
          depth = 30,
          period_relabel = "Flood #2",
          max_do = 0,
          min_do = 0,
          start = 0,
          end = 1)  %>% 
  add_row(plot = "Seawater", ## Add 30cm FW for Flood #2
          depth = 30,
          period_relabel = "Flood #2",
          max_do = 0,
          min_do = 0,
          start = 0,
          end = 1) %>% 
  mutate(delta_do = max_do - min_do, 
         delta_index = end - start, 
         delta_hrs = delta_index / 12, 
         do_perc_hr = delta_do / delta_hrs)

write_csv(rate_stats, "data/231116_rate_stats.csv")

# 5. Create / combine plots and export -----------------------------------------

## Set up a helper function that standardizes what can be across all plots 
common_plot_setup <- function(...) {
  p <- ggplot(...) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d() +
    facet_wrap(~plot, ncol = 1) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
          plot.title = element_text(hjust = 0.5))
  
  return(p)
}

## Make individual plots
p_rates <- common_plot_setup(rate_stats, aes(period_relabel, do_perc_hr, fill = as.factor(depth))) + 
  labs(x = "", y = "DO Consumption rates", fill = "Depth (cm)", title = "DO consumption (% / hr)")

p_hypoxic <- common_plot_setup(data = anoxia_by_plot_and_depth, aes(x = period_relabel, y = perc_hypoxic, fill = as.factor(depth))) +
  labs(x = "", y = "% of time hypoxic", fill = "Depth (cm)", title = "Hypoxia (DO < 20%)")

p_anoxic <- common_plot_setup(anoxia_by_plot_and_depth, aes(period_relabel, perc_anoxic, fill = as.factor(depth))) + 
  labs(x = "", y = "% of time anoxic", fill = "Depth (cm)", title = "Anoxia (DO < 1%)")

## Pull label
legend <- cowplot::get_legend(p_rates)

## Create combined plot
plot_grid(p_rates + theme(legend.position = "none"), 
          NULL,
          p_hypoxic + theme(legend.position = "none"), 
          NULL,
          p_anoxic + theme(legend.position = "none"), 
          legend, 
          rel_widths = c(1, 0.1, 1, 0.1, 1, 0.3),
          labels = c("A", "", "B", "", "C", ""),
          nrow = 1)
ggsave("figures/231019_hypoxic_anoxic_and_rates.png", width = 12, height = 6)

anoxia_by_plot_and_depth %>% 
  group_by(period_relabel, plot) %>% 
  summarize(mean(perc_anoxic))

