## This script analyzes Firesting data to determine drawdown rates
## 
## 2023-06-10
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/230610_firesting.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz))

## Plot for visualizing dataset and trimming events
p = ggplot(df, aes(datetime, do_percent_sat, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(~plot, ncol = 1)

ggplotly(p)


# 3. Parse first flood event ---------------------------------------------------

## create a list of datetimes to trim to just the decreasing limb
flood1_crop <- tibble(plot = c("FW", "FW", "FW", "FW", "SW", "SW", "SW", "SW"),
                      depth = c(5, 10, 20, 30, 5, 10, 20, 30),
                      start = c("2023-06-06 04:50", #fw 5
                                "2023-06-06 05:00", #fw 10
                                "2023-06-06 04:55", #fw 20
                                "2023-06-06 04:55", #fw 30
                                "2023-06-06 04:45", #sw 5
                                "2023-06-06 04:45", #sw 10
                                "2023-06-06 04:45", #sw 20
                                "2023-06-06 04:50"), 
                      end = c("2023-06-06 18:45", #fw 5
                              "2023-06-06 19:25", #fw 10
                              "2023-06-07 01:10", #fw 20
                              "2023-06-07 05:15", #fw 30
                              "2023-06-06 16:35", #sw 5
                              "2023-06-06 18:45", #sw 10
                              "2023-06-07 00:10", #sw 20
                              "2023-06-08 17:15")) %>% 
  mutate(start = force_tz(parsedate::parse_date(start), tzone = common_tz), 
         end = force_tz(parsedate::parse_date(end), tzone = common_tz))

flood1 <- inner_join(df, flood1_crop, by = c("plot", "depth")) %>% 
  filter(datetime >= start) %>% 
  filter(datetime <= end) 


# 4. Parse second flood event --------------------------------------------------

## create a list of datetimes to trim to just the decreasing limb
flood2_crop <- tibble(plot = c("FW", "FW", "FW", "SW", "SW", "SW"),
                      depth = c(5, 10, 20, 5, 10, 20),
                      start = c("2023-06-07 05:10", #fw 5
                                "2023-06-07 05:10", #fw 10
                                "2023-06-07 05:00", #fw 20
                                "2023-06-07 04:45", #sw 5
                                "2023-06-07 04:45", #sw 10
                                "2023-06-07 04:45"), #sw 20
                      end = c("2023-06-07 19:50", #fw 5
                              "2023-06-07 21:10", #fw 10
                              "2023-06-08 03:40", #fw 20
                              "2023-06-07 18:10", #sw 5
                              "2023-06-07 23:15", #sw 10
                              "2023-06-08 07:55")) %>%  #sw 20
  mutate(start = force_tz(parsedate::parse_date(start), tzone = common_tz), 
         end = force_tz(parsedate::parse_date(end), tzone = common_tz))

flood2 <- inner_join(df, flood2_crop, by = c("plot", "depth")) %>% 
  filter(datetime >= start) %>% 
  filter(datetime <= end) 


# 5. Plot DO consumption rates -------------------------------------------------

floods <- bind_rows(flood1 %>% mutate(event = "Flood 1"), 
                    flood2 %>% mutate(event = "Flood 2"))

flood_rates <- floods %>% 
  filter(do_percent_sat > 5) %>% ## Need to double-check, this should be based on sensor sensitivity
  group_by(plot, depth, event) %>% 
  summarize(start_time = min(datetime), 
            end_time = max(datetime), 
            start_do = first(do_percent_sat), 
            end_do = last(do_percent_sat)) %>% 
  mutate(delta_time = as.numeric(end_time - start_time), 
         delta_do = start_do - end_do, 
         delta_do_perc_per_hr = delta_do / delta_time) ## DO consumption rate in percent/L/hr


ggplot(floods %>% filter(do_percent_sat > 5), aes(datetime, do_percent_sat, color = as.factor(depth))) + 
  geom_line() + 
  facet_wrap(event~plot, scales = "free")


ggplot(flood_rates, aes(as.factor(depth), delta_do_perc_per_hr, fill = plot)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~event, nrow = 1)



# 6. Time hypoxic / anoxic plots -----------------------------------------------

x <- df %>% 
  filter(plot == "FW" & depth == 5)

df %>% 
  group_by(datetime, plot) %>%
  summarize(do_percent_sat = mean_(do_percent_sat)) %>% 
  ggplot(aes(datetime, do_percent_sat, color = plot)) + 
  geom_line() + 
  geom_vline(aes(xintercept = dump_start1), color = "black", linetype = "dashed") + 
  geom_vline(aes(xintercept = dump_start2), color = "black", linetype = "dashed") + 
  geom_hline(aes(yintercept = 20)) + 
  labs(x = "", y = "DO (% Sat)", color = "")
ggsave("figures/230610_mean_do_by_plot.png", width = 8, height = 4)


anoxia_stats <- df %>% 
  filter(datetime > flood1_start) %>% 
  group_by(plot, depth) %>% 
  summarize(n_hypoxic = 100 * (sum(do_percent_sat < 20)/n()), 
            n_anoxic = 100 * (sum(do_percent_sat < 5)/n())) 

anoxic_plot <- ggplot(anoxia_stats, aes(plot, n_anoxic, fill = as.factor(depth))) + 
  geom_col(position = "dodge") + 
  labs(x = "Plot", y = "% of time < 5%") + 
  ylim(0, 90)

hyoxic_plot <- ggplot(anoxia_stats, aes(plot, n_hypoxic, fill = as.factor(depth))) + 
  geom_col(position = "dodge") + 
  labs(x = "Plot", y = "% of time < 20%") + 
  ylim(0, 90)

plot_grid(hyoxic_plot, anoxic_plot, nrow = 1)
ggsave("figures/230610_time_anoxic.png", width = 8, height = 4)




