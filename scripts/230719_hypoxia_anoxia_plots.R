## Let's explore the DO data more quantitatively in a couple different ways
##
## 2023-07-19
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/230610_firesting.csv") %>% 
  mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
  label_flood_periods() %>% 
  mutate(period_relabel = fct_relevel(period_relabel, "Pre-Flood", "Flood #1", "Flood #2", "Post-Flood"))

# 3. Calculate the time hypoxic and anoxic by flood ----------------------------

anoxia_by_plot <- df %>% 
  filter(plot != "Control") %>% 
  group_by(period_relabel, plot) %>% 
  summarize(period = first(period), 
            perc_anoxic = length(do_percent_sat[do_percent_sat < 1]) / length(do_percent_sat) * 100,
            perc_hypoxic = length(do_percent_sat[do_percent_sat < 20]) / length(do_percent_sat) * 100)
  
anoxia_by_plot_and_depth <- df %>% 
  filter(plot != "Control") %>% 
  group_by(period_relabel, plot, depth) %>% 
  summarize(period = first(period), 
            perc_anoxic = length(do_percent_sat[do_percent_sat < 1]) / length(do_percent_sat) * 100,
            perc_hypoxic = length(do_percent_sat[do_percent_sat < 20]) / length(do_percent_sat) * 100) 

p_hypoxic <- ggplot(anoxia_by_plot_and_depth, aes(period_relabel, perc_hypoxic, fill = as.factor(depth))) + 
  geom_col(position = "dodge") + 
  facet_wrap(~plot, nrow = 2, scales = "free_x") + 
  scale_fill_viridis_d() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "% of time hypoxic", fill = "Depth (cm)", title = "Hypoxia (DO < 20%)")

p_anoxic <- ggplot(anoxia_by_plot_and_depth, aes(period_relabel, perc_anoxic, fill = as.factor(depth))) + 
  geom_col(position = "dodge") + 
  facet_wrap(~plot, nrow = 2, scales = "free_x") + 
  scale_fill_viridis_d() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "% of time anoxic", fill = "Depth (cm)", title = "Anoxia (DO < 1%)")

plot_grid(p_hypoxic, p_anoxic, 
          ncol = 2)
ggsave("figures/230719_percent_hypoxic_anoxic.png", width = 9, height = 6)


