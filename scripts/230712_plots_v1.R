## This script makes figures based on the v1 outline: 
## https://docs.google.com/document/d/1s8ddm-279NrkXnxSpY3PuuIdBZA50nQo2I5WmZ2zWfQ/edit
##
## Figures
## Figure 1: Map (make elsewhere)
## Figure 2: Contour plots for TEROS VWC and EC by plot
## Figure 3: Contour plots for Firesting by plot
## Figure 4: Contour plots for SWAP by plot
##
## Supplemental Figures
## Figure S1: Contour plot for TEROS Temp
## Figure S2: Boxplots comparing TEROS metrics by time-period
## Figure S3: Boxplots comparing Firesting DO by time-period
## Figure S4: Boxplots comparing SWAP redox by time-period
##
## 2023-07-12
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")

## We're piping all these figures into their own folder to keep versioning simple
output_path = "figures/230713_figures_v1/"

# 2. Read in data --------------------------------------------------------------

## Set up a helper function
read_csv_ <- function(...){read_csv(...)  %>% 
    mutate(datetime = force_tz(datetime, tzone = common_tz)) %>% 
    filter(datetime > pre_event_start & 
             datetime < post_event_end)} %>% 
  label_flood_periods()

## Load firesting data
firesting <- read_csv_("data/230712_firesting.csv")

## Load TEROS data
teros <- read_csv_("data/230712_teros_means.csv")

## Load SWAP data
swap <- read_csv_("data/230618_swap_redox_raw.csv") %>% 
  rename("depth" = depth_cm)


# 3. Create contour plots (Figures 2-4) ----------------------------------------

## Crux: how do we make the z-legend continuous instead of discrete ranges?
## Answer: metR::geom_contour_fill()
## Next question: How do we skew our breaks so that we don't default to yellow
## as the baseline color? (using Spectral palette gives this)

## Solution (janky but functional): trial and error to manually define breaks
swap_breaks <- c(-200, -100, 0, 100, 200, 300, 400, 500, 600, 800)
firesting_breaks <- c(80, 70, 60, 50, 40, 30, 20, 10)
vwc_breaks <- seq(from = 0.45, to = 0.23, length.out = 9)



plot_contours <- function(data, var, y_label, fill_direction, title){
  data %>% 
    ggplot(aes(datetime, depth)) + 
    metR::geom_contour_fill(aes(z = {{var}}), bins = 20) + 
    scale_y_reverse() + 
    scale_fill_viridis_c(option = "B", direction = fill_direction) +
    #scale_fill_fermenter(palette = "Spectral", direction = fill_direction, breaks = n_breaks) + 
    geom_vline(aes(xintercept = dump_start1), color = "white", linetype = "dashed") + 
    geom_vline(aes(xintercept = dump_start2), color = "white", linetype = "dashed") + 
    facet_wrap(~plot, ncol = 1) + 
    labs(x = "", y = "Depth (cm)", fill = y_label, title = title)
}


vwc_countours <- plot_contours(teros, vwc, "VWC (m3/m3)", 1, "TEROS - Volumetric Water Content")
ec_countours <- plot_contours(teros, ec, "EC (uS/cm)", 1, "TEROS - Conductivity")

plot_grid(vwc_countours, ec_countours)
ggsave(paste0(output_path, "Fig2_teros_contours.png"), width = 12, height = 6)

plot_contours(firesting, do_percent_sat, "DO (%)", -1, "Firesting - Dissolved Oxygen")
ggsave(paste0(output_path, "Fig3_firesting_contours.png"), width = 8, height = 6)

plot_contours(swap, redox_mv, "Redox (mv)", -1, "SWAP - Redox potential")
ggsave(paste0(output_path, "Fig4_swap_contours.png"), width = 8, height = 6)



# 4. Create boxplots -----------------------------------------------------------

make_boxplot <- function(data, var, y_label){
  
  #my_comparisons <- list(c("Pre-Flood", "Flood #1"), c("Flood #1", "Flood #2"), c("Flood #2", "Post-Flood"))
  
  ggplot(data, aes(period_relabel, {{var}})) + 
    #ggplot(data, aes(period_relabel, {{var}}, fill = as.factor(depth))) + 
    geom_boxplot() + 
    facet_wrap(~plot, ncol = 1) + 
    #scale_fill_viridis_d() + 
    labs(x = "", y = y_label, fill = "Depth (cm)") #+ 
    #stat_compare_means(comparisons = my_comparisons)
    
}

plot_grid(make_boxplot(teros, vwc, "VWC (m3/m3)"), 
          make_boxplot(firesting, do_percent_sat, "DO (%)"), 
          make_boxplot(swap, redox_mv, "Redox (mV)"), 
          nrow = 1)
ggsave(paste0(output_path, "Boxplots.png"), width = 12, height = 6)






# 5. Create supplemental plots -------------------------------------------------

## Depending how busy it gets, put time-series next to boxplots








# X. other nonsense

firesting_trim <- firesting %>% filter(depth == 5 | depth == 30) %>% 
  filter(plot != "Control") %>% 
  select(datetime, plot, depth, do_percent_sat)

teros_trim <- teros %>% filter(depth == 5 | depth == 30) %>% 
  filter(plot != "Control") %>% 
  select(datetime, plot, depth, vwc)

vwc_do <- inner_join(firesting_trim, teros_trim, by = c("datetime", "plot", "depth")) %>% 
  label_flood_periods() %>% 
  filter(grepl("#", period_relabel))

vwc_do2 <- inner_join(firesting %>% filter(depth == 5 & plot == "Freshwater") %>% select(datetime, do_percent_sat), 
                      teros %>% filter(depth == 5 & plot == "Freshwater") %>% select(datetime, vwc, tsoil, ec), 
                      by = "datetime")


x1 <- ggplot(vwc_do, 
       aes(datetime, vwc, color = as.factor(depth))) + 
  geom_line() + 
  geom_vline(xintercept = dump_start2) + 
  facet_wrap(~plot, nrow = 1)

x2 <- ggplot(vwc_do,
       aes(datetime, do_percent_sat, color = as.factor(depth))) + 
  geom_line() + 
  geom_vline(xintercept = dump_start2) + 
  facet_wrap(~plot, nrow = 1)


plot_grid(x1, x2, ncol = 1)



ggplot(vwc_do, 
       aes(datetime, vwc)) + 
  geom_line() + 
  geom_point(aes(color = do_percent_sat)) +
  geom_vline(xintercept = dump_start2) + 
  scale_color_fermenter(palette = "Spectral", direction = 1, 
                        breaks = c(0, 5, 10, 20, 50, 100)) + 
  facet_wrap(plot~depth)


ggplot(vwc_do %>% filter(plot == "Freshwater"), aes(datetime, do_percent_sat)) + 
  geom_line()



ggplot(vwc_do, aes(vwc, do_percent_sat, color = period_relabel)) + 
  geom_point() + 
  facet_wrap(depth~plot)

vwc_do %>% 
  group_by(plot, depth, period_relabel) %>% 
  summarize(start_do = first(do_percent_sat), 
            min_do = min(do_percent_sat), 
            start_time = min(datetime),
            time_min_do = datetime[which.min(do_percent_sat)],
            start_vwc = first(vwc), 
            max_vwc = max(vwc)) %>% 
  mutate(delta_do = start_do - min_do, 
         delta_vwc = max_vwc - start_vwc,
         time_to_anoxia = as.numeric(((time_min_do - start_time)))) %>% 
  filter(time_to_anoxia > 0 & time_to_anoxia < 24) %>% 
  ggplot(aes(delta_vwc, time_to_anoxia, color = plot, size = as.factor(depth))) + 
  geom_point() + 
  facet_wrap(~period_relabel)

# p_load(WaveletComp)
# 
# wc_test <- vwc_do %>% 
#   filter(depth == 5 & plot == "Freshwater")
# 
# x <- analyze.wavelet(my.data = vwc_do2, my.series = "do_percent_sat")
# 
# x <- analyze.coherency(my.data = as.data.frame(vwc_do2[,2:3]), my.pair = c(1,2))
# wc.image(x)

# Y. Create ex plots to explain contours

firesting_ex <- firesting %>% 
  filter(plot == "Freshwater")

do_timeseries <- ggplot(firesting_ex, aes(datetime, do_percent_sat, color = as.factor(depth))) + 
  geom_line() + 
  labs(x = "Date and Time", y = "DO (% Saturation)", color = "Depth (cm)")

do_contour <- ggplot(firesting_ex, aes(datetime, depth)) + 
  geom_contour_filled(aes(z = do_percent_sat), alpha = 0.5) + 
  geom_point(aes(color = do_percent_sat)) + 
  scale_color_viridis_c() + 
  labs(x = "Date and Time", y = "Depth (cm)", color = "DO (% Saturation)")

plot_grid(do_timeseries, NULL, do_contour, 
          rel_widths = c(1, 0.1, 1), nrow = 1)
ggsave(paste0(output_path, "X_contour_explanation.png"), width = 12, height = 6)









