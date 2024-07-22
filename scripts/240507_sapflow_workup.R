

require(pacman)
p_load(tidyverse, 
       cowplot,
       ggtukey)

theme_set(theme_bw())

df <- read_csv("data/240507_sapflow_final.csv") %>% 
  mutate(datetime_est = force_tz(datetime_est, tzone = common_tz)) %>% 
  dplyr::select(datetime_est, plot, sapflow_avg) %>%  
  #pivot_wider(names_from = plot, values_from = sapflow_avg) %>% 
  filter(datetime_est >= dump_start1 - hours(24) & 
           datetime_est <= dump_start2 + hours(48))


df_n <- inner_join(df %>% filter(plot == "Control") %>% 
                    select(-plot) %>% 
                    rename("sapflow_control" = sapflow_avg), 
                  df %>% filter(plot != "Control"), 
                  by = "datetime_est") %>% 
  mutate(n_sapflow = sapflow_avg - sapflow_control) %>% 
  mutate(period = case_when(datetime_est < dump_start1 ~ "1_preflood", 
                            datetime_est >= dump_start1 & datetime_est < dump_start2 ~ "2_flood1", 
                            datetime_est >= dump_start2 & datetime_est < dump_start2 + hours(24) ~ "3_flood2", 
                            datetime_est >= dump_start2 + hours(24) & datetime_est < dump_start2 + hours(48) ~ "4_postflood",
                            TRUE ~ NA)) %>% 
  filter(!is.na(period))
         

p1 <- ggplot(df, aes(x = datetime_est, y = sapflow_avg, color = plot)) + 
  geom_line() + 
  labs(x = "", y = "Avg sapflow")

p2 <- ggplot(df_n, aes(x = period, y = n_sapflow, fill = period)) + 
  geom_boxplot(width = 0.8, show.legend = F) + 
  facet_wrap(~plot, ncol = 1) + 
  geom_tukey(where = "whisker") + 
  scale_x_discrete(labels = unique(df_n$period)) + 
  scale_fill_viridis_d() + 
  labs(x = "", y = "Normalized sapflow (mV)")

plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 0.6))
ggsave("figures/7_Fig7_sapflow.png", width = 9, height = 5)
  


##############
##############
##############

z_stats <- df %>% 
  filter(plot == "Control") %>% 
  filter(datetime_est >= dump_start1 - hours(24) & 
           datetime_est < dump_start1) %>% 
  ungroup() %>% 
  summarize(vpre_mean = mean(sapflow_avg, na.rm = T), 
            vpre_sd = sd(sapflow_avg, na.rm = T))

z_df <- df %>% 
  #filter(plot != "Control") %>% 
  ungroup() %>% 
  group_by(plot) %>% 
  mutate(z_sapflow = (sapflow_avg - z_stats$vpre_mean) / z_stats$vpre_sd) 


n_df <- inner_join(z_df %>% filter(plot == "Control") %>% 
                     rename("z_sapflow_control" = z_sapflow) %>% 
                     ungroup() %>% 
                     dplyr::select(datetime_est, z_sapflow_control), 
                   z_df %>% filter(plot != "Control"), 
                   by = "datetime_est") %>% 
  mutate(n_sapflow = z_sapflow - z_sapflow_control) 

ggplot(n_df, aes(datetime_est, n_sapflow, color = plot)) + 
  geom_line() + 
  geom_vline(xintercept = dump_start1) + 
  geom_vline(xintercept = dump_start2)