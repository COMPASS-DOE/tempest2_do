








source("scripts/0_0_setup.R")

teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm)

p_load(#lme4,
       nlme,
       multcomp,
       robustlmm,
       emmeans)

x <- teros %>% #filter(plot == "Freshwater") %>% 
  label_flood_periods() %>% 
  filter(!is.na(period))

model <- lme(ec ~ period * plot, random = ~ 1 | plot/depth, data = x)

emmeans_result <- emmeans(model, pairwise ~ period | plot)

cld_result <- cld(emmeans_result, Letters = letters)

cld_df <- as.data.frame(cld_result)

shapiro.test(resid(model))



summary(emmeans_result)

model <- lmer(vwc ~ period + (1|depth) + (1|datetime_est), data = x)
#model <- lmer(vwc ~ period + (1|plot/depth) + (1|datetime_est), data = x)

shapiro.test(x$vwc)

hist(x$vwc)