
source("scripts/0_0_setup.R")

teros <- prep_csvs("data/240326_teros_final.csv")
firesting <- prep_csvs("data/240318_firesting_final.csv")
swap <- prep_csvs("data/240404_swap_final.csv") %>% 
  rename("depth" = depth_cm)


# 2. Read in soil and tree fluxes and concentrations ---------------------------

ghg_path = "data/raw_data/ghgs/"

soil_ghg_flux <- read_csv(paste0(ghg_path, "ghg_fluxes_soil_nickworking.csv")) %>% 
  clean_names() %>% 
  filter(!is.na(timepoint)) %>% 
  mutate(timepoint_dbl = str_sub(timepoint, 2)) %>% 
  rename("co2" = co2_umol_m2_s) %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "-", ""))) %>% 
  mutate(condition = fct_relevel(condition, "Preflood")) %>% 
  mutate(condition = case_when(condition == "Preflood" ~ "0_preflood", 
                               condition == "Flooded" ~ "1_flood",
                               condition == "Postflood" ~ "2_postflood"))


## New label_flood_periods that's simplified
label_flood_periods2 <- function(data){ 
  data %>% 
    #mutate(datetime = force_tz(datetime_est, tz = common_tz)) %>% 
    mutate(period = case_when(datetime_est >= dump_start1 - hours(24) & datetime_est < dump_start1 ~ "0_preflood", 
                              datetime_est >= dump_start1 & datetime_est < dump_end2 ~ "1_flood", 
                              datetime_est >= dump_end2 & datetime_est < dump_end2 + hours(24) ~ "2_postflood", 
                              TRUE ~ NA)) 
}





library(nlme)

x <- teros %>% 
  filter(plot == "Freshwater") %>% 
  filter(depth == 5) %>% 
  label_flood_periods2() %>% 
  filter(!is.na(period)) %>% 
  mutate(date_round = round_date(datetime_est, "1 hour")) %>% 
  group_by(date_round) %>% 
  summarize(period = first(period), 
            ec = mean(ec, na.rm =T),
            vwc = mean(vwc, na.rm =T)) 

x$obs_id <- as.factor(seq(nrow(x)))
x$date_round <- as.POSIXct(x$date_round)
x$period <- factor(x$period, levels = c("0_preflood", "1_flood", "2_postflood"))

gls_model <- gls(vwc ~ period, data = x, correlation = corARMA(form = ~ as.numeric(date_round), p = 2))

gls_residuals_ar2 <- residuals(gls_model, type = "normalized")
qqnorm(gls_residuals_ar2)
qqline(gls_residuals_ar2)
shapiro.test(gls_residuals_ar2)

plot(x$date_round, gls_residuals_ar2, type = "b", main = "Residuals Over Time")



install.packages("mgcv")
library(mgcv)

# Fit GAM model
gam_model <- gam(vwc ~ s(as.numeric(date_round)) + period, data = x)

# Summary of the new model
summary(gam_model)

# Normality Check
gam_residuals <- residuals(gam_model)
qqnorm(gam_residuals)
qqline(gam_residuals)
shapiro.test(gam_residuals)

# Independence and Autocorrelation Check
plot(x$date_round, gam_residuals, type = "b", main = "Residuals Over Time - GAM")























shapiro.test(soil_ghg_flux$co2)


p_load(#lme4,
       nlme,
       rstatix,
       car,
       MASS,
       lmtest,
       multcomp,
       multcompView,
       robustlmm,
       emmeans)

x <- x %>% mutate(unique_time = as.numeric(as.POSIXct(datetime_est)))

y = x %>% filter(plot == "Control") %>% filter(depth == 5)


# If your periods are uniquely identifiable, you can use them as blocks:
# Convert 'period' to a factor
y$period <- as.factor(y$period)

# Rank the data within each group
y <- y %>%
  group_by(unique_time) %>%
  mutate(rank = rank(vwc)) %>%
  ungroup()

# Run the Friedman test
friedman_result <- friedman.test(rank ~ period | unique_time, data = y)

# Print the result
print(friedman_result)







nrow(y) == length(unique(y$unique_time))

glmm_mod <- try(glmmPQL(vwc ~ period, random = ~ 1 | unique_time, 
                        correlation = corAR1(form = ~ unique_time), 
                        family = gaussian, data = y))

lme_mod <- try(lme(vwc ~ period, random = ~ 1 | unique_time, 
                   correlation = corAR1(form = ~ unique_time), 
                   data = y))

residuals <- residuals(glmm_mod, type = "normalized")


# 1. Check for normality of residuals
qqnorm(residuals, main = "QQ Plot for Control Plot, Depth 5")
qqline(residuals, col = "red")
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

fitted_values <- fitted(glmm_mod)
plot(fitted_values, residuals, main = "Residuals vs Fitted for Control Plot, Depth 5")
abline(h = 0, col = "red")
bp_test <- bptest(lm(residuals ~ fitted_values))
print(bp_test)

acf_res <- acf(residuals, main = "ACF of Residuals for Control Plot, Depth 5")



## Let's try first ranking our data, then doing a repeated measures anova
# Rank the data within each group
z <- x %>% 
  filter(plot == "Freshwater") %>% 
  filter(depth == 5) 

ggplot(z, aes(datetime_est, vwc, color = period)) + 
  geom_line()

# Tukey's Ladder of Powers transformation
tukey_transform <- function(x){
  bestTransform <- bestNormalize(x, allow_orderNorm = FALSE, allow_lambert_s = FALSE)
  bestTransform
}

# Box-Cox transformation
boxcox_transform <- function(y){
  lambda <- boxcox(y ~ 1, plot = FALSE)$x
  opt_lambda <- boxcox(y ~ 1, plot = FALSE)$y[which.max(boxcox(y ~ 1, plot = FALSE)$y)]
  y_transformed <- (y^opt_lambda - 1) / opt_lambda
  list(transformed = y_transformed, lambda = opt_lambda)
}

# Ranking
ranking_transform <- function(y){
  rank(y)
}

p_load(bestNormalize, 
       nortest)

# Apply transformations to vwc
z <- z %>%
  mutate(
    tukey_vwc = tukey_transform(vwc)$x.t,
   # boxcox_vwc = boxcox_transform(vwc)$transformed,
    rank_vwc = ranking_transform(vwc)
  )

# Function to create QQ plots using ggplot2
create_qq_plot <- function(data, variable, title) {
  ggplot(data, aes(sample = .data[[variable]])) +
    stat_qq() +
    stat_qq_line() +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
}

# Create QQ plots
qq_plot_original <- create_qq_plot(z, "vwc", "Original vwc")
qq_plot_tukey <- create_qq_plot(z, "tukey_vwc", "Tukey Transformed vwc")
qq_plot_boxcox <- create_qq_plot(z, "boxcox_vwc", "Box-Cox Transformed vwc")
qq_plot_rank <- create_qq_plot(z, "rank_vwc", "Rank Transformed vwc")

# Check normality with Anderson-Darling test
ad_test_original <- ad.test(z$vwc)
ad_test_tukey <- ad.test(z$tukey_vwc)
#ad_test_boxcox <- ad.test(z$boxcox_vwc)
ad_test_rank <- ad.test(z$rank_vwc)

# Print Anderson-Darling test results
print(ad_test_original)
print(ad_test_tukey)
#print(ad_test_boxcox)
print(ad_test_rank)



# Apply bestNormalize to find the best normalization for vwc
best_norm <- bestNormalize(z$vwc, allow_orderNorm = F, allow_lambert_s = TRUE)

# Extract the transformed data
z$bestnorm_vwc <- best_norm$x.t

# Function to create QQ plots using ggplot2
create_qq_plot <- function(data, variable, title) {
  ggplot(data, aes(sample = .data[[variable]])) +
    stat_qq() +
    stat_qq_line() +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
}

# Create QQ plots
qq_plot_original <- create_qq_plot(z, "vwc", "Original vwc")
qq_plot_bestnorm <- create_qq_plot(z, "bestnorm_vwc", "BestNormalize Transformed vwc")


# Check normality with Anderson-Darling test
ad_test_original <- ad.test(z$vwc)
ad_test_bestnorm <- ad.test(z$bestnorm_vwc)

ggplot(z, aes(datetime_est, bestnorm_vwc)) + geom_point()








# Ensure that 'period' is a factor
z$period <- as.factor(z$period)

# Ensure 'unique_time' is treated as a blocking variable and is a factor
z$unique_time <- as.factor(z$unique_time)

# Ensure no missing combinations, fill with NA if necessary
z_complete <- z %>%
  complete(period, unique_time)

# Replace missing 'vwc' values with NA if needed (ensure a complete block design)
#z_complete$vwc[is.na(z_complete$vwc)] <- NA

# Prepare the data for the Friedman test
# Spread the data to wide format, with 'period' as columns
z_wide <- z %>%  #z_complete %>%
  select(unique_time, period, vwc) %>%
  spread(period, vwc) %>%
  as.data.frame()

# Set row names to the unique_time identifiers for the Friedman test
rownames(z_wide) <- z_wide$unique_time
z_wide$unique_time <- NULL

# Run the Friedman test
friedman_result <- friedman.test(as.matrix(z_wide))


kruskal.test(vwc ~ period, data = z)


p_load(FSA)
dunnTest(vwc ~ period, data = z, method = "bh")

z_wide <- z %>%
  select(unique_time, period, vwc) %>%
  spread(key = period, value = vwc)


lm_model <- lm(vwc ~ period, data = z)

p_load(sandwich)
robust_se <- coeftest(lm_model, vcov = vcovHC(lm_model, type = "HC3"))










%>% 
  group_by(unique_time) %>%
  mutate(rank = rank(vwc)) %>%
  ungroup()



ggplot(z, aes(datetime_est, vwc)) + geom_line()


















glmm_results <- list()
for (group in unique(x$plot)) {
  for (depth in unique(x$depth)) {
    subset_data <- x %>% filter(plot == group, depth == depth)
    # Fit a GLMM with correlation structure
    glmm_mod <- try(glmmPQL(vwc ~ period, random = ~ 1 | unique_time, 
                            correlation = corAR1(form = ~ unique_time), 
                            family = gaussian, data = subset_data))
    glmm_results[[paste(group, depth, sep = "_")]] <- summary(glmm_mod)
  }
}

x %>% 
  filter(plot == "Control") %>% 
  filter(depth == 5) %>% 
  select(datetime_est) %>% 
  unique()




# Fit a Linear Mixed-Effects Model for each subset of data by plot and depth
lme_results <- list()
for (group in unique(x$plot)) {
  for (depth in unique(x$depth)) {
    subset_data <- x %>% filter(plot == group, depth == depth)
    # Fit a linear mixed-effects model with correlation structure
    lme_mod <- lme(vwc ~ period, random = ~ 1 | datetime_est, correlation = corAR1(form = ~ datetime_est), data = subset_data)
    lme_results[[paste(group, depth, sep = "_")]] <- summary(lme_mod)
  }
}




x <- teros %>% #filter(plot == "Freshwater") %>% 
  label_flood_periods2() %>% 
  filter(!is.na(period))

kruskal_results <- x %>%
  group_by(plot, depth) %>%
  kruskal_test(ec ~ period)

x %>%
  group_by(plot, depth) %>%
  dunn_test(ec ~ period, p.adjust.method = "bonferroni")


lm(vwc~datetime_est, data = x)

subset_data <- x %>% filter(plot == group, depth == depth)
lm_mod <- lm(vwc~datetime_est, data = subset_data)

dw_results <- list()
for (group in unique(x$plot)) {
  for (depth in unique(x$depth)) {
    subset_data <- x %>% filter(plot == group, depth == depth)
    lm_mod <- lm(ec ~ datetime_est, data = subset_data)
     dw_test <- durbinWatsonTest(lm_mod)
     dw_results[[paste(group, depth, sep = "_")]] <- dw_test
  }
}

# Sin of pseudo-replication: exaggerating sample size because you're saying all
# data are independent when they aren't
# plot is random
# depth is random


model <- lme(ec ~ period * plot, random = ~ 1 | plot/depth, data = x)

emmeans_result <- emmeans(model, pairwise ~ period | plot)

cld_result <- cld(emmeans_result, Letters = letters)

cld_df <- as.data.frame(cld_result)

shapiro.test(resid(model))


# Log transformation
y <- boxcox(x$vwc)

x$vwc_bc <- transformTukey(x$vwc)

x$vwc_log <- sqrt(x$vwc)  # Adding 1 to avoid log(0)

# Fit the model with transformed response variable
model_log <- lme(vwc_bc ~ period * plot, random = ~ 1 | plot/depth, data = x)

# Check residuals again using the previous steps
qqnorm(resid(model_log), main = "Q-Q Plot of Residuals (Log Transformed)")
qqline(resid(model_log))
shapiro.test(resid(model_log))



# Option 2: GLMM
model_glmm <- glmer(vwc ~ period * plot + (1 | plot/depth), family = gaussian(link = "log"), data = x)
residuals_glmm <- residuals(model_glmm, type = "pearson")
qqnorm(residuals_glmm, main = "Q-Q Plot of Pearson Residuals (GLMM)")
qqline(residuals_glmm)
shapiro.test(residuals_glmm)

model_robust <- rlmer(vwc_bc ~ period * plot + (1 | plot/depth), data = x)
qqnorm(resid(model_robust), main = "Q-Q Plot of Residuals (Robust Model)")
qqline(resid(model_robust))
shapiro.test(resid(model_robust))

boot_model <- function(data, indices) {
  model <- lme(vwc ~ period * plot, random = ~ 1 | plot/depth, data = data[indices, ])
  return(fixef(model))
}

qqnorm(resid(model), main = "Q-Q Plot of Residuals")
qqline(resid(model))


model <- lmer(vwc ~ period + (1|depth) + (1|datetime_est), data = x)
#model <- lmer(vwc ~ period + (1|plot/depth) + (1|datetime_est), data = x)

shapiro.test(x$vwc)

hist(x$vwc)