## This script is for various statistical analyses of the TEROS dataset
##
## 2023-06-16
## Peter Regier
## 
# ########## #
# ########## #


# 1. Setup ---------------------------------------------------------------------

## First, load setup script to set up environment
source("scripts/0_setup.R")

library(devtools)
install_github("ethanbass/ggtukey")
require(ggtukey)

# 2. Functions -----------------------------------------------------------------


## Create a function, which relies heavily on ggtukey::geom_tukey() to do the heavy
## lifting described in https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots
make_tukey_boxplots <- function(data, var, y_label){
  
  period_labels = c("Pre-Flood", "Flood 1", "Flood 2", "Post-Flood")
  
  ggplot(data, aes(x = period, y = {{var}}, fill = period)) + 
    geom_boxplot(show.legend = F) + 
    facet_wrap(~plot) + 
    geom_tukey(where = "whisker") + 
    scale_x_discrete(labels = period_labels) + 
    scale_fill_viridis_d() + 
    labs(x = "", y = y_label) +
    theme(axis.text.x = element_text(angle=45, vjust=.5))
}


# 3. Prep data -----------------------------------------------------------------

## Read in data and set windows. We're using a consistent 24-hour time-period
## to represent each section, since we observe that treatment impacts persist
## after each event, and the max window is 24 hours based on flood recurrance
## interval.

teros <- read_csv("data/230610_teros_medians.csv") %>% 
  label_flood_periods()

## Visually check that deliniations are right
ggplot(teros, aes(datetime, vwc, color = period)) + 
  geom_line() + 
  facet_wrap(~plot)

firesting <- read_csv("data/230610_firesting.csv") %>% 
  label_flood_periods()

redox <- read_csv("data/230618_swap_redox_raw.csv") %>% 
  label_flood_periods()


# 3. Make comparison boxplots --------------------------------------------------

plot_grid(make_tukey_boxplots(teros, tsoil, "Temp (C)"),
          make_tukey_boxplots(teros, vwc, "VWC (m3/m3)"),
          make_tukey_boxplots(teros, ec, "EC (mS/cm)"), 
          ncol = 1)
ggsave("figures/230616_tukey_boxplots_teros.png", width = 7, height = 12)
  
make_tukey_boxplots(firesting, do_percent_sat, "DO (% Sat)")
          ggsave("figures/230616_tukey_boxplots_firesting.png", width = 7, height = 4)

make_tukey_boxplots(redox, redox_mv, "Redox (mV)")
ggsave("figures/230616_tukey_boxplots_redox.png", width = 7, height = 4)






letters.df <- data.frame(multcompLetters(TukeyHSD(aov(vwc ~ period * plot, data = teros_trim))$`period:plot`[,4])$Letters)
colnames(letters.df)[1] <- "letter" #Reassign column name
letters.df$Category <- rownames(letters.df) #Create column based on rownames

placement <- teros_trim %>% #We want to create a dataframe to assign the letter position.
  group_by(period, plot) %>%
  summarise(y_placement = quantile(vwc)[4])

letters_df <- left_join(letters.df, placement)

# anova
anova <- aov(vwc ~ period * plot, data = teros_trim)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
dt <- teros_trim %>% 
  group_by(period, plot) %>%
  summarise(w=mean(vwc), sd = sd(vwc) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup()

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = cld$`period:plot`$Letters)
dt$cld <- cld2$letters

print(dt)


ggplot(teros_trim, aes(x = period, y = vwc, fill = period)) + 
  #geom_bar(stat = "identity", aes(fill = period), show.legend = FALSE) +
  #geom_errorbar(aes(ymin = w - sd, ymax = w + sd), width = 0.2) +
  geom_boxplot() + 
  labs(x = "period", y = "VWC") +
  geom_text(aes(label = cld, y = 0.5), vjust = -0.5) +
  facet_wrap(~plot)









