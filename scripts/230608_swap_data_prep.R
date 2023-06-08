## This script imports redox sensor data
##
## 2023-06-06
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       rdrop2, 
       cowplot, 
       lubridate, 
       parsedate, # parse_date()
       janitor, # clean_names()
       purrr) # map()

## Set ggplot theme
theme_set(theme_bw())


# 2. Pull in data and format ---------------------------------------------------

## Control is PB81, logger is labeled CR1000
## FW is PB82, logger is labeled CR1000_2
## SW is PB83, logger is labeled CR1000_3

read_swap <- function(path){
  read_delim(path, skip = 1) %>% 
    slice(3:n()) %>% 
    mutate(across(c(contains("Batt"), contains("Redox")), as.numeric))
}

control <- read_swap("data/redox_sensors/CR1000_Table1.dat")
fw <- read_swap("data/redox_sensors/CR1000_2_Table1.dat")
sw <- read_swap("data/redox_sensors/CR1000_3_Table1.dat")




