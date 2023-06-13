## This script sets up the environment for all follow-on scripts
##
## 2023-06-10
## Peter Regier
## 
# ########### #
# ########### #

## Load packages
require(pacman)
p_load(tidyverse, 
       rdrop2, 
       cowplot, 
       lubridate, 
       plotly,
       parsedate, # parse_date()
       janitor, # clean_names()
       purrr) # map()

## Set ggplot theme
theme_set(theme_bw())

## Set common tz to use across all datetimes to EST (not sure why, but it's + 
## instead of - to get EST)
common_tz = "Etc/GMT+5"

## Set timestamps to mark the start of each flood event. Note that events started
## at 6am EDT, but all timezones are in EST
dump_start1 = as.POSIXct("2023-06-06 05:00", tz = common_tz)
dump_start2 = as.POSIXct("2023-06-07 05:00", tz = common_tz)

## Helper functions that add na.rm to stats functions
mean_ <- function(var){mean({{var}}, na.rm = T)}
median_ <- function(var){median({{var}}, na.rm = T)}

