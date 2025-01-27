#Script matching tree dbh with sapflow data

#Load packages
library(tidyr)
library(readr)
library(dplyr)
library(stringr)

#Load dataframes
sapflow_inventory <- read.csv("data/from_kendal/sapflow_inventory.csv")
dbh_inventory <- read.csv("data/from_kendal/inventory.csv")

#Edit format to match dataframes
#Remove trailing white spaces 
sapflow_inventory %>%
  mutate(Plot = ifelse(Plot == "SW", "Saltwater", Plot)) %>%
  mutate(Plot = ifelse(Plot == "FW", "Freshwater", Plot)) %>%
  mutate(Plot = trimws(Plot, which = "right")) -> sapflow_inventory

#Merge dataframes and remove deep sapflow sensors
inventory <- merge(sapflow_inventory, dbh_inventory, 
                   by.x = c("Tree_ID", "Plot"), by.y = c("Tag", "Plot"), all.x = TRUE)
inventory %>%
  filter(!grepl("D", Sapflux_ID)) -> inventory

#Save as RDS
saveRDS(inventory, file = "data/from_kendal/dbh.rds")
      