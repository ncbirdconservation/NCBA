"
Created by N. Tarr April 1, 2020

Retrieves the season start and end dates from eBird status and 
trends.  Joins with NCBA species list to make a table of start and end
dates of each season for each NC major ecoregion. 
"
library(tidyverse)
library(ebirdst)

# Paths and variables
projDir = 'T:/NCBA/Species_list/'

# Read in the NCBA species list
NCBA <- read.csv(paste(projDir,'NCBA_species.csv', sep=""))

# Get species runs table from ebird
ebirdst <- ebirdst_runs

# Combine tables, drop non-NCBA species
# Rename the "Common.Name" column to "NCBA_common_name"
# Drop unwanted columns
seasons <- NCBA %>% 
  left_join(ebirdst, c("AOS59.Scientific.Name" = "scientific_name")) %>% 
  mutate(NCBA_common_name = Common.Name) %>% 
  select("NCBA_common_name", "breeding_start_dt", "breeding_end_dt", 
         "nonbreeding_start_dt", "nonbreeding_end_dt", 
         "postbreeding_migration_start_dt", 
         "postbreeding_migration_end_dt", "prebreeding_migration_start_dt",
         "prebreeding_migration_end_dt", "year_round_start_dt",
         "year_round_end_dt")

# Reformat data so that year is removed from values.
# from web d2$MthDD <- format(as.Date(d2$PickUpDate), "%m-%d")


# Copy rows for each ecoregion.



write.csv(seasons, file='T:/NCBA/Preliminary/ebird_season_boundaries.csv')



