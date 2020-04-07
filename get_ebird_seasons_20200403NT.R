"
Created by N. Tarr on April 3, 2020.
Creates a table (.csv) list of NCBA species with breeding season
boundaries from eBird status and trends.
"
library(tidyverse)
library(ebirdst)
library(lubridate)
library(stringr)

# Set paths and variables
projDir = 'T:/NCBA/Species_list/'

# Read in species list of NCBA
NCBA <- read.csv(paste(projDir,'NCBA_species.csv', sep=""))

# Make lower case to avoid capitilization problems
levels(NCBA$Common.Name) <- str_to_lower(levels(NCBA$Common.Name))

# Read in season boundaries tables from eBird status and trends
ebirdst <- ebirdst_runs

# Make lower case to avoid capitilization problems
#ebirdst$common_name <- str_to_lower(ebirdst$common_name)
ebirdst <- mutate(ebirdst, common_name = str_to_lower(ebirdst$common_name))

# Combine NCBA species dataframe with ebird runs
seasons <- NCBA %>%
left_join(ebirdst, c("Common.Name" = "common_name")) %>%
# Rename a column
mutate(NCBA_common_name = Common.Name) %>%
# Remove unwanted columns
select("NCBA_common_name", "breeding_start_dt", "breeding_end_dt",
"nonbreeding_start_dt", "nonbreeding_end_dt",
"postbreeding_migration_start_dt",
"postbreeding_migration_end_dt", "prebreeding_migration_start_dt",
"prebreeding_migration_end_dt", "year_round_start_dt",
"year_round_end_dt") %>%
# Sort
arrange(NCBA_common_name)

# Drop year from season date values
seasons

View(seasons)
