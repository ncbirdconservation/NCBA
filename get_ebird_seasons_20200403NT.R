"
Created by N. Tarr on April 3, 2020.

Creates a table (.csv) list of NCBA species with breeding season 
boundaries from eBird status and trends.
"
library(tidyverse)
library(ebirdst)
library(lubridate)

# Set paths and variables
projDir = 'T:/NCBA/Species_list/'

# Read in species list of NCBA
NCBA <- read.csv(paste(projDir,'NCBA_species.csv', sep=""))


# Read in season boundaries tables from eBird status and trends
eBird <- ebirdst_runs
View(eBird)


#