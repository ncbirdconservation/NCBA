# N. Tarr, 10/29/2021
# 
# Example script showing how to use the ncba_functions "connect_ncba_db()", 
#   "to_ebd_format()", and "breeding_boxplot()"

library(stringr)
library(tidyr)
library(dplyr)

setwd("~/Code/NCBA/resources")
source("ncba_functions.R")

config <- "~/Documents/NCBA/Scripts/ncba_config.R"
setwd("~/Documents/NCBA/species/")

# WHAT SPECIES? ----------------------------------------------------------------
species <- "Clapper Rail"

# GET NCBA DATA ----------------------------------------------------------------
# connect to a specific collection (table)
connection <- connect_ncba_db(ncba_config = config, database = "ebd_mgmt", 
                              collection = "ebd")

# execute a query
query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${species}"}')

nc_data <- connection$find(query) %>%
  unnest(cols = (c(OBSERVATIONS))) %>% # Expand observations
  filter(COMMON_NAME == species)

# Format columns
ebird <- to_ebd_format(nc_data, drop=FALSE)

# PLOT BREEDING CODES ----------------------------------------------------------
lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
no_plot_codes <- NULL
out_pdf <- "~/Documents/NCBA/test.pdf"

breeding_boxplot(species, ebird, pallet="Paired", out_pdf=NULL, 
                 no_plot_codes=no_plot_codes, lump=lump, drop=TRUE)