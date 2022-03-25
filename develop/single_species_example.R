# N. Tarr, 10/29/2021
# 
# Example script showing how to use the ncba_functions "connect_ncba_db()", 
#   "to_ebd_format()", and "breeding_boxplot()"

library(stringr)
library(tidyr)
library(dplyr)
library(lubridate)

setwd("~/Code/NCBA/resources")
source("ncba_functions.R")

config <- "~/Documents/NCBA/Scripts/ncba_config.R"
setwd("~/Documents/NCBA/species/")


# WHAT SPECIES? ----------------------------------------------------------------
species <- "Kentucky Warbler"


# GET NCBA DATA ----------------------------------------------------------------
# connect to a specific collection (table)
connection <- connect_ncba_db(ncba_config = config, database = "ebd_mgmt", 
                              collection = "ebd")

# execute a query
query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${species}"}')

nc_data <- connection$find(query) %>%
  unnest(cols = (c(OBSERVATIONS))) %>% # Expand observations
  filter(COMMON_NAME == species)

# format columns
sp_df <- to_ebd_format(nc_data, drop=FALSE)


# PLOT BREEDING CODES ----------------------------------------------------------
lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
no_plot_codes <- NULL
out_pdf <- "~/Documents/NCBA/test.pdf"

breeding_boxplot(species, sp_df, pallet="Paired", out_pdf=NULL, 
                 no_plot_codes=no_plot_codes, lump=lump, drop=TRUE)


# PLOT COORDINATES OF RECORDS --------------------------------------------------
plot(plot_checklists_coords(sp_df))

# SUMMARIZE START TIMES --------------------------------------------------------
plot(start_time_boxplot(sp_df))

# SUMMARIZE TRAVEL DISTANCE ----------------------------------------------------
plot(effort_distance_boxplot(sp_df))

# SUMMARIZE MINUTES EFFORT --------------------------------------------------------
plot(duration_minutes_boxplot(sp_df))

# LOCALITY TYPE BREAKDOWN ------------------------------------------------------
plot(locality_type_pie(sp_df))

# PLOT SIMPLE FEATURES ---------------------------------------------------------
sf <- records_as_sf(records_df=sp_df, kind="observations",
                       method="point-radius")
# Make a crude plot
plot(select(sf, c(sampling_event_identifier, geometry)))

# OBSERVATION PER BLOCK --------------------------------------------------------
blocks_path <- "/Volumes/nmtarr1/Datasets/ncba_blocks.shp"
blocks <- st_read(blocks_path) %>% st_transform(6542)
checklists_per_block(records_df=sp_df, blocks_sf=blocks, method="B")
