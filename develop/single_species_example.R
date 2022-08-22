# N. Tarr, 4/2/2022
# 
# Example script showing how to use the ncba_functions.
blocks.path <- "~/Datasets/ncba_blocks.shp"
config <- "~/Documents/NCBA/Scripts/ncba_config.R"
setwd("~/Code/NCBA/resources")
source("ncba_functions.R")
setwd("~/Documents/NCBA/Workspace/")

library(tidyverse)
library(tmap)
library(hms)
library(sf)

# Import the atlas functions
setwd("~/Code/NCBA/resources")
source("ncba_functions.R")

# Identify location of config file
config <- "~/Documents/NCBA/Scripts/ncba_config.R"

# Set a working environment
setwd("~/Documents/NCBA/species/")


# WHAT SPECIES? ----------------------------------------------------------------
species <- "American Bittern"


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

# !!!!!!!!!!!!!!!!! handle shared checklist duplication  # DEVELOP THIS !!!!!!!!!!!!!!!!!!!!!!!!!!
#sp_df2 <- auk_ebd(x=sp_df)


# PLOT BREEDING CODES ----------------------------------------------------------
lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
no_plot_codes <- NULL
out_pdf <- "~/Documents/NCBA/test.pdf"

breeding_boxplot(species, sp_df, pallet="Paired", out_pdf=NULL, 
                 no_plot_codes=no_plot_codes, lump=lump, drop=TRUE)


# PLOT COORDINATES OF RECORDS --------------------------------------------------
plot(plot_checklists_coords(sp_df))

# SUMMARIZE START TIMES --------------------------------------------------------
#plot(start_time_boxplot(sp_df))

# SUMMARIZE TRAVEL DISTANCE ----------------------------------------------------
plot(effort_distance_boxplot(sp_df))

# SUMMARIZE MINUTES EFFORT -----------------------------------------------------
plot(duration_minutes_boxplot(sp_df))

# LOCALITY TYPE BREAKDOWN ------------------------------------------------------
plot(locality_type_pie(sp_df))

# PLOT SIMPLE FEATURES ---------------------------------------------------------
sf <- records_as_sf(records_df=sp_df, kind="observations",
                    method="point-radius")
# Make a crude plot
plot(select(sf, c(sampling_event_identifier, geometry)))

# OBSERVATION PER BLOCK ----------------------------------------------
blocks.path <- "/Volumes/nmtarr1/Datasets/ncba_blocks.shp"
blocks <- st_read(blocks.path) %>% st_transform(6542) # Correct projection???????????

priority.n.B <- observations_per_block(records_df=sp_df, blocks_sf=blocks, 
                                       method="B")
priority.n.D <- observations_per_block(records_df=sp_df, blocks_sf=blocks, 
                                       method="D")


# Isolate priority blocks with fewer than 10 checklists
high.priority.B <- priority.n.B %>%
  filter(priority == 1, individuals <= 0)
high.priority.D <- priority.n.D %>%
  filter(priority == 1, individuals <= 0)

# Plot the map
tmap_mode("view")

tm_shape(priority.n.D) +
  tm_fill(col = "individuals", palette = "Reds", style="fixed", alpha = .7,
          breaks = c(0, 1, 2, 3, 5, 10, 1000), as.count = TRUE) +
  tm_shape(priority.n.B) +
  tm_fill(col = "individuals", palette = "Blues", style="fixed", alpha = .7,
          breaks = c(0, 1, 2, 3, 5, 10, 1000), as.count = TRUE) +
  tm_shape(high.priority.B) + 
  tm_fill(col = "individuals", style = "cat", palette=c("red","yellow")) +
  tm_shape(high.priority.D) + 
  tm_fill(col = "individuals", style = "cat", palette=c("blue","green")) +
  tm_borders()

