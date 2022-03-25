# Edit these paths.
blocks_path <- "//ncba_blocks.shp"
config <- "~/s/ncba_config.R"
setwd("~/Code/NCBA/resources")
source("ncba_functions.R")
setwd("~//Workspace/")

library(tidyverse)
library(tmap)

# Read in NCBA block spatial data frame
blocks_sf <- st_read(blocks_path) %>% st_transform(6542)

# Get checklists data from the NCBA database.
checklists_df <- get_all_checklists(config, drop_ncba_col=TRUE)
     
# Convert checklists to simple features
records_sf <- records_as_sf(checklists_df, kind="checklists", method="points")

# Summarize checklists per block
per.block <- checklists_per_block(checklists_df, blocks_sf, method="B")
#per.block <- select(per.block, c(checklists, geometry))

# Isolate priority blocks with fewer than 10 checklists
high.priority <- per.block %>%
  filter(priority == 1, checklists <= 10)

# Make an interactive map
tmap_mode("view")
tm_shape(per.block) +
  tm_fill(col = "checklists", palette = "Blues", style="fixed", alpha = .7,
          breaks = c(0, 1, 50, 100, 1000, 2000, 10000), as.count = TRUE) +
tm_shape(high.priority) + 
  tm_fill(col = "checklists", style = "cat", palette=c("red","yellow")) +
tm_borders()

