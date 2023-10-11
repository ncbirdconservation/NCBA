setwd("~/Code/NCBA/resources")
source("ncba_functions.R")
library(sf)

# Get the blocks with simple features
blocks <- get_blocks(spatial = TRUE, fields = c("GEOM", "ID_BLOCK_CODE", "ID_EBD_NAME"))

# Identify a block of interest
block <- "35079H1SE"

# Get the geometry of the block of interest
block_geom <- blocks[blocks$ID_BLOCK_CODE == block]

# Get the ID_BLOCK_CODE of blocks that are adjacent to the block of interest
touching <- st_intersects(block_geom, blocks)

# Use block_spp_lists to get the species lists of each intersecting block, and
# combine them into a single list
# loop on touching 
for (t in touching) {
    # Get the species ID code
    code <- blocks[t, ]$ID_BLOCK_CODE
    
    # Get the species list
    spp_list <- block_spp_lists(code, start_day = 75, end_day = 300)

    # Combine the species list with the existing list
    if (exists("spp_list_all")) {
        spp_list_all <- c(spp_list_all, spp_list)
    } else {
        spp_list_all <- spp_list
    }

    # Remove duplicates
    spp_list_all <- unique(spp_list_all)
}
