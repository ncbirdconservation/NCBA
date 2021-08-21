# title: "Non-atlas effort by atlas block (Non-atlas)"
# author: "N.M. Tarr"
# date: August 20 2021
# description: Summarizes eBird effort by block, but excludes NCBA-associated 
# data.  Input is the output from a run of "filter_eBird_sampling.R".

library(auk)
library(tidyverse)
library(sf)
library(lubridate)
starttime <- Sys.time()

# Read in data and prep
blocks_path <- "~/Data/ncba_blocks.shp"
blocks_sf <- st_read(blocks_path) %>% st_transform(6542)
filtered_checklists <- "~/Documents/NCBA/Data/filtered_checklists.txt"
checklists <- read_sampling(filtered_checklists) %>% filter(project_code != "EBIRD_ATL_NC")
results_path <- "~/Documents/NCBA/non-atlas_effort_by_block.csv"

# Further filtering of sampling data could go here -----------------------------

## Notes on methods ------------------------------------------------------------
# Although coordinates are provided by eBird for checklists, they do not provide 
# precise locations of eBirder effort for two reasons.  First, there are limits 
# to the spatial precision of the points due to gps precision and/or observers 
# ability to identify exactly where they birded on a map or in the app.  Second, 
# many birders travel while birding but their paths are not available, only the 
# distances they traveled.

# Locational uncertainty is important and problematic because if it is large in 
# relation to the level of analysis, it creates uncertainty about which spatial
# subregions, such as counties or atlas blocks, a checklist should be attributed 
# to.  I explored two appraoches to handling locational uncertainty.

# Method A -- Assign each checklist to the subregion that the checklist 
#  coordinate is located within.  This approach could generate deceptive 
#  results if checklists represent birding effort from multiple blocks but are 
#  assigned to a single block or if the coordinate is located in a block 
#  adjacent to where the birding actually occurred.  This approach should 
#  generally be expected to underestimate how many checklists covered some 
#  portion of a given block.

# Method B -- Use polygons instead of the coordinates (points, Method A) in 
# order to include the locational uncertainty.  Under this approach, coordinates
# are buffered with the distance traveled by the observer during the checklist 
# period, plus 100 m to account for the fact that observers may have recorded 
# birds at a distance from where they were located.  Each checklist is then 
# assigned to all of the blocks that the polygon intersects in order to 
# acknowledge that the checklist could represent effort from multiple blocks. 
# Results from this approach can logically be expected to exaggerate the true 
# footprint of birding effort and suggest blocks were sampled that actually were 
# not, thus overestimating how many checklists covered some portion of a given 
# block.  Furthermore, checklists with large effort distances produce enormous 
# footprints than make results unhelpful.  Thus, I excluded checklists with 
# effort distances greater than 5 km for this method.

# Uncertainty -- Under the assumption that checklist coordinates represent a 
# a location that was surveyed during the checklist, method A tallies can be 
# considered minimums for the blocks.  Furthermore, method B tallies can be 
# considered maximum bounds, and the difference between the two is a measure of 
# uncertainty in the true quantity within the block.  Note that some differences
# are negative because checklists with really large distances traveled were removed
# for method B.

# Method A  --------------------------------------------------------------------
count_A <- checklists %>%
  # Make checklists data frame spatial with right projection
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(6542) %>%
  # Find which blocks each coordinate is within
  st_join(blocks_sf, join = st_within, left=TRUE) %>%
  # Summarize by number checklists within each block
  group_by(name) %>%
  summarize(checklists=n(), minutes=sum(duration_minutes)) %>%
  select(name, checklists, minutes) %>%
  # Join back with blocks spatial frame to fill in zeros (as a data frame)
  data.frame() %>%
  select(-c(geometry)) %>%
  right_join(blocks_sf, by=("name" = "name")) %>%
  replace_na(list(checklists=0, minutes=0))

# Method B ---------------------------------------------------------------------
# Create new sf of buffered checklist coordinates.  Some lists have no distance
#   so replace those with 0.  Stationary or short lists should be buffered 100
#   m to account for area surveyed.  This is imperfect, but more defensible than
#   nothing. Lists traveling > 5 km are just problematic and not informative so 
#   removed here.
buffered_coordinates <- checklists %>%
  # Make spatial frame
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(6542) %>%
  select(checklist_id, atlas_block, protocol_type, effort_distance_km, 
         duration_minutes, geometry) %>%
  # Buffer coordinates
  replace_na(list(effort_distance_km=0)) %>%
  filter(effort_distance_km <= 5) %>%
  mutate(buffer_length = (effort_distance_km + 0.1)*1000) %>%
  mutate(footprint = st_buffer(geometry, buffer_length)) %>%
  st_set_geometry("footprint") %>%
  select(-c(geometry)) 

# Intersect lists with blocks, tally checklists per block, drop footprint,
#   join with blocks sf to get block geometry
count_B <- buffered_coordinates %>%
  # Intersect footprints with blocks, NOTE this keeps "withins" and fragments
  st_intersection(blocks_sf) %>%
  # Find count by block
  group_by(name) %>%
  summarize(checklists=n(), minutes=sum(duration_minutes)) %>%
  st_drop_geometry() %>%
  # Add zero blocks via a join
  right_join(blocks_sf, by="name") %>%
  select(name, checklists, minutes, geometry) %>%
  replace_na(list(checklists=0, minutes=0))

# Synthesis --------------------------------------------------------------------
# Join methods A and B results
count_A2 <- count_A %>%
  select(name, checklists, minutes) %>%
  mutate(method = "A")
count_B2 <- count_B %>%
  select(name, checklists, minutes) %>%
  mutate(method = "B")
count_AB <- rbind(count_A2, count_B2)

# Spread A and B results into separate columns
# Replace infinite uncertainty values with "B" value (inf happens when "A" = 0)
list_counts <- count_AB %>%
  select(name, checklists, method) %>%
  spread(key = method, value = checklists) %>%
  rename(c(checklists_A=A, checklists_B=B))
time_counts <- count_AB %>%
  select(name, minutes, method) %>%
  spread(key = method, value = minutes) %>%
  rename(c(minutes_A=A, minutes_B=B))
counts <- inner_join(list_counts, time_counts, by=("name" = "name")) %>%
  mutate(list_uncertainty = (checklists_B - checklists_A)) %>%
  mutate(minutes_uncertainty = (minutes_B - minutes_A)) %>%
  replace_na(list(list_uncertainty=0, minutes_uncertainty=0)) %>%
  select(name, checklists_A, checklists_B, list_uncertainty, minutes_A, 
         minutes_B, minutes_uncertainty)

# Save results -----------------------------------------------------------------
write_csv(counts, results_path)