# Author: N. Tarr
# Date: 10/17/2023

# This script creates a data frame with tallies of diurnal and nocturnal
# survey hours by block.  It also includes columns for priority/non-priority
# block, diurnal progress (percent of the way to 20 hours), and a tally of the 
# number of nocturnal checklists.  
#   Nocturnal status is based on the ncba_nocturnal field and nocturnal effort
# is based upon ncba_nocturnal_duration.

# Specify a path for the output csv file.
out_file <- "~/Temp/block_hours.csv"

# Mark the start time
time1 <- proc.time()

# GET CHECKLISTS --------------------------------------------------------------
checklists <- get_checklists(project = "EBIRD_ATL_NC", 
                             EBD_fields_only = FALSE) %>%
  to_EBD_format() %>%
  auk_unique(checklists_only = TRUE) %>%
  select(c("atlas_block", "priority_block", "duration_minutes", 
           "ncba_nocturnal_duration", "ncba_nocturnal")) %>%
  transform(ncba_nocturnal = parse_integer(ncba_nocturnal)) %>%
  replace_na(list(duration_minutes = 0, 
                  priority_block = "0",
                  ncba_nocturnal = 0)) %>%
  filter(atlas_block != "") %>% # Some checklists have "" for block id.
  transform(ncba_nocturnal_duration = as.numeric(ncba_nocturnal_duration))

# DIURNAL ---------------------------------------------------------------------
# Add columns with hours as unit.
checklists$diurnal_hrs <- checklists$duration_minutes/60

# Make a data frame summarizing DIURNAL hours
diurnal <- checklists %>%
  group_by(atlas_block, priority_block) %>%
  summarize(diurnal_hrs = format(sum(diurnal_hrs), digits = 4)) %>%
  transform(diurnal_hrs = as.numeric(diurnal_hrs))

# NOCTURNAL -------------------------------------------------------------------
# Add columns with nocturnal hours as unit.
checklists$nocturnal_hrs <- checklists$ncba_nocturnal_duration/60

# Make a data frame summarizing NOCTURNAL hours
nocturnal <- checklists %>%
  replace_na(list(nocturnal_hrs = 0)) %>%
  group_by(atlas_block, priority_block) %>%
  summarize(nocturnal_hrs = sum(nocturnal_hrs)) %>%
  transform(nocturnal_hrs = as.numeric(nocturnal_hrs))

# Make a data frame with count of nocturnal checklists
nocturnal.N <- checklists %>%
  filter(ncba_nocturnal == 1) %>%
  select(c("atlas_block", "ncba_nocturnal")) %>%
  group_by(atlas_block) %>%
  summarize(nocturnal_checklists = n()) %>%
  data.frame()

# COMBINE ---------------------------------------------------------------------
nocturnal <- left_join(nocturnal, nocturnal.N, by=("atlas_block")) %>%
  replace_na(list(nocturnal_checklists = 0))

block_hours <- left_join(diurnal, nocturnal, by=(c("atlas_block", 
                                                   "priority_block")))

# FORMATTING ------------------------------------------------------------------
# Add diurnal progress column
block_hours$diurnal_progress <- 100*(block_hours$diurnal_hrs/20)

# Transform columns
block_hours <- block_hours %>%
  transform(nocturnal_hrs = round(nocturnal_hrs, digits = 2)) %>%
  transform(diurnal_hrs = round(diurnal_hrs, digits = 2)) %>%
  transform(diurnal_progress = round(diurnal_progress, digits = 2))
  
# Reorder columns and arrange by priority_block
block_hours <- block_hours[, c("atlas_block", "priority_block", "diurnal_hrs", 
                               "diurnal_progress", "nocturnal_hrs", 
                               "nocturnal_checklists")] %>%
  arrange(desc(priority_block))
  

# Save to csv file
write.csv(block_hours, out_file)

# Report the runtime
t <- proc.time() - time1
print(t["elapsed"])