# Author: N. Tarr
# Date: 11/9/2023

# This script creates a data frame with tallies of diurnal and nocturnal
# survey hours by block and season.  It also includes columns for priority/non-priority
# block, diurnal progress (percent of the way to 20 hours), and a tally of the 
# number of nocturnal checklists by season (winter = nov 1 - feb 28 and summer =
# mar 1- august 31.  
#   Nocturnal status is based on the ncba_nocturnal field and nocturnal effort
# is based upon ncba_nocturnal_duration.

if(!require(here)) install.packages(
  "here", repos = "http://cran.us.r-project.org")
source(here("resources", "ncba_functions.R"))

knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

# Specify a path for the output csv file.
out_file <- "T:/NCBA/Analyses/block_hours.csv"

# Mark the start time
time1 <- proc.time()

# SUMMER
# GET CHECKLISTS --------------------------------------------------------------
checklists.S <- get_checklists(EBD_fields_only = FALSE) %>%
  to_EBD_format() %>%
  auk_unique(checklists_only = TRUE) %>%
  filter(month %in% c(3, 4, 5, 6, 7, 8)) %>%
  select(c("atlas_block", "priority_block", "duration_minutes", 
           "ncba_nocturnal_duration", "ncba_nocturnal",
           "observation_date")) %>%
  transform(ncba_nocturnal = parse_integer(ncba_nocturnal)) %>%
  replace_na(list(duration_minutes = 0, 
                  priority_block = "0",
                  ncba_nocturnal = 0)) %>%
  filter(atlas_block != "") %>% # Some checklists have "" for block id.
  transform(ncba_nocturnal_duration = as.numeric(ncba_nocturnal_duration))

# GET BLOCKS ------------------------------------------------------------------
blocks <- get_blocks(fields = c("ID_BLOCK_CODE", "ID_EBD_NAME"))
blocks <- blocks[, c("ID_EBD_NAME", "ID_BLOCK_CODE")]

# DIURNAL ---------------------------------------------------------------------
# Add columns with hours as unit.
checklists.S$diurnal_hrs <- checklists.S$duration_minutes/60

# Make a data frame summarizing DIURNAL hours
diurnal <- checklists.S %>%
  group_by(atlas_block, priority_block) %>%
  summarize(diurnal_hrs = format(sum(diurnal_hrs), digits = 4)) %>%
  transform(diurnal_hrs = as.numeric(diurnal_hrs))

# NOCTURNAL -------------------------------------------------------------------
# Add columns with nocturnal hours as unit.
checklists.S$nocturnal_hrs <- checklists.S$ncba_nocturnal_duration/60

# Make a data frame summarizing NOCTURNAL hours
nocturnal <- checklists.S %>%
  replace_na(list(nocturnal_hrs = 0)) %>%
  group_by(atlas_block, priority_block) %>%
  summarize(nocturnal_hrs = sum(nocturnal_hrs)) %>%
  transform(nocturnal_hrs = as.numeric(nocturnal_hrs))

# Make a data frame with count of nocturnal checklists
nocturnal.N <- checklists.S %>%
  filter(ncba_nocturnal == 1) %>%
  select(c("atlas_block", "ncba_nocturnal")) %>%
  group_by(atlas_block) %>%
  summarize(nocturnal_checklists_any = n()) %>%
  data.frame()

# Make a data frame with count of nocturnal SUMMER checklists
nocturnal.N.S <- checklists.S %>%
  filter(ncba_nocturnal == 1,
         yday(observation_date) >= 59 & yday(observation_date) <= 243) %>%
  select(c("atlas_block", "ncba_nocturnal")) %>%
  group_by(atlas_block) %>%
  summarize(nocturnal_checklists_summer = n()) %>%
  data.frame()

# Make a data frame with count of nocturnal WINTER checklists
nocturnal.N.W <- checklists.S %>%
  filter(ncba_nocturnal == 1,
         yday(observation_date) >= 304 | yday(observation_date) <= 58) %>%
  select(c("atlas_block", "ncba_nocturnal")) %>%
  group_by(atlas_block) %>%
  summarize(nocturnal_checklists_winter = n()) %>%
  data.frame()

# Combine winter and summer
nocturnal_periods <- nocturnal.N.W %>% left_join(nocturnal.N.S, 
                                                 by = "atlas_block")
nocturnal.N <- nocturnal.N %>% left_join(nocturnal_periods, by = "atlas_block")

# COMBINE ---------------------------------------------------------------------
nocturnal <- left_join(nocturnal, nocturnal.N, by=("atlas_block")) %>%
  replace_na(list(nocturnal_checklists_any = 0,
                  nocturnal_checklists_summer = 0,
                  nocturnal_checklists_winter = 0))

summer_hours <- left_join(diurnal, nocturnal, by=(c("atlas_block", 
                                                   "priority_block")))

# FORMATTING ------------------------------------------------------------------
# Add diurnal progress column
summer_hours$diurnal_progress <- 100*(summer_hours$diurnal_hrs/20)

# Transform columns
summer_hours <- summer_hours %>%
  transform(nocturnal_hrs = round(nocturnal_hrs, digits = 2)) %>%
  transform(diurnal_hrs = round(diurnal_hrs, digits = 2)) %>%
  transform(diurnal_progress = round(diurnal_progress, digits = 2))
  
# Reorder columns and arrange by priority_block
summer_hours <- summer_hours[, c("atlas_block", "priority_block", "diurnal_hrs", 
                               "diurnal_progress", "nocturnal_hrs", 
                               "nocturnal_checklists_any", 
                               "nocturnal_checklists_summer",
                               "nocturnal_checklists_winter")] %>%
  arrange(desc(priority_block))

# Add a season column
summer_hours$season <- "summer"
  
# Join to get block names
block_hours <- left_join(block_hours, blocks, 
                         by = join_by("atlas_block" == "ID_BLOCK_CODE")) %>%
  select("ID_EBD_NAME", everything())

# WINTER
# GET CHECKLISTS --------------------------------------------------------------
checklists.W <- get_checklists(EBD_fields_only = FALSE) %>%
  to_EBD_format() %>%
  auk_unique(checklists_only = TRUE) %>%
  filter(month %in% c(11, 12, 1, 2)) %>%
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
checklists.W$diurnal_hrs <- checklists.W$duration_minutes/60

# Make a data frame summarizing DIURNAL hours
diurnal <- checklists.W %>%
  group_by(atlas_block, priority_block) %>%
  summarize(diurnal_hrs = format(sum(diurnal_hrs), digits = 4)) %>%
  transform(diurnal_hrs = as.numeric(diurnal_hrs))

# NOCTURNAL -------------------------------------------------------------------
# Add columns with nocturnal hours as unit.
checklists.W$nocturnal_hrs <- checklists.W$ncba_nocturnal_duration/60

# Make a data frame summarizing NOCTURNAL hours
nocturnal <- checklists.W %>%
  replace_na(list(nocturnal_hrs = 0)) %>%
  group_by(atlas_block, priority_block) %>%
  summarize(nocturnal_hrs = sum(nocturnal_hrs)) %>%
  transform(nocturnal_hrs = as.numeric(nocturnal_hrs))

# Make a data frame with count of nocturnal checklists
nocturnal.N <- checklists.W %>%
  filter(ncba_nocturnal == 1) %>%
  select(c("atlas_block", "ncba_nocturnal")) %>%
  group_by(atlas_block) %>%
  summarize(nocturnal_checklists = n()) %>%
  data.frame()

# COMBINE ---------------------------------------------------------------------
nocturnal <- left_join(nocturnal, nocturnal.N, by=("atlas_block")) %>%
  replace_na(list(nocturnal_checklists = 0))

winter_hours <- left_join(diurnal, nocturnal, by=(c("atlas_block", 
                                                   "priority_block")))

# FORMATTING ------------------------------------------------------------------
# Add diurnal progress column
winter_hours$diurnal_progress <- 100*(winter_hours$diurnal_hrs/20)

# Transform columns
winter_hours <- winter_hours %>%
  transform(nocturnal_hrs = round(nocturnal_hrs, digits = 2)) %>%
  transform(diurnal_hrs = round(diurnal_hrs, digits = 2)) %>%
  transform(diurnal_progress = round(diurnal_progress, digits = 2))

# Reorder columns and arrange by priority_block
winter_hours <- winter_hours[, c("atlas_block", "priority_block", "diurnal_hrs", 
                               "diurnal_progress", "nocturnal_hrs", 
                               "nocturnal_checklists")] %>%
  arrange(desc(priority_block))

# Add a season column
winter_hours$season <- "winter"

# BIND AND SAVE ---------------------------------------------------------------
block_hours <- rbind(summer_hours, winter_hours)

# Save to csv file
write.csv(block_hours, out_file)

# Report the runtime
t <- proc.time() - time1
print(t["elapsed"])