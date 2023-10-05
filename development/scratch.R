setwd("~/Code/NCBA/resources")
source("ncba_functions.R")

observer <- "obsr1000095"

# Get the data
checks0 <- get_checklists(observer = observer, project = NULL) %>%
to_EBD_format()

# ------------------------------------------------------------
# VOLUNTEER BEHAVIOR
# ------------------------------------------------------------
# Make a data frame summarizing checklist types with the following
# format: rows for unique protocol types, columns for number, 
# proportion of total number.
checks1 <- checks0 %>% 
    group_by(protocol_type) %>%
    summarize(number = n(), percentage = 100 *(n / nrow(checks0))) %>%
    arrange(protocol_type)

# Add a column to checks1 that reports how many hours of effort 
# were spent on each protocol type.  The duration_minutes column
# holds the number of minutes spent on each checklist, but report
# the duration in hours.
checks2 <- checks0 %>%
    mutate(hours = duration_minutes / 60) %>%
    group_by(protocol_type) %>%
    summarize(duration_hours = sum(hours)) %>%
    arrange(protocol_type)

# Add the duration_hours column to checks1
checks3 <- checks1 %>%
    left_join(checks2, by = "protocol_type")
print(checks3)

# ------------------------------------------------------------
# MY ATLAS
# ------------------------------------------------------------
# Duplicate the checks3 but for the column "all_species_reported"
# instead of "protocol_type"
checks4 <- checks0 %>%
    group_by(all_species_reported) %>%
    summarize(number = n(), percentage = 100 *(number / nrow(checks0))) %>%
    arrange(all_species_reported)

# Add a column to checks4 that reports how many hours of effort
# were spent on each checklist type.
checks5 <- checks0 %>%
    mutate(hours = duration_minutes / 60) %>%
    group_by(all_species_reported) %>%
    summarize(duration_hours = sum(hours)) %>%
    arrange(all_species_reported)

# Add the duration_hours column to checks4
checks6 <- checks4 %>%
    left_join(checks5, by = "all_species_reported")
print(checks6)

# ------------------------------------------------------------
# DESCRIBE TIME AND DISTANCE
# ------------------------------------------------------------
# Start by making a new column with duration in hours.
checks7 <- checks0 %>%
    mutate(duration_hours = duration_minutes / 60)

# Make a data frame with rows "duration (hours)" and "distance (km)"
# and columns "mean", "median", "sd", "min", "max", "count"
time <- data.frame(
    row.names = c("duration (hours)"),
    min = checks7$duration_hours %>% min(),
    median = checks7$duration_hours %>% median(),
    max = checks7$duration_hours %>% max(),
    mean = checks7$duration_hours %>% mean(),
    sd = checks7$duration_hours %>% sd(),
    count = checks7$duration_hours %>% length()
)
print(time_distance)

# Make a data frame like the previous one but based upon effort_distance_km
distance <- data.frame(
    row.names = c("distance (km)"),
    min = checks7$effort_distance_km %>% min(),
    median = checks7$effort_distance_km %>% median(),
    max = checks7$effort_distance_km %>% max(),
    mean = checks7$effort_distance_km %>% mean(),
    sd = checks7$effort_distance_km %>% sd(),
    count = checks7$effort_distance_km %>% length()
)

# Combine with distance
time_distance <- rbind(time, distance)
print(time_distance)









