# This is a quick attempt to pull out species names that were detected along
# with a target species.  It is rough and should be verified and further developed.

setwd("~/Code/NCBA/resources")
source("ncba_functions.R")
setwd(work_dir)
library(auk)

# What species
species <- "Black-throated Green Warbler"

# Connect to the NCBA database, observation data
connection <- connect_ncba_db("ebd_mgmt", "ebd")

# Build the query
query <- str_interp('{"OBSERVATIONS.COMMON_NAME" : "${species}"}')

# Retrieve the checklists
records <- connection$find(query = query) %>%
  unnest(cols = (c(OBSERVATIONS))) %>%       # Expands observations
  to_EBD_format() %>%
  auk_unique() %>%
  filter(protocol_type == "Stationary",
         longitude >= -80.00,                # east of Rockingham NC
         month %in% c(4,5))                  # from April or May

# summarize count of checklists with it
summ <- records %>%
  select(c("common_name", "checklist_id")) %>%
  group_by(common_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

print(summ)


