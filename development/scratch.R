setwd("~/Code/NCBA/resources")
source("ncba_functions.R")

observer <- "obsr1000095"

# Get the data
checks0 <- get_checklists(observer = observer, project = NULL)

# Make a data frame summarizing checklist types
checks1 <- checks0 %>%
  group_by(checklist_type) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

print(checks1)