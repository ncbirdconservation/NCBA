library(tidyverse)
################################# Load the various species lists
################################################################
projDir = 'T:/NCBA/Species_list/'

# GAP
gap <- read.csv(paste(projDir,'Gap_birds_NC.csv', sep=""))
View(gap)

# eBird
ebirdst <- read.csv(paste(projDir,'ebirdst_runs.csv', sep=""))
View(ebirdst)

# NCBA
NCBA <- read.csv(paste(projDir,'NCBA_species.csv', sep=""))
View(NCBA)

# Get a dataframe of all NCBA species and a column denoting if there is 
# an ebird-st run.
ncebirdst <- NCBA %>% 
  left_join(ebirdst, c("AOS59.Scientific.Name" = "scientific_name")) %>%
  mutate(ebirdst = case_when(is.na(run_name) == FALSE ~ 1)) %>%
  select(Common.Name, ebirdst)
write.csv(ncebirdst, file='T:/NCBA/Species_List/ncba-ebirdsted.csv')

# Which GAP species don't match the NCBA list?
GAPnonBA <- gap %>% 
  full_join(NCBA, c("GAP_common_name" = "Common.Name")) %>%
  select("GAP_common_name", "GAP_scientific_name", 
         "AOS59.Scientific.Name", "AOS57.Scientific.Name")
View(GAPnonBA)