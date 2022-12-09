setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)
library(dplyr)
if (!require(suncalc)) install.packages(
  "suncalc", repos = "http://cran.us.r-project.org")

dt <- "2021-01-02"
tm <- "10:11:00"

dtfull <-paste(
    dt,
    tm,
    sep=" ")

date=as.Date(dtfull, tz="EST")

cData <- read.table(
    paste0("C:/Users/skanderson/OneDrive - State of North Carolina/",
    "@arcgis_projects/NCBA/20221209_noc_testing.csv"), header=TRUE,sep=",")

cData$Date <- paste(
    cData$OBSERVATION_DATE,
    cData$TIME_OBSERVATIONS_STARTED,
    sep = " "
)


# Savings Time Start/End Dates
# 2021	March 14	November 7
# 2022	March 13	November 6
# 2023	March 12	November 5
# 2024	March 10	November 3
# 2025	March 9	November 2
savings_time_cutpoints <- c(
    "2021-01-01", #start of project
    "2021-03-14", #start 2021 EDT
    "2021-11-07", #end 2021 EDT, start EST
    "2022-03-13", #start EDT
    "2022-11-06", #end EDT, start EST
    "2023-03-12", #start EDT
    "2023-11-05", #end EDT, start EST
    "2024-03-10", #start EDT
    "2024-11-03", #end EDT, start EST
    "2025-03-09", #start EDT
    "2022-11-02" #end EDT, start EST
)
st_labels <- c(
    "EST",
    "EDT",
    "EST",
    "EDT",
    "EST",
    "EDT",
    "EST",
    "EDT",
    "EST",
    "EDT"
)

test <- cut(as.Date(cData$Date),as.Date(savings_time_cutpoints),labels=st_labels)


# APEX-SE data
# Species: 152
# Confirmed (C4): 35 
# Probable (C3): 20
# Possible (C2): 31
# Diurnal: 668 hrs
# Nocturnal: 2 hrs
# Total: 671 hrs