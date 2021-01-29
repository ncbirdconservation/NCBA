# read in an ebd file and wrangle it
library(tidyverse)
library(auk)

ebd_file <- "/users/nmtarr/Documents/NCBA/Data/ebd_filtered.txt"
df <- ebd_file %>% read_ebd()

print(unique(df["common_name"]))

