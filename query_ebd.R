# Retrieve records, save in text file
library(auk)

# path to the ebird data file -------------------------------------------
#input_file <- system.file("extdata/ebd-sample.txt", package = "auk")
input_file <- "/Volumes/TARR64GB/ebd-NCBA.txt"

# parameters ------------------------------------------------------------
output_file <- "/users/nmtarr/Documents/NCBA/Data/ebd_filtered.txt"
species <- c("Broad-winged Hawk", "Loggerhead Shrike", "Worm-eating Warbler")
#state <- "US-NC"
bbox <- "bbox = c(-100, 37, -80, 52)" # in decimal degrees
#date <- c("2018-07-01", "2019-12-31") # e.g. date = c("*-05-01", "*-06-30") for observations from May and June of any year.
duration <- c(0, 60)
protocol <- ""
project <- ""
#distance <- c(0,3)
breeding <- ""
complete <- ""

# query -----------------------------------------------------------------
ebird_data <- input_file %>%
  # 1. reference file
  auk_ebd() %>%
  # 2. define filters
  auk_species(species = species) %>%
  # 3. run filtering
  auk_filter(file = output_file, overwrite = TRUE) %>%
  # 4. read text file into r data frame
  read_ebd()

# display ---------------------------------------------------------------------
View(ebird_data)
