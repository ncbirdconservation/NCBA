# author: "N.M. Tarr"
# date: "3/22/2021"
# description:
# The eBird sampling dataset can be downloaded from eBird, but includes many 
# records that are not relevant for the NCBA.  Therefore, checklist records of 
# interest must be extracted.  The auk package provides a way to do this and 
# saves the desirable records as a text file that can be read by other scripts.  
# This markdown performs the filtering.

# Load the necessary R packages.
library(auk)
library(tidyverse)

# Set a path for an output file
output_file <- "~/Documents/NCBA/Data/filtered_checklists.txt"

# Print path to sampling data set
sampling_file <- "/Volumes/eBird/ebd_sampling_relFeb-2021/ebd_sampling_relFeb-2021.txt"
print(sampling_file)

# Filtering criteria
state <- "US-NC"
country <- "US"
date <- c("2016-01-01", "2021-12-31")

# Filter the sampling data and save results to the output file. Note that only 
# complete checklists are returned.
starttime <- Sys.time()
effort_data <- auk_sampling(sampling_file) %>%
  # Define filters
  auk_date(date=date) %>%
  auk_country(country=country) %>%
  auk_state(state=state) %>%
  auk_complete() %>%
  # run filters
  auk_filter(file=output_file, 
             overwrite=TRUE,
             drop=c("Country Code", "State Code", "country", "State")
  ) %>%
  read_sampling()
endtime <- Sys.time()
print(endtime - starttime)

# Write to a csv file as well.
write_csv(effort_data, "~/Documents/NCBA/Data/filtered_checklists.csv")

# Now explore the filtered checklist records in another script (.R or .rmd file).