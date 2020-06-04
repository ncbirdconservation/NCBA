# Retrieve NC loggerhead shrike records, save in text file
library(auk)
#
auk_set_ebd_path(path = "/Volumes/TARR64GB/ebd_US-NC_relApr-2020", 
                 overwrite = FALSE)

# path to the ebird data file, here a sample included in the package
# in practice, provide path to ebd, e.g. input_file <- "data/ebd_relFeb-2018.txt"

#input_file <- system.file("extdata/ebd-sample.txt", package = "auk")
input_file <- "/Volumes/TARR64GB/ebd_US-NC_relApr-2020/ebd_US-NC_relApr-2020.txt"

# output text file
output_file <- "/users/nmtarr/Documents/NCBA/Data/ebd_filtered_tewa.txt"
ebird_data <- input_file %>%
  # 1. reference file
  auk_ebd() %>%
  # 2. define filters
  auk_species(species = "Tennessee Warbler") %>%
  auk_country() %>%
  auk_state() %>%
  auk_bbox() %>%
  auk_date() %>%
  auk_distance() %>%
  # 3. run filtering
  auk_filter(file = output_file, overwrite = TRUE) %>%
  # 4. read text file into r data frame
  read_ebd()