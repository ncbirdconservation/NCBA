library("ebirdst")
library(raster)
sp_path <- ebirdst_download(species = "example_data")
abd <- load_raster("abundance", path = sp_path)
