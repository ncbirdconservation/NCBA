# Functions for often-repeated actions associated with NCBA data management and 
# analysis
#
# To access these functions, run 'source("ncba_functions.R")' in your R console,
# script, or R markdown document.  If your working directory is not where 
# this file is stored, then replace "ncba_functions.R" with the path to the file
# For example, 'source("C:/Code/NCBA/ncba_functions.R").  The functions can
# then be called by their names.

# ------------------------------------------------------------------------------
connect_ncba_db <- function(ncba_config="ncba_config.R", database, collection){
  # Connect to the NCBA MongoDB database
  #
  # Description:
  # Returns a mongolite connection to the database for use in queries.  Username
  # and password are retrieved from a config file containing variables that can
  # retrieved from the working directory (default) or a user-specified location.
  # 
  # Parameters:
  # ncba_config -- Config file with NCBA MongoDB username and password
  # database -- the database (within MongoDB) to query, likely "ebd_management"
  # collection -- collection name (e.g., "ebd")
  #
  # Example:
  # conn <- connect_ncba_db("~/Documents/NCBA/Scripts/ncba_config.R",
  #                       database = "ebd_mgmt",
  #                       collection = "ebd")
  # mongodata <- conn$find({})
  
  # Retrieve credentials
  source(ncba_config)
  
  # Database info
  host <- "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
  uri <- sprintf("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=atlas-3olgg1-shard-0&readPreference=primary&ssl=true", 
                  ncba_db_user, ncba_db_pass, host, database)
  
  # Connect to a specific collection (table)
  m <- mongo(collection=collection, db=database, url=uri)
}


to_ebd_format <- function(dataframe, drop){
  # Reformat columns to match that of the EBD.
  #
  # Description:
  # Change the column names of a data frame retrieved from the NCBA mongo 
  #   database to the format of tables retrieved from the eBird EBD.  This is to
  #   facilitate the use and development of code for either the EBD or NCBA db. 
  # 
  # Parameters:
  # dataframe -- a data frame retrieved from the NCBA mongo database.
  # drop -- TRUE or FALSE whether to drop columns not present in the EBD.
  
  # Capitalize columns
  names(dataframe) <- str_to_lower(names(dataframe))
  
  # Drop columns
  ebd_columns <- c("last_edited_date", "county", "county_code",
                   "iba_code", "bcr_code", "usfws_code", "atlas_block",
                   "locality", "locality_id", "locality_type", "latitude",
                   "longitude", "observation_date", "time_observations_started",
                   "observer_id", "sampling_event_identifier", "protocol_type",
                   "protocol_code", "project_code", "duration_minutes",
                   "effort_distance_km", "effort_area_ha", "number_observers",
                   "all_species_reported", "group_identifier", "trip_comments")
  if (drop == TRUE) {
    x <- dataframe %>% select(ebd_columns)
  }
  else {
    x <- dataframe
  }
}