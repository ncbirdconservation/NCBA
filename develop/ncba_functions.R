# Functions for often-repeated actions associated NCBA data management and 
# analysis
#
# To access these functions, run 'source("ncba_functions.R")' in your R console
# or script or R markdown document.  If your working directory is not where 
# this file is stored, then replace "ncba_functions.R" with the path to the file
# For example, 'source("C:/Documents/NCBA/ncba_functions.R").  The functions can
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
  # ncba_db_user -- NCBA MongoDB username
  # ncba_db_pass -- NCBA MongoDB password  
  # database -- the database (within MongoDB) to query, likely "ebd_management"
  # collection -- collection name (e.g., "ebd")
  #
  # Example:
  # conn <- connect_ncba_db("~/Documents/NCBA/Scripts/ncba_config.R",
  #                       db = "ebd_mgmt",
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

