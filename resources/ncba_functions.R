# Functions for often-repeated actions associated with NCBA data management and 
# analysis
#          
# To access these functions, run 'source("ncba_functions.R")' in your R console,
# script, or R markdown document.  If your working directory is not where 
# this file is stored, then replace "ncba_functions.R" with the path to the file
# For example, 'source("C:/Code/NCBA/ncba_functions.R").  The functions can
# then be called by their names.
library(tidyverse)

# Load the config file
source("ncba_config.r")

# Set the working directory to the work_dir variable from the config file.
#   This may not always work (rmarkdown).....
setwd(work_dir)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
connect_ncba_db <- function(database, collection){
  # Connect to the NCBA MongoDB database
  #
  # Description:
  # Returns a mongolite connection to the database for use in queries.  Username
  # and password are retrieved from a config file containing variables that can
  # retrieved from the working directory (default) or a user-specified location.
  # 
  # Parameters:
  # database -- the database (within MongoDB) to query, likely "ebd_management"
  # collection -- collection name (e.g., "ebd")
  #
  # Example:
  # conn <- connect_ncba_db(database = "ebd_mgmt", collection = "ebd")
  # mongodata <- conn$find({})
  library(mongolite)
  
  # Database info
  host <- "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
  uri <- sprintf("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=atlas-3olgg1-shard-0&readPreference=primary&ssl=true", 
                  ncba_db_user, ncba_db_pass, host, database)
  
  # Connect to a specific collection (table)
  m <- mongo(collection=collection, db=database, url=uri)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_blocks <- function(spatial = FALSE, fields = NULL,
                           crs = 4326) {
  # Returns a data frame of blocks with or without geometries
  # 
  # Description:
  #   Retrieves the blocks data set in a data frame or spatial data frame 
  #   (simple feature).  A subset of all available fields can be specified
  #   to speed up the query.
  # 
  # Parameters:
  # spatial -- TRUE or FALSE whether to return a spatially enabled data frame
  #     TRUE yields a data frame whereas FALSE returns a simple features data 
  #     frame.
  # fields -- either NULL or a list of column names to include while excluding
  #     all others.  On August 28, 2023, column names available were:
  #   "AREA_SQMI"             "COUNTY"                "GEOM"                 
  #   "ID_BLOCK"              "ID_BLOCK_CODE"         "ID_EBD_NAME"          
  #   "ID_NCBA_BLOCK"         "ID_OLD_ID"             "ID_S123_NOSPACES"     
  #   "ID_S123_SPACES"        "ID_WEB_BLOCKMAP"       "NW_X"                 
  #   "NW_Y"                  "POSITION"              "PRIORITY"             
  #   "QUADID"                "QUAD_BLOCK"            "QUAD_NAME"            
  #   "REGION"                "SE_X"                  "SE_Y"                 
  #   "SUBNAT2"               "TYPE"                  "ID_S123_NOSPACES_TEMP"
  #   "ID_S123_SPACES_TEMP"   "GAP_SPP" (nested)      "EBD_SPP" (nested)    
  #   "ECOREGION" 
  #
  #   crs -- code of the CRS that you want spatial output in.  Defaults to 4326.
  library(sf)
  
  # Connect to the blocks collection (table)
  connection_blocks <- connect_ncba_db(database = "ebd_mgmt", 
                                       collection = "blocks")
  
  # Condition on whether fields were provided
  if (is.null(fields) == TRUE) {
    # Run query for data frame
    blocks <- connection_blocks$find()
    
  } else {
    
    # If spatial is true, add necessary fields to fields list
    if (spatial == TRUE) {
      fields <- c(fields, "SE_X", "SE_Y", "NW_X", "NW_Y")
    }
    
    # Convert the list of field names to a mongolite filter string
    fields_string <- paste0('{', paste0('"', fields, '" : true', 
                                        collapse = ', '), '}')
    
    # Run query for data frame
    blocks <- connection_blocks$find(fields = fields_string)
  }
  
  # Spatial - get a simple features data frame
  if (spatial == TRUE) {
    # Make a column with Well-known Text from SE_X and SE_Y etc.
    blocks$wkt <- paste0("POLYGON((", blocks$SE_X, " ", blocks$SE_Y, ", ", 
                         blocks$SE_X, " ", blocks$NW_Y, ", ", blocks$NW_X, " ", 
                         blocks$NW_Y, ", ", blocks$NW_X, " ", blocks$SE_Y, ", ", 
                         blocks$SE_X, " ", blocks$SE_Y, "))")
    
    # Use the st_as_sf function to create a simple features data frame
    blocks_sf <- st_as_sf(blocks, wkt = "wkt", crs = 4326)
    
    # Transform coordinate reference system
    if (crs != 4326) {
      blocks_sf <- st_transform(blocks_sf, crs)
    }
    
    return(blocks_sf)
    
  } else {
    
    return(blocks)
  }
}


# ------------------------------------------------------------------------------
to_EBD_format <- function(dataframe, drop = FALSE) {
  # Reformat columns to match that of the EBD
  #
  # Description:
  # Change the column names of a data frame retrieved from the NCBA Atlas Cache 
  #   database to the format of tables retrieved from the eBird EBD.  Also 
  #   assign data types of columns to match those of EBD as read in by the auk 
  #   package.  This functions first assesses whether a data frame has the EBD
  #   format.  If not, it then reformats the data frame to match the EBD.  It 
  #   then checks again for compliance before returning output.
  # 
  # Parameters:
  # dataframe -- a data frame retrieved from the NCBA Atlas Cache.
  # drop -- TRUE or FALSE whether to drop columns not present in the EBD.
  
  # Column capitalization ------------------------------------------------------
  names(dataframe) <- str_to_lower(names(dataframe))
  
  # Column exclusion (dropping) ------------------------------------------------
  # Some names are in the EBD but not the example data set from auk, add them.
  EBD_names <- EBD_fields(case = "lower")
  
  # Drop extra field
  if (drop == TRUE) {
    dataframe <- dataframe %>% select(any_of(EBD_names))
  }
  
  # Data types -----------------------------------------------------------------
  # Transform the data types of the columns in records that are also in EBD and
  #   have different data types.  However, this statement depends on whether the
  #   dataframe is for observations or checklists
  if ("observation_count" %in% names(dataframe)) {
    df2 <- transform(dataframe, bcr_code = as.integer(bcr_code),
                     duration_minutes = as.integer(duration_minutes),
                     effort_area_ha = as.numeric(effort_area_ha),
                     all_species_reported = as.logical(all_species_reported),
                     exotic_code = as.logical(exotic_code),
                     observation_date = as.Date(observation_date),
                     observation_count = as.character(observation_count),
                     has_media = as.logical(has_media),
                     taxonomic_order = as.numeric(taxonomic_order),
                     approved = as.logical(approved),
                     reviewed = as.logical(reviewed)
    )
  } else {
    df2 <- transform(dataframe, bcr_code = as.integer(bcr_code),
                     duration_minutes = as.integer(duration_minutes),
                     effort_area_ha = as.numeric(effort_area_ha),
                     all_species_reported = as.logical(all_species_reported),
                     exotic_code = as.logical(exotic_code),
                     observation_date = as.Date(observation_date)
    )
  }
  
  return(df2)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
breeding_dates <- function(species, basis, quantiles, year){
  # Calculates start and end dates for breeding in North Carolina.
  #
  # Description:
  # Returns a nested list of start and end dates for breeding based upon NCBA 
  #   data.  Provides dates for each ecoregion and statewide.  The basis for 
  #   determining dates can be controlled with the parameters.
  #
  # Notes:
  # The nature of the calculations with dates creates things that users 
  #   should keep in mind.  Here, quantiles are calculated by first converting 
  #   dates to a day of year (number).  Leap years throw this conversion off, 
  #   and the atlas includes one (2024).  Furthermore, converting the start and 
  #   end dates back to a date is affected by the leap year and requires 
  #   specification of a start date (the first day of a particular year).  The 
  #   result of these issues is that reported start and end dates may be "off" 
  #   by a day.  However, this is not a significant amount of error for the 
  #   intended applications of this function.
  #
  # Parameters:
  # species -- common name of the species
  # basis -- the types of breeding categories to bass calculations upon as a 
  #   vector.  For example, c("confirmed", "probable", "possible").
  # quantiles -- vector of lower and upper quantiles to use as the bounds.  For
  #   example, c(0.1, 0.9) for the 10th and 90th quantiles.
  # year -- the year to use for calculations from day of year back to calendar 
  #   year.  Should be an integer.

  library(lubridate)
  
  # GET DATA
  # Observations of the species
  obs <- get_observations(species, NCBA_only = TRUE, EBD_fields_only = FALSE)
  
  # Format columns to the standard analysis format (ebd format)
  obs2 <- to_EBD_format(obs, drop=FALSE)
  
  # Get the blocks data frame
  fields <- c("ID_BLOCK", "ID_EBD_NAME", "ECOREGION")
  blocks <- get_blocks(spatial = FALSE, fields = fields)
  
  # GAIN ECOREGION COLUMN 
  # Join the records to the blocks data frame to gain the ecoregion column
  obs3 <- left_join(obs2, blocks, by = c("ncba_block" = "ID_EBD_NAME")) %>%
    filter(is.na(ECOREGION) == FALSE) 
  
  # Replace abbreviations
  obs3$ECOREGION[obs3$ECOREGION == "CP"] <- "Coastal Plain"
  obs3$ECOREGION[obs3$ECOREGION == "P"] <- "Piedmont"
  obs3$ECOREGION[obs3$ECOREGION == "M"] <- "Mountains"
  
  # WRANGLE BREEDING CODES
  # replace breeding code entries "" with NULL
  obs3["breeding_code"][obs3["breeding_code"] == ""] <- "NULL"
  
  # remove white space from evidence codes
  obs3$breeding_code <- trimws(obs3$breeding_code)
  
  # lump evidence codes to broad categories
  lump <- breeding_codes(lumped = TRUE)
  for (i in seq_along(lump)) {
    indx <- obs3$breeding_code %in% lump[[i]]
    obs3[indx, "breeding_code"] <- names(lump)[i]
  }
  
  # drop records with breeding categories not in basis
  records <- filter(obs3, breeding_code %in% basis)
  
  # GET DATES
  # Make a column with date as day of year
  records$day_of_year <- yday(records$observation_date)
  
  # Define a function for calculating dates and putting into a list.
  get_bounds <- function(records) {
    # Get the bounds (start and end dates)
    bounds <- quantile(records$day_of_year, quantiles)
    #bounds3 <- quantile(records$observation_date, quantiles, type=1)
    
    # Convert to a date, but adjust origin back one day to ensure jan 1 is day 1.  
    #   Otherwise it will be day 0.
    year <- as.integer(year - 1)
    bounds2 <- c(as.Date(bounds[[1]], origin = paste0(year, "-12-31")),
                 as.Date(bounds[[2]], origin = paste0(year, "-12-31")))
    
    result <- format(bounds2, "%m-%d")
    return(result)
  }
  
  # Get statewide dates
  S <- list("statewide" = get_bounds(records))
  
  # Get coastal Plain dates
  records.cp <- filter(records, ECOREGION == "Coastal Plain")
  CP <- list("coastal_Plain" = get_bounds(records.cp))
  
  # Get piedmont dates
  records.p <- filter(records, ECOREGION == "Piedmont")
  P <- list("piedmont" = get_bounds(records.p))
  
  # Get mountains dates
  records.m <- filter(records, ECOREGION == "Mountains")
  M <- list("mountains" = get_bounds(records.m))
  
  # Combine lists
  result <- c(S, CP, P, M)
  
  return(result)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
breeding_boxplot <- function(species, data, type="interactive", 
                             pallet="Paired", omit_codes=NULL,
                             lump=NULL, drop=TRUE, cex.x.axis = 0.9, 
                             cex.y.axis = 0.8, subtitle = NULL) {
  # Produces a boxplot of breeding codes over calendar day.
  #
  # Description:
  #   Produces a boxplot of breeding codes with some customization options.  
  #     Copied from the wbbii_tools repo and altered.
  # 
  # Arguments:
  # species -- common name of the species
  # data -- data frame of ebird or NCBA data
  # type -- whether to create an interactive plot that supports opening
  #   checklist URLs by clicking on data points in the figure, a non-interactive
  #   plot with data from the entire state, or a plot separated out by 
  #   ecoregions. Options are "interactive", "non-interactive", and "ecoregional".
  # pallet -- choose a named RColorBrewer pallet (multiple colors), or a single
  #   color (name or hex); see brewer.pal.info for list and 
  #   display.brewer.all() to view all pallets
  # omit_codes -- a vector of evidence codes not be plotted. For example, 
  #   c("PE", "UN")
  # lump -- a list of named vectors where the vector name is used to place all
  #   codes in the corresponding vector (e.g. 'S = c("S", "S7", "M")' replaces
  #   all "S", "S7", and "M" with "S"). Note that any code that is not already in
  #   variable "codelevels" in function "chronplot" (below) will need to be added
  #   there.
  # drop -- TRUE or FALSE whether to include unreported codes in the plot
  # subtitle -- NULL or text that you wish to use as a subtitle.
  
  library(lubridate)
  library(grid)
  library(gridBase)
  library(RColorBrewer)
  library(ggiraph)
  library(ggplot2)
  
  # Data prep ------------------------------------------------------------------
  ebird <- data # This should eventually be removed and ebird renamed.
  
  # replace breeding code entries "" with NULL
  ebird["breeding_code"][ebird["breeding_code"] == ""] <- "NULL"
  
  # remove white space from evidence codes
  ebird$breeding_code <- trimws(ebird$breeding_code)
  
  # put all dates within the same year -- ignores leap year
  ebird$observation_date <- sub("^20\\d\\d", year(now()) - 1, 
                                ebird$observation_date)
  
  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$observation_date, "%Y-%m-%d")
  
  # Manage Breeding Codes ------------------------------------------------------
  # set drop to true if lump is used
  if (is.null(lump) == FALSE) {
    drop <- TRUE
  }
  
  # set drop to true if no plot codes is used
  if (is.null(omit_codes) == FALSE) {
    drop <- TRUE
  }
  
  # specificy breeding codes and preferred plotting order
  # this vector will need updating if any new codes are introduced via "lump".
  codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "O", "NC", "NULL")
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  
  # add any new codes from the lump categories    
  if (is.null("lump") == FALSE) {
    codelevels <- c(codelevels, names(lump)[! names(lump) %in% codelevels])
  }
  
  # warn of unknown breeding codes in the data
  if (! all(ebird$breeding_code %in% codelevels)) {
    warn <- paste("Not all eBird codes (breeding_code) for",
                  species, "are in codelevels")
    warning(warn)
  }
  
  # add in unreported breeding codes to the data if drop is set to FALSE
  if (drop == FALSE) {
    # add rows with breeding codes from codelevels that are not present in ebird, 
    # but are present in codelevels.  Leave all field blank except for breeding_code.
    # get missing codes
    missing_codes <- codelevels[! codelevels %in% ebird$breeding_code]
    
    # make a dataframe with same columns as ebird where all fields are blank except
    # for breeding_code
    missing <- data.frame(matrix(ncol = ncol(ebird), 
                                 nrow = length(missing_codes)))
    names(missing) <- names(ebird)
    
    # add missing_codes to breeding_codes
    missing$breeding_code <- missing_codes
    
    # add missing codes to ebird
    ebird <- rbind(ebird, missing)
    
    # make breeding codes factors so they are ordered correctly
    ebird <- ebird %>% 
      mutate(breeding_code = factor(ebird$breeding_code, 
                                    levels = codelevels, ordered = TRUE))
  }
  
  # if drop is set to TRUE, use present breeding codes as the code levels
  #   but maintain the desired order.
  if (drop == TRUE) {
    # remove unwanted evidence codes
    if (is.null("omit_codes") == FALSE) {
      ebird <- ebird[! ebird$breeding_code %in% omit_codes, ]
    }
    
    # lump evidence codes if lump has been set
    for (i in seq_along(lump)) {
      indx <- ebird$breeding_code %in% lump[[i]]
      ebird[indx, "breeding_code"] <- names(lump)[i]
    }
    
    # make breeding codes factors so they are ordered correctly
    codelevels <- codelevels[codelevels %in% ebird$breeding_code]
    
    if (is.null("omit_codes") == FALSE) {
      codelevels <- codelevels[! codelevels %in% omit_codes]
    }
    
    ebird <- ebird %>% 
      mutate(breeding_code = factor(ebird$breeding_code, levels = codelevels, 
                                    ordered = TRUE))
  }
  
  
  # Colors ---------------------------------------------------------------------
  # associate colors with codelevels
  if (pallet %in% rownames(brewer.pal.info)) {
    n <- brewer.pal.info[pallet, "maxcolors"]
    codecolors <- colorRampPalette(brewer.pal(n, pallet))(length(codelevels))
  } else {
    codecolors <- rep(pallet, length(codelevels))
  }
  
  # colors 
  names(codecolors) <- codelevels
  
  # add column for color
  ebird$col <- codecolors[ebird$breeding_code]
  
  # Non-interactive plot -------------------------------------------------------
  if (type == "non-interactive") {
    # plot "empty" box plot
    boxplot(obsdate ~ breeding_code, horizontal = TRUE, 
            cex.axis = cex.y.axis, xaxt = "n", data = ebird, border = "white", 
            main = species, las = 2, xlab = "Calendar Day", 
            ylab = "Breeding Code", show.names = TRUE,
            na.action = na.pass)
    
    have_dates <- subset(ebird, is.na(observation_date) == FALSE)
    date0 <- round_date(min(have_dates$obsdate), "month")
    date1 <- round_date(max(have_dates$obsdate), "month")
    labels <- seq(from = date0, to = date1, by = "month")
    
    if (length(unique(month(have_dates$obsdate))) == 1) {
      labels <- c(min(have_dates$obsdate), max(have_dates$obsdate))
      labels <- unique(labels)  # in case there's only one obs
    } else {
      # limit labels to those within observed range
      ##int <- interval(min(have_dates$obsdate), max(have_dates$obsdate))
      ##labels <- labels[labels %within% int]
      
      if (nrow(have_dates) > 1 && length(labels) == 1) {
        labels <- unique(c(min(have_dates$obsdate), max(have_dates$obsdate)))
      }
    }
    
    # use format "%m/%d" for e.g. 06/01
    # use format "%b %d" for e.g. "Aug 23"
    names(labels) <- format(labels, "%b %d")
    
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    
    # label x axis; set font size in gpar(cex = relative_fontsize);
    # grid.text is can be hard to follow but allows for arbitrary rotation of
    # x labels
    grid.text(names(labels), x = unit(labels, "native"),
              y = unit(-0.7, "lines"), just = "right", rot = 65,
              gp = gpar(cex = cex.x.axis))
    popViewport(3)
    
    # add tick marks
    axis(1, labels, labels = FALSE)
    
    # uncomment this to label the x axis a second time for sanity check
    # because grid.text can be difficult to understand
    # axis(1, labels, format(labels, "%m/%d"), col.axis = "red", las = 2)
    
    # select colors for stripchart
    # should be able to use "codecolors[levels(ebird$breeding_code)]",  but
    # that's giving an issue matching the empty string...
    #col <- codecolors[names(codecolors) %in% levels(ebird$breeding_code)]
    col <- unique(ebird$col)
    
    stripchart(obsdate ~ breeding_code, data = ebird, vertical = FALSE,
               method = "jitter", pch = 16, col = col, add = TRUE, 
               na.action = na.pass)
    
    # plot
    boxplot(obsdate ~ breeding_code, horizontal = TRUE,  col = "#F5F5F500",
            yaxt = "n", xaxt = "n", data = ebird, add = TRUE, 
            na.action = na.pass)
  }
  
  # Ecoregional -----------------------------------------------------------------
  if (type == "ecoregional") {
    # Get the blocks data frame
    fields <- c("ID_BLOCK", "ID_EBD_NAME", "ECOREGION", "COUNTY", "ID_WEB_BLOCKMAP")
    blocks <- get_blocks(spatial = FALSE, fields = fields, 
                         crs = 4326)
    
    # Join the records to the blocks data frame to gain the ecoregion column
    records2 <- left_join(ebird, blocks, by = c("ncba_block" = "ID_EBD_NAME")) %>%
      filter(is.na(ECOREGION) == FALSE) 
    
    # Replace abbreviations
    records2$ECOREGION[records2$ECOREGION == "CP"] <- "Coastal Plain"
    records2$ECOREGION[records2$ECOREGION == "P"] <- "Piedmont"
    records2$ECOREGION[records2$ECOREGION == "M"] <- "Mountains"
    
    # Get the values in desired order via making a factor
    records2$ECOREGION <- factor(records2$ECOREGION, 
                                 levels = c("Coastal Plain", "Piedmont",
                                            "Mountains"))
    
    # make a column with suitable x tick values
    ebird$xtick <- as.Date(ebird$observation_date, "%m-%d")
    
    # Boxplot
    result  <- ggplot(data = records2) +
      geom_boxplot(aes(x = obsdate, y = breeding_code)) +
      facet_wrap(~ ECOREGION, nrow=3) + 
      labs(y="Breeding Code", x="Calendar Day", title = species)
    plot(result)
  }
  
  # Interactive plot -----------------------------------------------------------
  if (type == "interactive") {
    ebird$front <- 'https://ebird.org/checklist/'
    ebird$ChecklistLink <- with(ebird, paste0(front, sampling_event_identifier))
    
    # ggiraph code for boxplot and interactive points
    gg_point = ggplot(data = ebird) +
      labs(y="Breeding Code", x="Calendar Day") +
      geom_boxplot(aes(x = obsdate, y = breeding_code)) +
      geom_point_interactive(aes(x = obsdate, y = breeding_code, color = col, 
                                 tooltip = obsdate, data_id = obsdate,
                                 onclick=paste0('window.open("', ChecklistLink,
                                                '", "_blank")')),
                             show.legend = FALSE, 
                             position = position_jitter(width = .2, 
                                                        height = .2)) +
      theme_minimal() + labs(title = species)
    
    girafe(ggobj = gg_point, width_svg=10, 
           options = list(opts_sizing(rescale = TRUE)))
  }
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_checklists <- function(database = "AtlasCache", EBD_fields_only = TRUE,
                               NCBA_only = TRUE, fields = NULL){
  # Get a data frame of checklists from the AtlasCache.  Use get_observations()
  #   instead if you want species observation records.
  # 
  # Parameters:
  # database -- either "EBD" for a downloaded eBird database or "AtlasCache" for
  #   the NCBA mongodb.
  # EBD_fields_only -- whether to include non-EBD, Atlas Cache fields in the output 
  #   data frame. TRUE or FALSE and defaults to FALSE. This argument is set to 
  #   FALSE if the fields argument in not NULL. When database is set to EBD,
  #   this parameter is obsolete.
  # NCBA_only -- whether to exclude non-NCBA project records.  This argument 
  #   is set to FALSE if the fields argument in not NULL.
  # fields -- a list of fields to return, excluding those not listed.  This 
  #   parameter offers no speed benefit with EBD sampling datasets.
  #
  # Notes:
  # - Data frame output when setting database to "AtlasCache" may require 
  #     additional wrangling with the to_EBD_format function before subsequent
  #     functions can be used.
  
  library(tidyverse)
  library(auk)
  
  # Set the working directory
  if (is.null(work_dir) == FALSE) {
    setwd(work_dir)
  }
  
  # If a list of fields is provided, set EBD_fields_only to FALSE
  if (is.null(fields) == FALSE) {
    EBD_fields_only = FALSE
  }
  
  if (database == "AtlasCache") {
    # Connect to the NCBA database
    connection <- connect_ncba_db("ebd_mgmt", "ebd")
    
    # Define a query
    if (NCBA_only == FALSE) {
      query <- '{}'
    } else {
      query <- '{"PROJECT_CODE" : "EBIRD_ATL_NC"}'
    }
    
    # Define a filter that excludes the observation column...
    if (is.null(fields) == TRUE) {
      fields <- '{"OBSERVATIONS" : false}'
      
      # Identify AC fields for omission
      AC.fields <- nonEBD_fields()
      
      # Build a field string for the query if necessary
      if (EBD_fields_only == TRUE) {
        # Convert the list of field names to a mongolite filter string
        fields_string <- paste0('{', paste0('"', AC.fields, '" : false', 
                                            collapse = ', '))
        
        # Redefine fields so that it can be pasted with the filter string
        fields <- ', "OBSERVATIONS" : false}'
        
        # Combine with the existing fields string
        fields <- paste0(fields_string, fields)
      }
      
      # ... but if fields are provided, use those as a filter
    } else {
      # Convert the list of field names to a mongolite filter string
      fields_string <- paste0('{', paste0('"', fields, '" : true', 
                                          collapse = ', '), '}')
      fields <- c(fields_string)
    }
    
    # Retrieve the checklists
    checklists <- connection$find(query = query, fields = fields)
  }
  
  
  if (database == "EBD") {
    library(auk)
    
    # Condition next action on whether NCBA records only are desired.
    if (ncba_only == TRUE) {
      # Read in sampling data frame with auk
      sampling <- EBD_sampling %>%
        auk_sampling() %>%
        auk_project("EBIRD_ATL_NC") %>%
        auk_filter("TMP_EBD.txt") %>%
        read_sampling() %>%
        data.frame()
    } else {
      sampling <- EBD_sampling %>%
        auk_sampling() %>%
        auk_filter("TMP_EBD.txt") %>%
        read_sampling() %>%
        data.frame()
    }
    
    # Subset the columns
    if (is.null(fields) == FALSE) {
      checklists <- select(sampling, fields)
    } else {
      checklists <- sampling
    }
  }
  
  return(checklists)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_all_checklists <- function(drop_ncba_col=TRUE){
  # Get a data frame of checklists from the AtlasCache
  # 
  # Parameters:
  # drop_ncba_col -- Setting to TRUE will drop columns from the NCBA database
  #   that are not provided by eBird. List of columns found in eBird Sampling 
  #   Dataset on 2/18/2022. 
  #
  # Example:
  # lists <- get_all_checklists(drop_ncba_col=FALSE)
  library(tidyverse)
  
  # Connect to the NCBA database
  connection <- connect_ncba_db("ebd_mgmt", "ebd")
  
  # Define a query
  query <- '{}'
  
  # Define a filter that excludes the observation column
  filter <- '{"OBSERVATIONS":0}'
  
  # Retrieve the checklists
  checklists <- connection$find(query = query, fields = filter)
  
  # Make column names lower case to match eBird data download format
  colnames(checklists) <- tolower(colnames(checklists))
  
  # Drop columns not in eBird sampling database.
  colnames(checklists)[1] <- c("checklist_id")
  
  if (drop_ncba_col == TRUE) {checklists %>% select(
    c(checklist_id, last_edited_date, county, county_code,
      iba_code, bcr_code, usfws_code, atlas_block,
      locality, locality_id, locality_type, latitude,
      longitude, observation_date, time_observations_started,
      observer_id, sampling_event_identifier, protocol_type,
      protocol_code, project_code, duration_minutes, 
      effort_distance_km, effort_area_ha, number_observers,
      all_species_reported, group_identifier, trip_comments))}
  else {checklists <- checklists}
  
  # Coerce data types of some columns to match eBird database format
  checklists <- transform(checklists, bcr_code = as.integer(bcr_code),
                          duration_minutes = as.integer(duration_minutes),
                          all_species_reported = as.logical(all_species_reported),
                          observation_date = as.Date(observation_date))
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_observations <- function(species, database = "AtlasCache",
                                 NCBA_only = FALSE,
                                 EBD_fields_only = FALSE,
                                 fields = NULL) {
  # Returns a data frame of species observations
  #
  # Description:
  #   Retrieves the observation records for a species from either the NCBA
  #   database or from a downloaded copy of the EBD.  If data is requested from
  #   the Atlas Cache, then NCBA columns that are not found in the EBD databases
  #   can be dropped or retained. Additionally, a customized list of fields can
  #   be specified to limit the columns that are included in the output data
  #   frame.
  #
  # Parameters:
  # species -- common name of the species
  # database -- either "EBD" for a downloaded eBird database or "AtlasCache" for
  #   the NCBA mongodb.
  # EBD_fields_only -- whether to include non-EBD, Atlas Cache fields in the output
  #   data frame. TRUE or FALSE and defaults to FALSE. This argument is set to
  #   FALSE if the fields argument in not NULL. When database is set to EBD,
  #   this parameter is obsolete.
  # NCBA_only -- whether to exclude non-NCBA project records.  This argument
  #   is set to FALSE if the fields argument in not NULL.
  # fields -- a list of fields to return, excluding those not listed.  This
  #   parameter offers no speed benefit with EBD sampling datasets.
  #
  # Notes:
  # - Data frame output when setting database to "AtlasCache" may require
  #     additional wrangling with the to_EBD_format function before subsequent
  #     functions can be used.
  
  library(tidyverse)
  library(auk)

  # Set the working directory
  if (is.null(work_dir) == FALSE) {
    setwd(work_dir)
  }
  
  # If a list of fields is provided, set EBD_fields_only to FALSE
  if (is.null(fields) == FALSE) {
    EBD_fields_only = FALSE
  }
  
  if (database == "AtlasCache") {
    # Connect to the NCBA database
    connection <- connect_ncba_db("ebd_mgmt", "ebd")
    
    # Define a query
    if (NCBA_only == FALSE) {
      query <- str_interp('{"OBSERVATIONS.COMMON_NAME" : "${species}"}')
    }
    
    if (NCBA_only == TRUE) {
      query <- str_interp('{"PROJECT_CODE" : "EBIRD_ATL_NC",
                          "OBSERVATIONS.COMMON_NAME" : "${species}"}')
    }
    
    # Define a fields filter for the desired columns...
    if (is.null(fields) == TRUE) {
      if (EBD_fields_only == TRUE) {
        # Identify AC fields for omission
        AC.fields <- nonEBD_fields()
        
        # Convert the list of field names to a mongolite filter string
        #   this will allow observations field through which then gets unnested
        #   but that is OK because all fields nested within OBSERVATIONS are
        #   EBD fields
        fields2 <- paste0('{', paste0('"', AC.fields, '" : false',
                                      collapse = ', '), '}')
      }
      
      if (EBD_fields_only == FALSE) {
        fields2 <- "{}"
      }
    }
    
    # ... but if fields are provided, use those as a filter
    if (is.null(fields) == FALSE) {
      # Convert the list of field names to a mongolite filter string
      fields_string <- paste0('{', paste0('"', fields, '" : true',
                                          collapse = ', '))
      
      # Redefine fields so that it can be pasted with the filter string
      fields2 <- ', "OBSERVATIONS" : true}'
      
      # Combine with the existing fields string
      fields2 <- paste0(fields_string, fields2)
    }
    
    # Retrieve the checklists
    records <- connection$find(fields = fields2, query = query) %>%
      unnest(cols = (c(OBSERVATIONS))) %>% # Expand observations
      filter(COMMON_NAME == species) # Rows for non-target species detected along
    # with target species exist and need to be
    # dropped.
    
    # Second pass at dropping unwanted fields (needed because of nested fields).
    if (is.null(fields) == FALSE) {
      # Get a list of names from fields that are still in the columns of records
      dropem <- intersect(names(records), fields)
      
      # Drop the unwanted columns
      records <- records %>% select(any_of(dropem))
    }
    return(data.frame(records))
  }
  
  
  if (database == "EBD") {
    library(auk)
    
    # Condition next action on whether NCBA records only are desired.
    if (NCBA_only == TRUE) {
      # Read in sampling data frame with auk
      ebd <- EBD_observations %>%
        auk_ebd() %>%
        auk_project("EBIRD_ATL_NC") %>%
        auk_species(species = species) %>%
        auk_filter("TMP_EBD.txt", overwrite = TRUE) %>%
        read_ebd() %>%
        data.frame()
    }
    
    if (NCBA_only == FALSE) {
      ebd <- EBD_observations %>%
        auk_ebd() %>%
        auk_species(species = species) %>%
        auk_filter("TMP_EBD.txt", overwrite = TRUE) %>%
        read_ebd() %>%
        data.frame()
    }
    
    # Subset the columns
    if (is.null(fields) == FALSE) {
      checklists <- select(ebd, fields)
    } else {
      checklists <- ebd
    }
  }
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
lists_by_week <- function(checklists){
  # Return a figure of checklists per week
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ observation_date field
  #
  # Example:
  # week.figure <- lists_by_week(get_all_checklists(drop_ncba_col=TRUE))
  # plot(week.figure)
  
  by_week <- checklists %>%
    mutate(week=week(date(observation_date))) %>%
    group_by(week) %>%
    summarize(count=n())
  
  ggplot(data=by_week) +
    geom_line(mapping=aes(y=count, x=week), show.legend=TRUE, color="orange") + 
    labs(title="",
         caption="Checklists from before 2021 are not included") +
    ylab("total number of checklists") +
    scale_x_continuous(limits=c(0,52), breaks=seq(0,52,by=4)) +
    scale_y_continuous(breaks=seq(0,30000,5000))
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
counties_NC <- function(){
  library(maps)
  library(sf)
  # Read in a county spatial data frame in EPSG 6542
  st_as_sf(map("county", plot = FALSE, fill = TRUE)) %>%
  subset(grepl("north carolina", ID)) %>%
  mutate(county = str_to_title(str_replace(ID, "north carolina,", ""))) %>%
  st_transform(6542) %>%
  select(-c(ID))
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
plot_checklists_coords <- function(checklists){
  # Return a map of checklist locations, based on their reported coordinates
  #   
  # Note: points will be mapped in the CRS of the checklists data frame,
  #   which is likely EPSG:4326.
  # 
  # Parameters:
  # checklists -- data frame of checklists, with columns named "latitude" and 
  #   "longitude".
  #
  # Example:
  # coords.map <- plot_checklists_coords(get_checklists())
  # plot(coords.map)
  ggplot(data=checklists) +
    geom_point(mapping=aes(y=latitude, x=longitude), color="darkgreen",
               shape=3) + 
    labs(title="",
         caption="Checklists from before 2021 were not included in this summary") +
    ylab("latitude") + 
    xlab("longitude")
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
effort_distance_boxplot <- function(checklists){
  # Describe the distribution of effort_distance_km values as a boxplot
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ effort_distance_km.
  #
  # Example:
  # effort.dist <- effort_distance_boxplot(get_all_checklists(config, 
  #                                                         drop_ncba_col=TRUE))
  # plot(effort.dist)
  boxplot <- ggplot(data=checklists) +
    geom_boxplot(mapping=aes(y=effort_distance_km, x=""), 
                 color="darkgreen", 
                 outlier.colour="blue", show.legend=TRUE) + 
    coord_flip() + 
    labs(title="",
         caption=" ") +
    ylab("Kilometers") + 
    xlab("Checklist Travel Distances") +
    scale_y_continuous(n.breaks=12)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
duration_minutes_boxplot <- function(checklists){
  # Describe the distribution of effort_minutes values as a box plot.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ effort_minutes.
  #
  # Example:
  # duration.min <- plot_checklists_coords(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(duration.min)
  boxplot <- ggplot(data=checklists) +
    geom_boxplot(mapping=aes(y=duration_minutes, x=""), 
                 color="darkblue", 
                 outlier.colour="orange", show.legend=TRUE) + 
    coord_flip() + 
    labs(title="", caption="") +
    ylab("Minutes") + 
    xlab("Checklist Durations ") +
    scale_y_continuous(n.breaks=12)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
start_time_boxplot <- function(checklists){
  # Describe the distribution of checklist start times as a box plot.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ start time_observations_started.
  #
  # Example:
  # start.box <- start_time_boxplot(get_all_checklists(config, 
  #                                                    drop_ncba_col=TRUE))
  # plot(start.box)
  # make a vector of plottable start times.
  library(hms)
  times <- checklists %>%
    filter(time_observations_started != "") %>% # empty strings cause probs.
    select(time_observations_started) %>%
    mutate(time=hour(as_hms(time_observations_started)))
  
  # make graph
  boxplot <- ggplot(data=times) + 
    geom_boxplot(mapping=aes(y=time, x=""), 
                 color="darkblue", outlier.colour="magenta", show.legend=TRUE) + 
    coord_flip() + 
    labs(title="",
         caption="") + 
    ylab("Time of Day") +
    xlab("Checklist Start Time") +
    scale_y_continuous(n.breaks=12)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
locality_type_pie <- function(checklists){
  # Describe the locality types present as a pie chart: whether checklists are 
  #   for hotspots, personal locations, etc.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ locality_type.
  #
  # Example:
  # locality.pie <- locality_type_pie(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(locality.pie)
  
  by_locality_type <- checklists %>%
    group_by(locality_type) %>%
    summarize(count = n())
  
  ## Print table
  #knitr::kable(by_locality_type,
  #             caption="Count of checklists per locality type")
  
  # Pie chart
  pie <- ggplot(data=by_locality_type, aes(x="", y=count, fill=locality_type)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_viridis_d(alpha = 1, option="D") +
    theme_void() + 
    #theme(legend.title = "Locality Type") +
    labs(title="", caption="") +
    guides(fill=guide_legend(title="Locality Type"))
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
protocol_type_pie <- function(checklists){
  # Describe the protocol types present as a pie chart: whether checklists are 
  #   traveling, stationary, etc.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ protocol_type.
  #
  # Example:
  # protocol.pie <- protocol_type_pie(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(protocol.pie)
  
  by_protocol_type <- checklists %>%
    group_by(protocol_type) %>%
    summarize(count = n())
  
  ## Print table
  #knitr::kable(by_protocol_type,
  #             caption="Count of checklists per protocol type")
  
  # Pie chart
  pie <- ggplot(data=by_protocol_type, aes(x="", y=count, fill=protocol_type)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_viridis_d(alpha = 1, option="D") +
    theme_void() + 
    labs(title="", caption="") +
    guides(fill=guide_legend(title="Protocol Type"))
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
year_bar <- function(checklists){
  # Summarize how many checklists were reported each year.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ year.
  #
  # Example:
  # year.bar <- year_bar(get_all_checklists(config, drop_ncba_col=TRUE))
  # plot(year.bar)
  
  barchart <- ggplot(data=checklists) +
    geom_bar(mapping=aes(x=year), 
             show.legend=FALSE) + 
    labs(title="",
         caption=" ") +
    ylab("Checklists") + 
    xlab("Year") 
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
records_as_sf <- function(records_df, kind, method, fill_na_km = 0.1) {
  # Create new simple features (spatial data frame) of records (checklists or 
  #   observations)  Output can be plotted or used as input for other functions.
  # 
  #   Description: 
  #   Checklist records often need to be assigned geometries for visualization
  #   and spatial analyses, and different methods could be used.  Checklists
  #   can be represented as points or polygons, and polygons could be drawn as 
  #   buffers around the checklist coordinates (circles) or buffers drawn around
  #   checklist tracks.  Buffer length is meant to represent locational
  #   uncertainty (spatial precision) and can be approximated in different ways.
  #   Stationary or short lists should likely be buffered 100 m or more to 
  #   at least partially account for area surveyed.  Null effort_distance_km 
  #   values are filled with zero, which assumes those records are stationary 
  #   counts.
  #
  #   Parameters:
  #   records_df -- data frame of records with latitude, longitude, 
  #     checklists_id or sampling_event_identifier, atlas_block, protocol_type,
  #     and effort_distance_km columns.
  #   kind -- "checklists" or "observations" to identify what type of records are
  #     in the data frame.  Individual species data will be observations.
  #   method -- how to represent each record spatially.  Options are "points",
  #     "point-radius", and "buffered-tracks".
  #   fill_na_km -- NA values may exist in some records, which precludes creating
  #     a point-radius polygon for them. Enter a km distance to use as a 
  #     replacement.  This argument is inconsequential for the point method.
  #   
  #   Results:
  #   A spatial (simple features) data frame with columns for sampling_event_identifier or 
  #     sampling_event_identifier, atlas_block, protocol_type, 
  #     effort_distance_km, latitude, longitude and observation_count and
  #     breeding_code if the kind is observations.
  library(sf)
  
  if (kind == "checklists"){
    records_df <- records_df %>%
      select(sampling_event_identifier, atlas_block, protocol_type, effort_distance_km,
             latitude, longitude)
  } else {
    records_df <- records_df %>%
      select(sampling_event_identifier, atlas_block, protocol_type, 
             effort_distance_km, latitude, longitude, observation_count, 
             breeding_code)
  }
  
  # Make spatial frame
  checklists_sf <- records_df %>%
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
    st_transform(6542)
  
  # Apply method
  if (method == "points") {
    checklists_sf <- checklists_sf
  }
  
  if (method == "point-radius") {
    checklists_sf <- checklists_sf %>%
      # Buffer coordinates
      replace_na(list(effort_distance_km=fill_na_km)) %>%
      mutate(buffer_length = (effort_distance_km)*1000) %>%
      mutate(footprint = st_buffer(geometry, buffer_length)) %>%
      select(-c(geometry)) %>%
      mutate(geometry = footprint) %>%
      st_set_geometry("geometry")
  }
  
  if (method == "buffer-tracks") {
    print("This method is currently unavailable until we get checklist tracks.")
  }
  
  return(checklists_sf)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
checklists_per_block <- function(records_df, blocks_sf, attribute, method){ # DRAFT DRAFT DRAFT
  # Tallies the number of checklists per block.  Records_as_sf() produces
  #   input for this function.
  # 
  # Description:
  # Although coordinates are provided by eBird for checklists, they do not
  # provide precise locations of eBirder effort for two reasons.  First, there
  # are limits to the spatial precision of the points due to gps precision
  # and/or observers ability to identify exactly where they birded on a map or
  # in the app.  Second, many birders travel while birding but their paths are
  # not yet available, only the distances they traveled.  
  # 
  # Locational uncertainty is important and problematic because if it is large
  # in relation to the level of analysis, it creates uncertainty about which
  # spatial subregions, such as counties or atlas blocks, a checklist should
  # be attributed to.
  #
  # Parameters:
  # records_sf -- a data frame of checklists from EBD or the atlas cache.  
  #   The functions "records_as_sf()" can provide this.
  #
  # kind -- what the records are, "checklists" or "observations"? FORTHCOMING
  #
  # blocks_sf -- a spatial data frame of atlas blacks
  #
  # attribute -- column to summarize, such as "checklists"  FORTHCOMING
  # 
  # method -- specify the method to use for attributing checklists to blocks.
  #   Choices are: "A", "B", "C", or "D", but method C is unavailable.
  #   
  #   Method A uses the block identified by eBird in the column "atlas_block".
  #       An abundance of empty values for atlas_block poses a problem.
  #
  #   Method B assigns each checklist to the block that the checklist
  #   coordinate is located within.  This approach could generate deceptive
  #   results if checklists represent birding effort from multiple blocks but
  #   are assigned to a single block or if the coordinate is located in a block
  #   adjacent to where the birding actually occurred.  This approach should
  #   generally be expected to underestimate how many checklists covered some
  #   portion of a given block.  Results are likely the same as with method A.
  #   
  #   Method C is currently unavailable.  It would involve acquiring checklist
  #   tracks and buffering them before intersecting with blocks.
  #
  #   Method D uses polygons instead of the coordinates (points as in Method A) in
  #   order to include the locational uncertainty.  Under this approach,
  #   coordinates are buffered with the distance traveled by the observer
  #   during the checklist period, plus 100 m to account for the fact that
  #   observers may have recorded birds at a distance from where they were
  #   located.  Each checklist is then assigned to all of the blocks that the
  #   polygon intersects in order to acknowledge that the checklist could
  #   represent effort from multiple blocks. Results from this approach can
  #   logically be expected to exaggerate the true footprint of birding effort
  #   and suggest blocks were sampled that actually were not, thus overestimating
  #   how many checklists covered some portion of a given block.  Furthermore,
  #   checklists with large effort distances produce enormous footprints than make
  #   results unhelpful.  Thus, I excluded checklists with effort distances
  #   greater than 5 km for this method.
  # 
  #   Results:
  #   Spatial data frame of blocks with a checklist tally.
  
  if (method=="A") {
    result <- records_sf %>%
      # Summarize by number checklists within each block
      group_by(atlas_block) %>%
      summarize(checklists=n()) %>%
      select(atlas_block, checklists) %>%
      replace_na(list(checklists=0)) %>%
      # Make spatial again
      st_as_sf()}
  
  if (method=="B") {
    records_sf <- records_as_sf(records_df, kind="checklists",
                                method="points")
    result <- records_sf %>%
      # Find which blocks each coordinate is within
      st_join(blocks_sf, join = st_within, left=TRUE) %>%
      # Summarize by number checklists within each block
      group_by(name) %>%
      summarize(checklists=n()) %>%
      select(name, checklists) %>%
      # Join back with blocks spatial frame to fill in zeros (as a data
      # frame)
      data.frame() %>%
      select(-c(geometry)) %>%
      right_join(blocks_sf, by=("name" = "name")) %>%
      replace_na(list(checklists=0)) %>%
      # Make spatial again
      st_as_sf()}
  
  if (method=="C") {
    records_as_sf(checklists_df, method="buffer-tracks", kind="checklists")
    result <- NULL}
  
  if (method=="D") {
    records_sf <- records_as_sf(checklists_df, method="point-radius", 
                                kind="checklists")
    result <- records_sf %>%
      # Intersect footprints with blocks, NOTE this keeps "withins" and fragments
      st_intersection(blocks_sf) %>%
      # Find count by block
      group_by(name) %>%
      summarize(checklists = n()) %>%
      st_drop_geometry() %>%
      # Add zero blocks via a join
      right_join(blocks_sf, by="name") %>%
      select(name, checklists, geometry) %>%
      replace_na(list(checklists=0)) %>%
      st_as_sf()}
  return(result)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
observations_per_block <- function(records_df, blocks_sf, method){ # DRAFT DRAFT DRAFT
  # Tallies the number of observations per block.  Records_as_sf() produces
  #   input for this function.
  # 
  # Description:
  # Although coordinates are provided by eBird for checklists, they do not
  # provide precise locations of eBirder effort for two reasons.  First, there
  # are limits to the spatial precision of the points due to gps precision
  # and/or observers ability to identify exactly where they birded on a map or
  # in the app.  Second, many birders travel while birding but their paths are
  # not available, only the distances they traveled.  
  # 
  # Locational uncertainty is important and problematic because if it is large
  # in relation to the level of analysis, it creates uncertainty about which
  # spatial subregions, such as counties or atlas blocks, an observation should
  # be attributed to.
  #
  # Parameters:
  # records_sf -- a data frame of observations from EBD or the atlas cache.
  #
  # blocks_sf -- a spatial data frame of atlas blacks
  #
  # method -- specify the method to use for attributing observations to blocks.
  #   Choices are: "A", "B", "C", or "D", but method C is unavailable.
  #   
  #   Method A uses the block identified by eBird in the column "atlas_block".
  #       An abundance of empty values for atlas_block poses a problem.
  #
  #   Method B assigns each observation to the block that the checklist
  #   coordinate is located within.  This approach could generate deceptive
  #   results if checklists represent birding effort from multiple blocks but
  #   are assigned to a single block or if the coordinate is located in a block
  #   adjacent to where the birding actually occurred.  This approach should
  #   generally be expected to underestimate how many checklists covered some
  #   portion of a given block.  Results are likely the same as with method A.
  #   
  #   Method C is currently unavailable.  It would involve acquiring checklist
  #   tracks and buffering them before intersecting with blocks.
  #
  #   Method D uses polygons instead of the coordinates (points as in Method A) in
  #   order to include the locational uncertainty.  Under this approach,
  #   coordinates are buffered with the distance traveled by the observer
  #   during the checklist period, plus 100 m to account for the fact that
  #   observers may have recorded birds at a distance from where they were
  #   located.  Each checklist is then assigned to all of the blocks that the
  #   polygon intersects in order to acknowledge that the checklist could
  #   represent effort from multiple blocks. Results from this approach can
  #   logically be expected to exaggerate the true footprint of birding effort
  #   and suggest blocks were sampled that actually were not, thus overestimating
  #   how many checklists covered some portion of a given block.  Furthermore,
  #   checklists with large effort distances produce enormous footprints than make
  #   results unhelpful.  Thus, I excluded checklists with effort distances
  #   greater than 5 km for this method.
  # 
  #   Results:
  #   Spatial data frame of blocks with a tally of individuals reported.
  
  if (method=="A") {
    result <- records_sf %>%
      # Summarize by number checklists within each block
      group_by(atlas_block) %>%
      summarize(individuals=sum(observation_count)) %>%
      select(atlas_block, individuals) %>%
      replace_na(list(individuals=0)) %>%
      # Make spatial again
      st_as_sf()
  }
  
  if (method=="B") {
    records_sf <- records_as_sf(records_df, kind="observations",
                                method="points")
    result <- records_sf %>%
      # Find which blocks each coordinate is within
      st_join(blocks_sf, join = st_within, left=TRUE) %>%
      # Summarize by number checklists within each block
      group_by(name) %>%
      summarize(individuals=sum(observation_count)) %>%
      select(c(name, individuals)) %>%
      # Join back with blocks spatial frame to fill in zeros (as a data
      # frame)
      data.frame() %>%
      select(-c(geometry)) %>%
      right_join(blocks_sf, by=("name" = "name")) %>%
      replace_na(list(individuals=0)) %>%
      # Make spatial again
      st_as_sf()
  }
  
  if (method=="C") {
    records_as_sf(records_df, method="buffer-tracks", kind="observations")
    result <- NULL
  }
  
  if (method=="D") {
    records_sf <- records_as_sf(records_df, method="point-radius", 
                                kind="observations")
    result <- records_sf %>%
      # Intersect footprints with blocks, NOTE this keeps "withins" and fragments
      st_intersection(blocks_sf) %>%
      # Find count by block
      group_by(name) %>%
      summarize(individuals=sum(observation_count)) %>%
      select(c(name, individuals)) %>%
      st_drop_geometry() %>%
      # Add zero blocks via a join
      right_join(blocks_sf, by=("name" = "name")) %>%
      replace_na(list(individuals=0)) %>%
      st_as_sf()
  }
  return(result)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
breeding_codes <- function(lumped = TRUE){
  # Returns either a full or nested list of all breeding codes.
  
  # Parameters:
  # collapsed -- TRUE or FALSE whether you want codes lumped into categories: 
  #   observed, possible, probable, confirmed.
  if (lumped == FALSE) {
    list <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
              "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
              "F", "O", "NC", "NULL")
  }
  
  if (lumped == TRUE) {
    lists <- list(observed = c("F", "NULL", ""), 
                  possible = c("H", "S"),
                  probable = c("S7", "M", "P", "T", "C", "N", "A", "B"), 
                  confirmed = c("PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF",
                                "FY", "FS", "CF", "NE", "NY")
                  )
  }
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
nonEBD_fields <- function(case = "upper") {
  # Returns a list of NCBA only fields that are in the Atlas Cache EBD 
  #   collection.
  fields <- c("GEOM", "NCBA_REVIEW_DATE", "NCBA_REVIEWED", "NCBA_APPROVED",
              "NCBA_REVIEWER", "NCBA_COMMENTS", "NCBA_BLOCK", "ID_BLOCK_CODE", 
              "ID_NCBA_BLOCK", "PRIORITY_BLOCK", "EBD_NOCTURNAL", 
              "NCBA_NOCTURNAL", "NCBA_NOCTURNAL_DURATION", 
              "NCBA_NOCTURNAL_PARTIAL", "NCBA_OBSDT_UTC", "NCBA_SEASON",
              "NCBA_QUARTER", "NCBA_INKIND", "NCBA_BLOCK_CODE",
              "NOCTURNAL", "ID_NCBA_BLOCK_CODE", "SUBSPECIES_SCIENTIFIC_NAME", 
              "SUBSPECIES_COMMON_NAME")
  if (case == "lower") {
    return (str_to_lower((fields)))
  } else {
    return(fields)
  }
}

EBD_fields <- function(case = "upper") {
  # Returns a list of fields present in an EBD download
  fields <- c("checklist_id", "global_unique_identifier", "last_edited_date",
              "taxonomic_order", "category", "common_name", "scientific_name",
              "observation_count", "breeding_code", "breeding_category", 
              "age_sex", "country", "country_code", "state", "state_code",
              "county", "county_code", "iba_code", "bcr_code", "usfws_code", 
              "atlas_block", "locality", "locality_id", "locality_type", 
              "latitude", "longitude", "observation_date", 
              "time_observations_started", "observer_id", 
              "sampling_event_identifier", "protocol_type", "protocol_code",
              "project_code", "duration_minutes", "effort_distance_km", 
              "effort_area_ha", "number_observers", "all_species_reported", 
              "group_identifier", "has_media", "approved", "reviewed",
              "reason", "trip_comments", "species_comments", "behavior_code",
              "taxon_concept_id", "exotic_code", "year")
  if (case == "upper") {
    return (str_to_upper((fields)))
  } else {
    return(fields)
  }
}
  
  