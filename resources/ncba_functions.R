# Functions for often-repeated actions associated with NCBA data management and
# analysis
#
# To access these functions, run 'source("ncba_functions.R")' in your R console,
# script, or R markdown document.  If your working directory is not where
# this file is stored, then replace "ncba_functions.R" with the path to the file
# For example, 'source("C:/Code/NCBA/ncba_functions.R").  The functions can
# then be called by their names.

# this package loads the working directory from the R Studio Project
# be sure to open in R Studio and create a project from the root
if (!require(here)) install.packages(
  "here", repos = "http://cran.us.r-project.org"
)
# alternatively, you can set the working directory explicitly
# setwd("C:/Users/skanderson/OneDrive - State of North Carolina/@@ncba/ncba/Code/NCBA/resources")

if (!require(auk)) install.packages(
  "auk", repos = "http://cran.us.r-project.org"
)
if (!require(tmap)) install.packages(
  "tmap", repos = "http://cran.us.r-project.org"
)
if (!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org"
)

# Load the config file
source(here("resources",  "ncba_config.r"))

# Set the working directory to the work_dir variable from the config file.
#   This may not always work (rmarkdown).....
# setwd(work_dir)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
connect_ncba_db <- function(database, collection){
  # Connect to the NCBA MongoDB database
  #
  # Description:
  # Returns a mongolite connection to the database for use in queries.  Username
  # and password are retrieved from a config file containing variables that can
  # retrieved from the working directory (default) or a user-specified location.
  #
  # Arguments:
  # database -- the database (within MongoDB) to query, likely "ebd_management"
  # collection -- collection name (e.g., "ebd")
  #
  # Example:
  # conn <- connect_ncba_db(database = "ebd_mgmt", collection = "ebd")
  # mongodata <- conn$find({})
  library(mongolite)

  # Database info
  host <- "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
  uri <- sprintf(
    paste0(
      "mongodb://%s:%s@%s/%s?authSource=admin&",
      "replicaSet=atlas-3olgg1-shard-0&readPreference=primary&ssl=true"
    ),
    ncba_db_user,
    ncba_db_pass,
    host,
    database
  )

  # Connect to a specific collection (table)
  m <- mongo(collection = collection, db = database, url = uri)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_blocks <- function(
  spatial = FALSE,
  fields = NULL,
  priority_only = FALSE,
  crs = 4326
) {
  # Returns a data frame of blocks with or without geometries
  #
  # Description:
  #   Retrieves the blocks data set in a data frame or spatial data frame
  #   (simple feature).  A subset of all available fields can be specified
  #   to speed up the query.
  #
  # Arguments:
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
  connection_blocks <- connect_ncba_db(
    database = "ebd_mgmt",
    collection = "blocks"
  )

  # Set query string if priority_only is TRUE
  if (priority_only) {
    querystring <- '{"PRIORITY": "1"}'

  } else {

    querystring <- "{}"
  }

  # Condition on whether fields were provided
  if (is.null(fields) == TRUE) {
    # Run query for data frame
    blocks <- connection_blocks$find(querystring)

  } else {

    # If spatial is true, add necessary fields to fields list
    if (spatial == TRUE) {
      fields <- c(fields, "SE_X", "SE_Y", "NW_X", "NW_Y")
    }

    # Convert the list of field names to a mongolite filter string
    fields_string <- paste0('{', paste0('"', fields, '" : true',
                                        collapse = ', '), '}')

    # Run query for data frame
    blocks <- connection_blocks$find(
      querystring,
      fields = fields_string
      )
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
  # Arguments:
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

  # Drop some problematic fields from AtlasCache
  df2 <- select(df2, ! any_of(c("x_id", "geom")))
  return(df2)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
ncba_unique <- function (x, group_id = "group_identifier",
                         checklist_id = "sampling_event_identifier",
                         species_id = "scientific_name",
                         observer_id = "observer_id",
                         checklists_only = FALSE) {
  # Drops duplicate checklist records that result from group checklists.
  #
  # Copied from the auk package.
  assertthat::assert_that(is.data.frame(x), assertthat::is.flag(checklists_only),
                          assertthat::is.string(group_id), group_id %in% names(x),
                          assertthat::is.string(checklist_id), checklist_id %in%
                            names(x), assertthat::is.string(species_id), checklists_only ||
                            species_id %in% names(x), assertthat::is.string(observer_id),
                          observer_id %in% names(x), is.character(x[[group_id]]),
                          is.character(x[[checklist_id]]), is.character(x[[observer_id]]),
                          checklists_only || is.character(x[[species_id]]))
  if (isTRUE(attr(x, "unique"))) {
    return(x)
  }
  x[[group_id]][x[[group_id]] == ""] <- NA_integer_
  grouped <- !is.na(x[[group_id]])
  x_grouped <- x[grouped, ]
  x_grouped <- x_grouped[order(x_grouped[[checklist_id]]),
  ]
  if (checklists_only) {
    cols <- group_id
  }
  else {
    cols <- c(species_id, group_id)
  }
  ids <- dplyr::select(x_grouped, dplyr::one_of(c(cols, checklist_id,
                                                  observer_id)))
  ids <- dplyr::group_by_at(ids, cols)
  ids <- dplyr::arrange_at(ids, checklist_id)
  ids <- dplyr::summarize(ids, .cid = paste(.data[[checklist_id]],
                                            collapse = ","), .oid = paste(.data[[observer_id]], collapse = ","))
  ids <- dplyr::ungroup(ids)
  x_grouped <- dplyr::inner_join(x_grouped, ids, by = cols)
  x_grouped[[checklist_id]] <- x_grouped$.cid
  x_grouped[[observer_id]] <- x_grouped$.oid
  x_grouped$.cid <- NULL
  x_grouped$.oid <- NULL
  x_grouped <- x_grouped[!duplicated(x_grouped[, cols]), ]
  x$checklist_id <- x[[checklist_id]]
  x_grouped$checklist_id <- x_grouped[[group_id]]
  x <- rbind(x[!grouped, ], x_grouped)
  x <- dplyr::select(x, .data$checklist_id, dplyr::everything())
  attr(x, "unique") <- TRUE
  dplyr::as_tibble(x)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
block_spp_lists <- function(block, start_day = 1, end_day = 365, within = TRUE) {
  # Generates a list of species lists by breeding category for a specified block.
  #
  # Description:
  # ALL observations are pulled from the Atlas Cache for the specified block and
  #   species lists are generated by breeding category.  Any reported species is
  #   considered "observed" regardless of whether a breeding code was submitted
  #   for the species. Results can be restricted to within a period of the year
  #   (e.g., season) by using the start_day and end_day arguments to set the
  #   period bounds.  The start and end days are included in the period.  The
  #   lists returned include one for all species reported for the category (e.g.,
  #   "possible") and one for only those species for which the category is
  #   is the highest reported (e.g., "possible_highest").
  #
  # Arguments:
  # block -- an atlas block ID code.  Passed to get_observations().
  # start_day -- a numbered day of the year for the start of a period.  Can be
  #   obtained with yday().
  # end_day -- a numbered day of the year for the end of a period
  # within -- TRUE or FALSE whether to exclude records from outside of the
  #   period.  TRUE keeps records within the period and FALSE keeps records
  #   from outside of the period. Start and end days are considered part of the
  #   period (i.e. the process is inclusive for within == TRUE and exclusive for
  #   within == FALSE).

  # Get all the observations from the block
  observations <- get_observations(block = block, project = NULL) %>%
    to_EBD_format()

  # Filter on day period
  if (within == TRUE) {
    observations <- observations %>%
      filter(yday(observation_date) >= start_day & yday(observation_date) <= end_day)
  }

  if (within == FALSE) {
    observations <- observations %>%
      filter(yday(observation_date) < start_day | yday(observation_date) > end_day)
  }

  # Get species list from the common name column
  observed_spp <- unique(observations$common_name)
  # Remove ".sp" and "/" names
  observed_spp <- observed_spp[!grepl("sp.", observed_spp)]
  observed_spp <- observed_spp[!grepl("/", observed_spp)]

  # Get a list of confirmed species
  confirmed <- filter(observations, breeding_category == "C4")
  confirmed_spp <- unique(confirmed$common_name)
  # Remove ".sp" and "/" names
  confirmed_spp <- confirmed_spp[!grepl("sp.", confirmed_spp)]
  confirmed_spp <- confirmed_spp[!grepl("/", confirmed_spp)]

  # Get a list of probable species
  probable <- filter(observations, breeding_category == "C3")
  probable_spp <- unique(probable$common_name)
  # Remove ".sp" and "/" names
  probable_spp <- probable_spp[!grepl("sp.", probable_spp)]
  probable_spp <- probable_spp[!grepl("/", probable_spp)]

  # Get a list of possible species
  possible <- filter(observations, breeding_category == "C2")
  possible_spp <- unique(possible$common_name)
  # Remove ".sp" and "/" names
  possible_spp <- possible_spp[!grepl("sp.", possible_spp)]
  possible_spp <- possible_spp[!grepl("/", possible_spp)]

  probable_spp_highest <- setdiff(probable_spp, confirmed_spp)
  possible_spp_highest <- setdiff(possible_spp,
                                  union(confirmed_spp, probable_spp))
  observed_spp_highest <- setdiff(observed_spp,
                                  union(confirmed_spp,
                                        union(possible_spp, probable_spp)))

  # Build a list as output
  output <- list(
    "observed" = observed_spp,
    "possible" = possible_spp,
    "probable" = probable_spp,
    "confirmed" = confirmed_spp,
    "observed_highest" = observed_spp_highest,
    "possible_highest" = possible_spp_highest,
    "probable_highest" = probable_spp_highest,
    "all" = union(
      observed_spp,
      union(union(possible_spp, probable_spp), confirmed_spp)
    )
  )

  return(output)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
summarize_duration <- function(
  block,
  summarize_by,
  start_day,
  end_day,
  within
) {
  # Returns a data frame that summarizes hours of effort within a block
  #
  # Description:
  #   Summarizes hours of survey effort within a block by year, month, or year
  #   and month.  The combination of the start_day, end_day, and within
  #   parameters accommodate queries from within a breeding season.  The default
  #   start and end days allow records from any day into the process and setting
  #   custom start and end days enables restriction to within a certain period
  #   (e.g., breeding season). Setting within to TRUE will accommodate a summer
  #   breeder by only retaining records that are on or after the start day or
  #   before or on the end day.  Setting within to FALSE accommodates winter
  #   breeders by keeping records before the start day or after the end day.
  #
  # Arguments:
  # block -- an eBird block ID code.
  # summarize_by -- time period to summarize by: "year", "month", or "year-month"
  # start_day -- a numbered day of the year for the start of a period.  Can be
  #   obtained with yday().
  # end_day -- a numbered day of the year for the end of a period.
  # within -- TRUE or FALSE whether to exclude records from outside of the
  #   period.  TRUE keeps records within the period and FALSE keeps records
  #   from outside of the period.

  # Get all the checklists from the block
  fields <- c("checklist_id", "year", "month", "ncba_nocturnal", "duration_minutes",
              "observation_date", "atlas_block", "sampling_event_identifier")

  checklists <- get_checklists(block = block, EBD_fields_only = FALSE) %>%
    to_EBD_format() %>%
    auk_unique(checklists_only = TRUE)

  if (within == TRUE) {
    # Filter within period
    checklists <- checklists %>%
      filter(yday(observation_date) >= start_day & yday(observation_date) <= end_day)
  }

  if (within == FALSE) {
    # Filter outside period
    checklists.W <- checklists %>%
      filter(yday(observation_date) < start_day | yday(observation_date) > end_day)
  }

  if (summarize_by == "year") {
    # Summarize
    summary <- checklists %>%
      select(c("year", "ncba_nocturnal", "duration_minutes")) %>%
      group_by(year, ncba_nocturnal) %>%
      summarize(total_hrs = sum(duration_minutes)/60) %>%
      pivot_wider(names_from = ncba_nocturnal, values_from = total_hrs) %>%
      replace_na(list("1" = 0)) %>%
      mutate_if(is.numeric, ~round(., 1))

    # Rename columns
    names(summary) <- c("year", "diurnal_hours", "nocturnal_hours")
  }

  if (summarize_by == "month") {
    # Summarize
    summary <- checklists %>%
      select(c("month", "ncba_nocturnal", "duration_minutes")) %>%
      group_by(month, ncba_nocturnal) %>%
      summarize(total_hrs = sum(duration_minutes)/60) %>%
      pivot_wider(names_from = ncba_nocturnal, values_from = total_hrs) %>%
      replace_na(list("1" = 0)) %>%
      mutate_if(is.numeric, ~round(., 1))

    # Replace month number with abb
    summary$month <- month.abb[summary$month]

    # Rename columns
    names(summary) <- c("month", "diurnal_hours", "nocturnal_hours")
  }

  if (summarize_by == "year-month") {
    # Summarize
    summary <- checklists %>%
      select(c("year", "month", "ncba_nocturnal", "duration_minutes")) %>%
      group_by(year, month, ncba_nocturnal) %>%
      summarize(total_hrs = sum(duration_minutes)/60) %>%
      pivot_wider(names_from = ncba_nocturnal, values_from = total_hrs) %>%
      replace_na(list("1" = 0)) %>%
      mutate_if(is.numeric, ~round(., 1))

    # Replace month number with abb
    summary$month <- month.abb[summary$month]

    # Rename columns
    names(summary) <- c("year", "month", "diurnal_hours", "nocturnal_hours")
  }

  return(summary)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_breeding_dates <- function(species, day_year = FALSE){
  # Gets the start and end day of year for breeding from the Atlas Cache.
  #
  # Description:
  # Returns a list of start and end days of year for breeding.
  #
  # Arguments:
  # species -- common name of the species

  # Connect to the blocks collection (table)
  connection <- connect_ncba_db(database = "ebd_mgmt",
                                collection = "safe_dates")

  # Define query
  query <- str_interp('{"COMMON_NAME" : "${species}"}')

  # Run query for data frame
  df <- connection$find(query = query)

  result <- c(format(ymd(df[["B_SAFE_START_DATE"]]), "%m-%d"),
              format(ymd(df[["B_SAFE_END_DATE"]]), "%m-%d"))

  # Convert to day of year
  if (day_year == TRUE) {
    result <- c(ymd(df[["B_SAFE_START_DATE"]]),
                ymd(df[["B_SAFE_END_DATE"]]))
    result <- yday(result)
  }

  return(result)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
highest_category <- function(species, dataframe = NULL) {
  # Returns a data frame of the highest breeding code reported in each block
  #   with geometries.
  #
  # Description:
  # Creates a data frame with one row for every atlas block that contains the
  #   highest breeding category reported (C4, C3, C2, or C1). If dataframe is
  #   set to NULL, then the get_observations() and to_EBD_format() functions
  #   are used to acquire a data frame of all NCBA observations for the species
  #   from the Atlas Cache.  The output of this function is a simple features
  #   data frame that can be used in maps or table applications.
  #
  # Arguments:
  # species -- the common name of the species of interest.
  # dataframe -- the name of a data frame to use as input.  NULL prompts the use
  #   of get_observations() to access a data frame from the Atlas Cache.
  #
  #
  if (is.null(dataframe) == TRUE) {
    obs <- get_observations(species = species) %>%
      to_EBD_format()
  } else {
    obs <- dataframe
  }

  # Make the breeding_category values a factor to set a rank.  First, check that
  #   the column is present.
  if ("breeding_category" %in% names(obs)) {
    obs$breeding_category <- factor(obs$breeding_categor,
                                    levels = c("C4", "C3", "C2", "C1"))

    # Use summarise to pull out the records with the highest code
    highest <- obs %>%
      filter(is.na(breeding_category) == FALSE) %>%
      arrange(atlas_block, breeding_category) %>%
      group_by(atlas_block) %>%
      summarise(highest_category = first(breeding_category))

    # Get a blocks data frame
    fields <- c("ID_BLOCK_CODE", "ID_EBD_NAME", "PRIORITY")
    blocks_sf <- get_blocks(spatial = TRUE, fields = fields)

    # Join to add NA rows and block geometries.
    highest_sf <- left_join(blocks_sf, highest,
                            by = join_by("ID_BLOCK_CODE" == "atlas_block"))

    return(highest_sf)

  } else {
    print("The provided data frame does not contain a breeding_category column.")
  }
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
calculate_breeding_dates <- function(species, basis, quantiles, year = 2023,
                                     year_day = FALSE){
  # Calculates start and end day of year for breeding in North Carolina.
  #
  # Description:
  # Returns a nested list of start and end day of year for breeding based upon NCBA
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
  # Arguments:
  # species -- common name of the species.
  # basis -- the types of breeding categories to bass calculations upon as a
  #   vector.  For example, c("confirmed", "probable", "possible").
  # quantiles -- vector of lower and upper quantiles to use as the bounds.  For
  #   example, c(0.1, 0.9) for the 10th and 90th quantiles.
  # year -- the year to use for calculations from day of year back to calendar
  #   year.  Should be an integer.
  # year_day -- whether to return results as day of the year.

  library(lubridate)

  # GET DATA
  # Observations of the species
  obs <- get_observations(species = species, project = "EBIRD_ATL_NC",
                          EBD_fields_only = FALSE)

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

    if (year_day == TRUE) {
      result <- bounds
    } else {
      # Convert to a date, but adjust origin back one day to ensure jan 1 is day 1.
      #   Otherwise it will be day 0.
      year <- as.integer(year - 1)
      bounds2 <- c(as.Date(bounds[[1]], origin = paste0(year, "-12-31")),
                   as.Date(bounds[[2]], origin = paste0(year, "-12-31")))
      result <- format(bounds2, "%m-%d")
    }
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
breeding_map <- function(species) {
  # Makes an interactive map of records color coded by breeding category
  #
  # Description:
  # Gets observations for a species and maps them atop a counties layer in an
  #   interactive map.  Clicking on record symbols reveals a hyperlink to the
  #   checklist webpage on ebird.com.
  #
  # Arguments:
  # species -- common name

  # Get the records
  records <- get_observations(species = species, EBD_fields_only = FALSE) %>%
    to_EBD_format() %>%
    auk_unique()

  # Make spatial data frames of all records
  records_sf <- records_as_sf(records, kind = "observations", method = "points") %>%
    right_join(records, by = "sampling_event_identifier") %>%
    filter(breeding_category != "")

  # Add a column with code to open the webpage for each checklist
  records_sf$front <- '<a href = https://ebird.org/checklist/'

  # Strip off any excess identifiers (group checklists produce "S104604778,S104604779")
  records_sf$sampling_event_identifier <- lapply(strsplit(records_sf$sampling_event_identifier, split = ","), function(l) l[[1]])
  records_sf$URL <- with(records_sf, paste0(front, sampling_event_identifier,
                                            ">visit</a>"))

  # Draw the map
  tmap_mode("view")
  tm_shape(shp = counties_NC(), name = "counties") + tm_borders() +
    tm_shape(shp = records_sf, name = "observations") +
    tm_dots(interactive = TRUE, popup.vars = c("URL", "observation_date", "behavior_code"), col = "breeding_category",
            popup.format = list(html.escape = F), border.alpha = 0#,
            #palette = c("yellow", "lightgreen", "darkgreen", "purple"),
            #labels = c("observed", "possible", "probable", "confirmed")
    ) +
    tm_layout(title = species)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
breeding_boxplot <- function(species, data = NULL, type = "interactive",
                             pallet = "Paired", omit_codes = NULL,
                             lump = NULL, drop=TRUE, cex.x.axis = 0.9,
                             cex.y.axis = 0.8, subtitle = NULL) {
  # Produces a boxplot of breeding codes over calendar day.
  #
  # Description:
  #   Produces a boxplot of breeding codes with some customization options.
  #     Copied from the wbbii_tools repo and altered.
  #
  # Arguments:
  # species -- common name of the species.
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
  #
  # Notes:
  # It is possible to use this function for plots of breeding code(s) records
  #   for any species if the data parameter is fed a data frame generated by
  #   the get_breeding_records() function.

  library(lubridate)
  library(grid)
  library(gridBase)
  library(RColorBrewer)
  library(ggiraph)
  library(ggplot2)

  # Data prep ------------------------------------------------------------------
  # ebird <- data # This should eventually be removed and ebird renamed.
  # if data passed, use, otherwise, get obs from species name
  if (is.null(data)) {
    ebird <- to_EBD_format(get_observations(species = species))
  } else {
    ebird <- data
  }

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
            ylab = "", show.names = TRUE,
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
map_records <- function(records, popup.vars = c("URL"), title) {
  # Creates an interactive map of records.
  #
  # Description:
  # Uses the TMAP package to create an interactive map of the provided records.
  #   Designed for use with output from get_breeding_records().  Hovering the
  #   mouse over the record symbol displays the sampling event identifier.
  #   Clicking on symbols opens a small table showing records attributes.  The
  #   popup.vars argument controls what is included in those tables.  Including
  #   "URL" will provide a hyperlink to the checklist webpage on ebird.com.
  #
  #
  # Arguments:
  # records -- dataframe of records
  # popup.vars -- vector containing column names to include in the popup table.
  #   Always include the text "URL" as an item in the vector.
  # title -- a title to use for the map

  # Make data frames of suspicious records and their uncertainty buffers
  sf <- records_as_sf(records, kind = "observations",
                      method = "points") %>%
    select(c("sampling_event_identifier", "geometry")) %>%
    distinct() %>%
    right_join(records, by = "sampling_event_identifier")

  uncertainty_buffer <- records_as_sf(records, kind = "observations",
                                      method = "point-radius") %>%
    filter(buffer_length > 0)

  # Add a column with code to open the webpage for each checklist
  sf$front <- '<a href = https://ebird.org/checklist/'

  # Strip off any excess identifiers; group checklists produce "S104604778,S104604779"
  sf$sampling_event_identifier <- lapply(strsplit(sf$sampling_event_identifier,
                                                  split = ","),
                                         function(l) l[[1]])
  sf$URL <- with(sf, paste0(front, sampling_event_identifier, ">visit</a>"))

  # Draw the map
  tmap_mode("view")
  tm_shape(counties_NC(), name = "Counties") + tm_borders() +
    tm_shape(sf, name = "Records") +
    tm_dots(popup.vars = popup.vars, popup.format = list(html.escape = F)) +
    tm_shape(uncertainty_buffer, name = "Locational Uncertainty") + tm_borders(col = "magenta") +
    tm_layout(title = title)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_checklists <- function(database = "AtlasCache", EBD_fields_only = TRUE,
                           project = "EBIRD_ATL_NC", fields = NULL,
                           observer = NULL, block = NULL){
  # Get a data frame of checklists from the AtlasCache.  Use get_observations()
  #   instead if you want species observation records.
  #
  # Description:
  # This function enables the retrieval of checklist records from the AtlasCache
  #   or the EBD.  It offers options to restrict records to those with a
  #   particular project id, block id, or observer id.  Additionally, one can
  #   specify a subset of fields to return or drop non-EBD fields.  to apply the
  #   to_EBD_format() function to the result of an AtlasCache query, set the
  #   EBD_format variable to TRUE.
  #
  #
  # Arguments:
  # database -- either "EBD" for a downloaded eBird database or "AtlasCache" for
  #   the NCBA mongodb.
  # EBD_fields_only -- whether to include non-EBD, Atlas Cache fields in the output
  #   data frame. TRUE or FALSE and defaults to FALSE. This argument is set to
  #   FALSE if the fields argument in not NULL. When database is set to EBD,
  #   this parameter is obsolete.
  # project -- a project name to filter on.  Default is "EBIRD_ATL_NC".  Setting
  #   to NULL will pull records from all projects.
  # fields -- a list of fields to return, excluding those not listed.  This
  #   parameter offers no speed benefit with EBD sampling datasets.
  # observer -- an observer id to filter based on. For example, "obs421398"
  # block -- a block id to filter based on. For example, "34080G3NW"
  #
  # Notes:
  # - Data frame output when setting database to "AtlasCache" may require
  #     additional wrangling with the to_EBD_format function before subsequent
  #     functions can be used.

  library(tidyverse)
  library(auk)

  # Set the working directory
  # if (is.null(work_dir) == FALSE) {
  #   setwd(work_dir)
  # }

  # If a list of fields is provided, set EBD_fields_only to FALSE
  if (is.null(fields) == FALSE) {
    EBD_fields_only = FALSE
  }

  if (database == "AtlasCache") {
    # Connect to the NCBA database
    connection <- connect_ncba_db("ebd_mgmt", "ncba_functions_ebd_view")

    # Define a query sequentially.  First, address project
    if (is.null(project) == TRUE) {
      query <- '{}'
    } else {
      query <- str_interp('{"PROJECT_CODE" : "${project}"}')
    }

    # Next, address observer
    if (is.null(observer) == FALSE) {
      # Avoid a leading comma
      if (query == '{}') {
        new_end <- str_interp('"OBSERVER_ID" : "${observer}"}')
      } else {
        new_end <- str_interp(', "OBSERVER_ID" : "${observer}"}')
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
      } else {
      query <- query
    }

    # Next, address block id
    if (is.null(block) == FALSE) {
      new_end <- str_interp(', "ATLAS_BLOCK" : "${block}"}')
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
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
    if (is.null(project) == FALSE) {
      # Read in sampling data frame with auk
      sampling <- EBD_sampling %>%
        auk_sampling() %>%
        auk_project(project) %>%
        auk_filter(file = "TMP_EBD.txt", overwrite = TRUE) %>%
        read_sampling() %>%
        data.frame()
    } else {
      sampling <- EBD_sampling %>%
        auk_sampling() %>%
        auk_filter(file = "TMP_EBD.txt", overwrite = TRUE) %>%
        read_sampling() %>%
        data.frame()
    }

    # Pull out desired block and observer
    if (is.null(observer) == FALSE) {
      sampling <- filter(sampling, observer_id == observer)
    } else {
      sampling <- sampling
    }

    if (is.null(block) == FALSE) {
      sampling <- filter(sampling, atlas_block == block)
    } else {
      sampling <- sampling
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
  # Arguments:
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
get_observations <- function(database = "AtlasCache", species = NULL,
                             observer = NULL, block = NULL,
                             project = "EBIRD_ATL_NC", EBD_fields_only = FALSE,
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
  # Arguments:
  # species -- common name of the species
  # database -- either "EBD" for a downloaded eBird database or "AtlasCache" for
  #   the NCBA mongodb.
  # EBD_fields_only -- whether to include non-EBD, Atlas Cache fields in the output
  #   data frame. TRUE or FALSE and defaults to FALSE. This argument is set to
  #   FALSE if the fields argument in not NULL. When database is set to EBD,
  #   this parameter is obsolete.
  # NCBA_only -- whether to exclude non-NCBA project records.  This argument
  #   is set to FALSE if the fields argument in not NULL.
  # fields -- a list of fields to return, excluding those not listed.  The field
  #   names need to be entered in upper case.  This parameter offers no speed
  #   benefit with EBD sampling datasets due to the structure of auk.
  #
  # Notes:
  # - Data frame output when setting database to "AtlasCache" may require
  #     additional wrangling with the to_EBD_format function before subsequent
  #     functions can be used.

  library(tidyverse)
  library(auk)

  # SET UP ---------------------------------------------------------------------
  # Set the working directory
  # if (is.null(work_dir) == FALSE) {
  #   setwd(work_dir)
  # }

  # If a list of fields is provided, set EBD_fields_only to FALSE
  if (is.null(fields) == FALSE) {
    EBD_fields_only = FALSE

    # and make upper case
    fields <- str_to_upper(fields)
  }

  # ATLAS CACHE ----------------------------------------------------------------
  if (database == "AtlasCache") {
    # Connect to the NCBA database
    # connection <- connect_ncba_db("ebd_mgmt", "ebd")
    connection <- connect_ncba_db("ebd_mgmt", "ncba_functions_ebd_view")

    # ---------- QUERY DEFINITION ----------
    # Define a query sequentially.  First, address project
    if (is.null(project) == TRUE) {
      query <- '{}'
    } else {
      query <- str_interp('{"PROJECT_CODE" : "${project}"}')
    }

    # Next, address observer
    if (is.null(observer) == FALSE) {
      # Avoid a leading comma
      if (query == '{}') {
        new_end <- str_interp('"OBSERVER_ID" : "${observer}"}')
      } else {
        new_end <- str_interp(', "OBSERVER_ID" : "${observer}"}')
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
    }

    # Next, address block id
    if (is.null(block) == FALSE) {
      if (query == '{}') {
        new_end <- str_interp('"ATLAS_BLOCK" : "${block}"}')
      } else {
        new_end <- str_interp(', "ATLAS_BLOCK" : "${block}"}')
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
    }

    # Next, address species
    if (is.null(species) == FALSE) {
      if (query == '{}') {
        new_end <- str_interp('"OBSERVATIONS.COMMON_NAME" : "${species}"}')
      } else {
        new_end <- str_interp(', "OBSERVATIONS.COMMON_NAME" : "${species}"}')
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
    }

    # ---------- FIELDS ----------
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
        # don't include this field, duplicated in OBSERVATIONS
        fields2 <- '{}'
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

    # ---------- GET RECORDS ----------
    # Retrieve the checklists
    if (is.null(species) == FALSE) {
      records <- connection$find(fields = fields2, query = query) %>%
        # Expands observations
        unnest(cols = (c(OBSERVATIONS))) %>%
        # Rows for non-target species detected along
        filter(COMMON_NAME == species)
      # with target species exist and need to be dropped.
    } else {
      records <- connection$find(fields = fields2, query = query) %>%
        unnest(cols = (c(OBSERVATIONS)))
    }

    # Second pass at dropping unwanted fields
    # (needed because of nested fields).
    if (is.null(fields) == FALSE) {
      # Get a list of names from fields that
      # are still in the columns of records
      dropem <- intersect(names(records), fields)

      # Drop the unwanted columns
      records <- records %>% select(any_of(dropem))
    }
    return(data.frame(records))
  }

  # EBIRD BASIC DATASET -------------------------------------------------------
  if (database == "EBD") {
    library(auk)

    # Condition next action on whether a single species is specified.
    if (is.null(species) == FALSE) {
      # Read in sampling data frame with auk
      ebd <- EBD_observations %>%
        auk_ebd() %>%
        auk_species(species = species) %>%
        auk_filter("TMP_EBD.txt", overwrite = TRUE) %>%
        read_ebd() %>%
        data.frame()
    }

    if (is.null(species) == TRUE) {
      ebd <- EBD_observations %>%
        auk_ebd() %>%
        auk_filter("TMP_EBD.txt", overwrite = TRUE) %>%
        read_ebd() %>%
        data.frame()
    }

    # Pull out desired project
    if (is.null(project) == FALSE) {
      ebd <- filter(ebd, project_code == project)
    }

    # Pull out desired block
    if (is.null(block) == FALSE) {
      ebd <- filter(ebd, atlas_block == block)
    }

    # Pull out desired observer
    if (is.null(observer) == FALSE) {
      ebd <- filter(ebd, observer_id == observer)
    }

    # Subset the columns
    if (is.null(fields) == FALSE) {
      ebd <- select(ebd, str_to_lower(fields))
    } else {
      checklists <- ebd
    }
    return(ebd)
  }
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_breeding_records <- function(behaviors = NULL,
                                 observer = NULL, block = NULL,
                                 project = "EBIRD_ATL_NC",
                                 EBD_fields_only = FALSE,
                                 fields = NULL,
                                 database = "AtlasCache") {
  # Returns a data frame of observations with breeding/behavior codes
  #
  # Description:
  #   Retrieves the observation records with breeding/behavior codes from either
  #   the NCBA database or from a downloaded copy of the EBD.  Results can be
  #   limited to records with breeding/behavior codes of interest.  If data is
  #   requested from the Atlas Cache, then NCBA columns that are not found in
  #   the EBD databases can be dropped or retained.  Additionally, a customized
  #   list of fields can be specified to limit the columns that are included in
  #   the output data frame.
  #
  # Arguments:
  # behaviors -- a list of breeding/behavior codes of interest.  Setting this to
  #   NULL will return records with any of the breeding/behavior codes.
  # EBD_fields_only -- whether to include non-EBD, Atlas Cache fields in the output
  #   data frame. TRUE or FALSE and defaults to FALSE. This argument is set to
  #   FALSE if the fields argument in not NULL. When database is set to EBD,
  #   this parameter is obsolete.
  # project -- atlas project to access.  This argument
  #   is set to FALSE if the fields argument in not NULL.
  # fields -- a list of fields to return, excluding those not listed.  The field
  #   names need to be entered in upper case.  This parameter offers no speed
  #   benefit with EBD sampling datasets due to the structure of auk.
  # database -- either "EBD" for a downloaded eBird database or "AtlasCache" for
  #   the NCBA mongodb.
  #
  # Notes:
  # - Data frame output when setting database to "AtlasCache" may require
  #     additional wrangling with the to_EBD_format function before subsequent
  #     functions can be used.

  library(tidyverse)
  library(auk)

  # SET UP ---------------------------------------------------------------------
  # Set the working directory
  # if (is.null(work_dir) == FALSE) {
  #   setwd(work_dir)
  # }

  # If a list of fields is provided, set EBD_fields_only to FALSE
  if (is.null(fields) == FALSE) {
    EBD_fields_only = FALSE

    # and make upper case
    fields <- str_to_upper(fields)
  }

  # Handle condition where behavior codes is NULL
  if (is.null(behaviors) == TRUE) {
    behaviors = breeding_codes(lumped = FALSE)
  }

  # ATLAS CACHE ----------------------------------------------------------------
  if (database == "AtlasCache") {
    # Reformat behaviors argument as a string
    behaviors_mongolite <- paste0('["', paste(behaviors, collapse = '", "'), '"]')

    # Connect to the NCBA database
    connection <- connect_ncba_db("ebd_mgmt", "ebd")

    # ---------- QUERY DEFINITION ----------
    # Define a query sequentially.  First, address project
    if (is.null(project) == TRUE) {
      query <- '{}'
    } else {
      query <- str_interp('{"PROJECT_CODE" : "${project}"}')
    }

    # Next, address observer
    if (is.null(observer) == FALSE) {
      # Avoid a leading comma
      if (query == '{}') {
        new_end <- str_interp('"OBSERVER_ID" : "${observer}"}')
      } else {
        new_end <- str_interp(', "OBSERVER_ID" : "${observer}"}')
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
    }

    # Next, address block id
    if (is.null(block) == FALSE) {
      if (query == '{}') {
        new_end <- str_interp('"ATLAS_BLOCK" : "${block}"}')
      } else {
        new_end <- str_interp(', "ATLAS_BLOCK" : "${block}"}')
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
    }

    # Next, address species
    if (is.null(behaviors) == FALSE) {
      if (query == '{}') {
        new_end <- str_interp(
          '"OBSERVATIONS.BEHAVIOR_CODE" : { "$in" : ${behaviors_mongolite} } }'
        )
      } else {
        new_end <- str_interp(
          ', "OBSERVATIONS.BEHAVIOR_CODE" : {"$in":${behaviors_mongolite}} }'
        )
      }
      query <- paste0(substr(query, 1, nchar(query)-1), new_end)
    } else {
      query <- query
    }


    # ---------- FIELDS ----------
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
        fields2 <- '{}'
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

    # ---------- GET RECORDS ----------
    # Retrieve the checklists #
    records <- connection$find(fields = fields2, query = query) %>%
      unnest(cols = (c(OBSERVATIONS))) %>% # Expands observations
      filter(BREEDING_CODE %in% behaviors) # Rows for non-target records detected along
    # with target records exist and need to be dropped.

    # Second pass at dropping unwanted fields (needed because of nested fields).
    if (is.null(fields) == FALSE) {
      # Get a list of names from fields that are still in the columns of records
      dropem <- intersect(names(records), fields)

      # Drop the unwanted columns
      records <- records %>% select(any_of(dropem))
    }
  }

  # EBIRD BASIC DATASET --------------------------------------------------------
  if (database == "EBD") {
    library(auk)

    # Condition next action on whether a single species is specified.
    if (is.null(behaviors) == FALSE) {
      # Read in sampling data frame with auk
      ebd <- EBD_observations %>%
        auk_ebd() %>%
        auk_breeding() %>%
        auk_filter("TMP_EBD.txt", overwrite = TRUE) %>%
        read_ebd() %>%
        data.frame()
    }

    if (is.null(behaviors) == TRUE) {
      ebd <- EBD_observations %>%
        auk_ebd() %>%
        auk_filter("TMP_EBD.txt", overwrite = TRUE) %>%
        read_ebd() %>%
        data.frame()
    }

    # Pull out desired project
    if (is.null(project) == FALSE) {
      ebd <- filter(ebd, project_code == project)
    }

    # Pull out desired block
    if (is.null(block) == FALSE) {
      ebd <- filter(ebd, atlas_block == block)
    }

    # Pull out desired observer
    if (is.null(observer) == FALSE) {
      ebd <- filter(ebd, observer_id == observer)
    }

    # Remove unwanted codes
    ebd <- filter(ebd, behavior_code %in% behaviors)

    # Subset the columns
    if (is.null(fields) == FALSE) {
      records <- select(ebd, str_to_lower(fields))
    } else {
      records <- ebd
    }


  }
  return(records)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
observer_priority_by_breeding <- function(observer, data) {
  # Returns a table with species or block tallies by block type and breeding
  #   category.
  #
  # Description:
  # This function returns a data frame with rows for priority, non-priority,
  #   and either in reference to block type.  The columns are for the various
  #   breeding categories.  The cell values are either species counts or a tally
  #   of how many blocks the observer reported a category in.
  #
  # Arguments:
  # observer -- an eBird observer id
  # data -- enter "species" or "blocks" to pick which data to summarize.
  #
  library(auk)

  # Get a data frame of atlas blocks with the priority column
  blocks <- get_blocks(spatial = FALSE,
                       fields = c("ID_BLOCK_CODE", "PRIORITY"))

  # Get a data frame of the observer's observations, join get PRIORITY
  observations <- get_observations(observer = observer) %>%
    to_EBD_format() %>%
    auk_unique()

  # Drop excess columns in observations and join to get PRIORITY
  obs0 <- observations %>%
    select(c("atlas_block", "common_name", "breeding_category")) %>%
    left_join(blocks, by = join_by("atlas_block" == "ID_BLOCK_CODE")) %>%
    data.frame()

  # Replace the values in breeding_category with words
  obs0$breeding_category <- ifelse(obs0$breeding_category == "C4",
                                   "confirmed",
                                   ifelse(obs0$breeding_category == "C3",
                                          "probable",
                                          ifelse(obs0$breeding_category == "C2",
                                                 "possible",
                                                 ifelse(obs0$breeding_category == "C1",
                                                        "observed", NA))))

  # Fill NA breeding_category values with "observed"
  obs0$breeding_category <- ifelse(is.na(obs0$breeding_category), "observed",
                                   obs0$breeding_category)

  # Replace the values in PRIORITY.  0 = non-priority, 1 = priority.
  obs0$PRIORITY <- ifelse(obs0$PRIORITY == 0, "non-priority", "priority")

  # Species --------------------------------------------------------------------
  if (data == "species") {
    # Make a summary data frame with PRIORITY values as the index, breeding_category values
    # as the columns, and the number of species (i.e., common_names) in each cell.
    obs1 <- obs0 %>%
      group_by(PRIORITY, breeding_category) %>%
      summarise(n = n_distinct(common_name)) %>%
      rename("block_type" = PRIORITY,
             "breeding_category" = breeding_category,
             "species_count" = n) %>%
      data.frame() %>%
      pivot_wider(names_from = breeding_category,
                  values_from = species_count,
                  values_fill = list(species_count = 0)) %>%
      arrange(desc(block_type))

    # Reorder the columns to confirmed, probable, possible, observed
    obs1 <- obs1[, c(1, 2, 5, 4, 3)]

    # Add a row to the bottom of the table with the number of species with
    # each breeding_category value from any atlas block.
    obs1 <- obs1 %>%
      add_row(block_type = "either",
              confirmed = n_distinct(obs0$common_name[obs0$breeding_category == "confirmed"]),
              probable = n_distinct(obs0$common_name[obs0$breeding_category == "probable"]),
              possible = n_distinct(obs0$common_name[obs0$breeding_category == "possible"]),
              observed = n_distinct(obs0$common_name[obs0$breeding_category == "observed"])) %>%
      data.frame()
    return(obs1)
  }

  # Data -----------------------------------------------------------------------
  if (data == "blocks") {
    # Make a summary data frame with PRIORITY values as the index,
    #   breeding_category values as the columns, and the number of blocks
    #   (i.e., atlas_block) in each cell.
    obs2 <- obs0 %>%
      group_by(PRIORITY, breeding_category) %>%
      summarise(n = n_distinct(atlas_block)) %>%
      rename("block_type" = PRIORITY,
             "breeding_category" = breeding_category,
             "block_count" = n) %>%
      data.frame() %>%
      pivot_wider(names_from = breeding_category,
                  values_from = block_count,
                  values_fill = list(block_count = 0)) %>%
      arrange(desc(block_type))

    # Reorder the columns to confirmed, probable, possible, observed
    obs2 <- obs2[, c(1, 2, 5, 4, 3)]

    # Add a row to the bottom of the table for either block type
    obs2 <- obs2 %>%
      add_row(block_type = "either",
              confirmed = sum(obs2$confirmed),
              probable = sum(obs2$probable),
              possible = sum(obs2$possible),
              observed = sum(obs2$observed)) %>%
      data.frame()

    return(obs2)
  }
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
blocks_observed_in <- function(observations, start_day = 0, end_day = 366,
                                  within = TRUE,
                                  breeding_categories = c("C4", "C3", "C2",
                                                          "C1", ""))
{
  # Returns a data frame of blocks where the species was observed
  #
  # Description:
  # This functions offers a flexible way to produce a list of blocks that a
  #   species was observed in.  The combination of the start_day
  #   end_day, and within parameters accommodate queries from within a breeding
  #   season.  The default start and end days allow records from any day into
  #   the process and setting custom start and end days enables restriction to
  #   within a certain period (e.g., breeding season). Setting within to TRUE
  #   will accommodate a summer breeder by only retaining records that are on or
  #   after the start day or before or on the end day.  Setting within to FALSE
  #   accommodates winter breeders by keeping records before the start day or
  #   after the end day.  Unwanted breeding categories can be filtered out of
  #   the data during the process.
  #
  # Note:
  # Non-NCBA records are not assigned to an atlas block and thus get dropped
  #   by this function when is performs a "group by" on the atlas_block field.
  #
  # Arguments:
  # observations -- data frame of observation records obtained with
  #   get_observations() and to_EBD_format().
  # start_day -- a numbered day of the year for the start of a period.  Can be
  #   obtained with yday().
  # end_day -- a numbered day of the year for the end of a period
  # within -- TRUE or FALSE whether to exclude records from outside of the
  #   period.  TRUE keeps records within the period and FALSE keeps records
  #   from outside of the period.
  # breeding_categories -- breeding categories to include.  Defaults to c("C4",
  #   "C3", "C2", "C1")

  # Filter on day period
  if (within == TRUE) {
    obs <- observations %>%
      filter(yday(observation_date) >= start_day & yday(observation_date) <= end_day)
  }

  if (within == FALSE) {
    obs <- observations %>%
      filter(yday(observation_date) < start_day | yday(observation_date) > end_day)
  }

  # Filter on breeding categories
  obs <- filter(obs, breeding_category %in% breeding_categories)

  # Collapse away duplicates
  fields <- c("atlas_block", "common_name")
  obs <- obs %>%
    select(fields) %>%
    distinct()

  return(obs)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
lists_by_week <- function(checklists){
  # Return a figure of checklists per week
  #
  # Arguments:
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
observer_complete_by_priority <- function(observer, data) {
  # Returns a data frame summarizing an observers effort by checklist
  #   completeness and block type.  Reported values can be number of checklists
  #   or number of blocks.
  #
  # Description:
  # Creates a data frame with rows for block type and columns for checklist type
  #   (complete or incomplete).  The data parameter controls what the cell
  #   values are.  Selecting "checklists" will present the number of checklists
  #   in each combination, whereas selecting "blocks" will present the number of
  #   blocks with checklists in each combination.
  #
  # Arguments:
  # observer -- an eBird observer id
  # data -- your choice of what to summarize.  Either "checklists" or "blocks"

  # Get a data frame of atlas blocks with the priority column
  blocks <- get_blocks(spatial = FALSE,
                       fields = c("ID_BLOCK_CODE", "PRIORITY"))

  # Get a data frame of the observer's checklists, join get PRIORITY
  checklists <- get_checklists(observer = observer) %>%
    to_EBD_format() %>%
    auk_unique(checklists_only = TRUE)

  # Drop excess columns in checklists and join to get PRIORITY
  checks0 <- checklists %>%
    select(c("atlas_block", "all_species_reported", "sampling_event_identifier")) %>%
    left_join(blocks, by = join_by("atlas_block" == "ID_BLOCK_CODE")) %>%
    data.frame()

  # Replace the values in PRIORITY.
  checks0$PRIORITY <- ifelse(checks0$PRIORITY == 1, "priority", "non-priority")

  # Replace the values in all_species_reported.
  checks0$all_species_reported <- ifelse(checks0$all_species_reported == TRUE,
                                         "complete", "incomplete")

  if (data == "blocks") {
    # Make a summary data frame.
    obs3 <- checks0 %>%
      group_by(PRIORITY, all_species_reported) %>%
      summarise(n = n_distinct(atlas_block)) %>%
      rename("block_type" = PRIORITY,
             "checklist_type" = all_species_reported,
             "block_count" = n) %>%
      data.frame() %>%
      pivot_wider(names_from = checklist_type,
                  values_from = block_count,
                  values_fill = list(block_count = 0)) %>%
      arrange(desc(block_type))
    return(obs3)
  }

  if (data == "checklists") {
    # Make a summary data frame.
    obs4 <- checks0 %>%
      group_by(PRIORITY, all_species_reported) %>%
      summarise(n = n_distinct(sampling_event_identifier)) %>%
      rename("block_type" = PRIORITY,
             "checklist_type" = all_species_reported,
             "checklist_count" = n) %>%
      data.frame() %>%
      pivot_wider(names_from = checklist_type,
                  values_from = checklist_count,
                  values_fill = list(checklist_count = 0)) %>%
      arrange(desc(block_type))

    # Add a row at the bottom for the column totals.
    obs4 <- obs4 %>%
      add_row(block_type = "either",
              complete = sum(obs4$complete),
              incomplete = sum(obs4$incomplete)) %>%
      data.frame()
    return(obs4)
  }
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
plot_checklists_coords <- function(checklists){
  # Return a map of checklist locations, based on their reported coordinates
  #
  # Note: points will be mapped in the CRS of the checklists data frame,
  #   which is likely EPSG:4326.
  #
  # Arguments:
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
  # Arguments:
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
  # Arguments:
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
  # Arguments:
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
  # Arguments:
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
protocol_table <- function(records) {
  # Returns a summary table of protocol type values from a data frame.
  #
  # Description:
  # Works for data frames of observation or checklist records. The data frame
  #   result summarises the number and percentage of records for each type
  #   as well as the total duration of checklists of each type.
  #
  # Arguments:
  # records -- a data frame of records in in the EBD format.  Can be
  #   observation or checklist records.  Needs to have the duration_minutes
  #   and protocol_type columns.

  # Build the basic table
  basic <- records %>%
    group_by(protocol_type) %>%
    summarize(number = n(), percentage = 100 * (number / nrow(records))) %>%
    arrange(protocol_type)

  # Make a column for total hours
  th <- records %>%
    mutate(hours = duration_minutes / 60) %>%
    group_by(protocol_type) %>%
    summarize(duration_hours = sum(hours)) %>%
    arrange(protocol_type)

  # Add the column with a join
  out <- basic %>%
    left_join(th, by = "protocol_type")

  return(out)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
complete_checklist_table <- function(records) {
  # Returns a summary table of completeness (all species counted).
  #
  # Description:
  # Works for data frames of observation or checklist records. The data frame
  #   result summarises the number and percentage of records from complete
  #   checklists, as well as the total duration of checklists of each type.
  #
  # Arguments:
  # records -- a data frame of records in in the EBD format.  Can be
  #   observation or checklist records.  Needs to have the all_species_reported
  #   and duration_minutes columns.

  # Build the basic table
  basic <- records %>%
    group_by(all_species_reported) %>%
    summarize(number = n(), percentage = 100 *(number / nrow(records))) %>%
    arrange(all_species_reported)

  # Make a column for total hours
  th <- records %>%
    mutate(hours = duration_minutes / 60) %>%
    group_by(all_species_reported) %>%
    summarize(duration_hours = sum(hours)) %>%
    arrange(all_species_reported)

  # Add the column with a join
  out <- basic %>%
    left_join(th, by = "all_species_reported")

  return(out)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
duration_distance_table <- function(records) {
  # Returns a summary table of checklist duration and distance.
  #
  # Description:
  # Works for data frames of observation or checklist records. The data frame
  #   result summarises the number and percentage of records from complete
  #   checklists, as well as the total duration of checklists of each type.
  #   The duration_minutes and effort_distance_km columns must be present.
  #
  # Arguments:
  # records -- a data frame of records in the EBD format.  Can be
  #   observation or checklist records.

  # Start by making a new column with duration in hours.
  records$duration_hours = records$duration_minutes / 60

  # Make a data frame with duration summaries
  time <- data.frame(
    row.names = c("duration (hours)"),
    min = records$duration_hours %>% min(),
    median = records$duration_hours %>% median(),
    max = records$duration_hours %>% max(),
    mean = records$duration_hours %>% mean(),
    sd = records$duration_hours %>% sd(),
    count = records$duration_hours %>% length()
  )

  # Make a data frame with distance summaries
  distance <- data.frame(
    row.names = c("distance (km)"),
    min = records$effort_distance_km %>% min(),
    median = records$effort_distance_km %>% median(),
    max = records$effort_distance_km %>% max(),
    mean = records$effort_distance_km %>% mean(),
    sd = records$effort_distance_km %>% sd(),
    count = records$effort_distance_km %>% length()
  )

  # Concatenate the two data frames
  time_distance <- rbind(time, distance)

  return(time_distance)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
protocol_type_pie <- function(checklists){
  # Describe the protocol types present as a pie chart: whether checklists are
  #   traveling, stationary, etc.
  #
  # Arguments:
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
  # Arguments:
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
  #   Arguments:
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
block_predicted_spp <- function(block, source) {
  # Returns predicted summer and winter species lists.
  #
  # Description:
  # The AtlasCache blocks document contains fields that report species lists
  #   for each block in summer and winter.  Those lists are derived from
  #   predictions by the USGS GAP Analysis Program and eBird.  This function
  #   retrieves lists of species that were predicted to occupy a
  #   block of interest from a source of interest of each summer and winter.
  #
  # Arguments:
  # block -- the eBird ID code of the block of interest.
  # source -- who's prediction do you want: "GAP" or "eBird"?

  source_lookup <- list("GAP" = "GAP_SPP", "eBird" = "EBD_SPP")
  source <- source_lookup[[source]]

  # Connect to the blocks collection (table)
  connection_blocks <- connect_ncba_db(
    database = "ebd_mgmt",
    collection = "blocks"
  )

  # Define and execute a query (with fields) for blocks of predicted presence.
  fields <- str_interp('{"${source}": true}')
  query <- str_interp('{"ID_BLOCK_CODE" : "${block}"}')
  pres <- connection_blocks$find(query = query, fields = fields) %>%
    unnest(source)

  # GAP prediction
  if (source == "GAP_SPP") {
    # Summer list
    summer_spp <- pres %>%
      filter(SUMMER == 1)
    summer_spp <- summer_spp$PRIMARY_COM_NAME

    # Winter list
    winter_spp <- pres %>%
      filter(WINTER == 1)
    winter_spp <- winter_spp$PRIMARY_COM_NAME
  }

  # eBird prediction
  if (source == "EBD_SPP") {
    # Summer list
    summer_spp <- pres %>%
      filter(BREEDING == 1)
    summer_spp <- summer_spp$PRIMARY_COM_NAME

    # Winter list
    winter_spp <- pres %>%
      filter(WINTERING == 1)
    winter_spp <- winter_spp$PRIMARY_COM_NAME
  }

  return(list("summer" = summer_spp, "winter" = winter_spp))
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
get_predicted_presence <- function(species, source, season) {
  # Returns a dataframe of blocks where the species was predicted to occur.
  #
  # Description:
  # The AtlasCache blocks document contains fields that report species lists
  #   for each block in summer and winter.  Those lists are derived from
  #   predictions by the USGS GAP Analysis Program and eBird.  This function
  #   retrieves a dataframe of blocks that were predicted to be occupied by a
  #   species of interested, during a season of interest, and from a source
  #   of interest.
  #
  # Arguments:
  # species -- the common name of the species of interest.
  # source -- who's prediction do you want: "GAP" or "eBird"?
  # season -- "summer" or "winter" for GAP; "breeding" or "wintering" for eBird

  season <- str_to_upper(season)
  source_lookup <- list("GAP" = "GAP_SPP", "eBird" = "EBD_SPP")
  source <- source_lookup[[source]]

  # Connect to the blocks collection (table)
  connection_blocks <- connect_ncba_db(database = "ebd_mgmt",
                                       collection = "blocks")

  # Define and execute a query (with fields) for blocks of predicted presence.
  fields <- '{"ID_BLOCK_CODE": true}'
  query <- str_interp('{"${source}.PRIMARY_COM_NAME": "${species}", "${source}.${season}": 1}')
  pres <- connection_blocks$find(fields = fields, query = query)

  # Add a presence column for the source
  pres$present <- TRUE
  new_name <- paste0(source, "_", season)
  pres <- rename(pres, !!new_name:= present)

  return(pres)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
spp_count_summary <- function(observed_spp, predicted_spp) {
  # Returns a table summarizing species count by breeding category.
  #
  # Description:
  # Provides a summary of how many species have been reported in each category
  #   as a total number of species, percentage of observed species, and
  #   percentage of species predicted/expected to occur.  The observed_spp
  #   must come from block_spp_lists() because this function expects the format
  #   that function returns.
  #
  # Arguments:
  # observed_spp -- a lists of lists retrieved from block_spp_lists().
  # predicted_spp -- a vector of species names.

  # Make empty data frame
  dataframe <- data.frame(
    highest_category = c("observed", "possible", "probable", "confirmed"),
    species_count = c(0, 0, 0, 0),
    percent_of_observed = c(0, 0, 0, 0),
    percent_of_predicted = c(0, 0, 0, 0)
  )

  # Fill out values
  dataframe[dataframe$highest_category == "observed",
            "species_count"] <- length(observed_spp$observed_highest)
  dataframe[dataframe$highest_category == "possible",
            "species_count"] <- length(observed_spp$possible_highest)
  dataframe[dataframe$highest_category == "probable",
            "species_count"] <- length(observed_spp$probable_highest)
  dataframe[dataframe$highest_category == "confirmed",
            "species_count"] <- length(observed_spp$confirmed)

  # Get species count for what's been observed
  total <- length(observed_spp$all)

  dataframe[dataframe$highest_category == "observed",
            "percent_of_observed"] <- round(100*(length(observed_spp$observed_highest)/total))
  dataframe[dataframe$highest_category == "possible",
            "percent_of_observed"] <- round(100*(length(observed_spp$possible_highest)/total))
  dataframe[dataframe$highest_category == "probable",
            "percent_of_observed"] <- round(100*(length(observed_spp$probable_highest)/total))
  dataframe[dataframe$highest_category == "confirmed",
            "percent_of_observed"] <- round(100*(length(observed_spp$confirmed)/total))

  # Get species count for predicted
  total <- length(predicted_spp)

  dataframe[dataframe$highest_category == "observed",
            "percent_of_predicted"] <- round(100*(length(observed_spp$observed_highest)/total))
  dataframe[dataframe$highest_category == "possible",
            "percent_of_predicted"] <- round(100*(length(observed_spp$possible_highest)/total))
  dataframe[dataframe$highest_category == "probable",
            "percent_of_predicted"] <- round(100*(length(observed_spp$probable_highest)/total))
  dataframe[dataframe$highest_category == "confirmed",
            "percent_of_predicted"] <- round(100*(length(observed_spp$confirmed)/total))

  # Add a sum row at the bottom
  dataframe <- rbind(dataframe, c("sum", sum(dataframe$species_count),
                                  sum(dataframe$percent_of_observed),
                                  sum(dataframe$percent_of_predicted)))

  return(dataframe)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
blocks_needed <- function(species, source, season, database, project,
                          observations = NULL) {
  # Returns a data frame of blocks with geometries where the species was
  #   predicted to occur but has not been observed.
  #
  # Description:
  # Relies upon other NCBA functions and AtlasCache datasets to create the
  #   data frame.  The following are used: get_breeding_dates(), get_blocks(),
  #   get_observations(), blocks_observed_in(), and get_predicted_presence().
  #   This function's parameters are passed to some of those functions.  The
  #   function get_observations() can be quite slow for some species, so an
  #   option is provided to pass an observations data frame to avoid needing to
  #   run get_observations() each time this is run.
  #
  # Arguments:
  # species -- the common name of the species of interest.
  # source -- who's prediction you want to base result upon: "GAP" or "eBird"
  # season -- what season to get a prediction for ("summer" or "winter" for GAP,
  #   and "breeding" or "wintering" for eBird)
  # database -- which database to get observations from: "AtlasCache" or "EBD".
  #   Default is AtlasCache
  # project -- project to limit to (e.g., "EBIRD_ATL_NC").
  # observations -- a data frame of observations that came from use of the
  #   get_observations function.  If set to NULL (the default), then the
  #   function will be run as part of the process, which increases runtime
  #   greatly.

  # Pull out breeding season records
  breedates <- get_breeding_dates(species, day_year = TRUE)

  # Get a blocks data frame with simple features
  fields <- c("ID_BLOCK_CODE", "ID_EBD_NAME", "PRIORITY")
  blocks_sf <- get_blocks(spatial = TRUE, fields = fields)

  # Get all the observations for the species
  if (is.null(observations) == TRUE) {
    obs <- get_observations(species = species, database = database,
                            project = project) %>%
      to_EBD_format()
  } else {
    obs <- observations
  }

  # Pull out the blocks with observations
  #   First we have to set the within parameter to TRUE or FALSE according to
  #   desired season.
  if (season %in% c("summer", "SUMMER", "breeding", "BREEDING") == TRUE) {
    within = TRUE
  }

  if (season %in% c("winter", "WINTER", "wintering", "WINTERING") == TRUE) {
    within = FALSE
  }

  # Get a data frame of blocks with observations
  observed <- blocks_observed_in(observations = obs,
                                 start_day = breedates[[1]],
                                 end_day = breedates[[2]],
                                 within = within,
                                 breeding_categories = c("C4", "C3", "C2",
                                                         "C1", ""))$atlas_block

  # Pull out blocks that were in predicted range
  predicted <-get_predicted_presence(species, source, season)$ID_BLOCK_CODE

  # Find blocks from the predicted range that don't have any observations
  needed <- setdiff(predicted, observed)

  # Get a spatially-enabled data frame of records for the needed blocks
  needed_sf <- filter(blocks_sf, ID_BLOCK_CODE %in% needed == TRUE)

  # A column with a single value helps for making maps
  needed_sf$needed <- "needed"

  return(needed_sf)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
breeding_codes <- function(lumped = TRUE){
  # Returns either a full or nested list of all breeding codes.

  # Arguments:
  # lumped -- TRUE or FALSE whether you want codes lumped into categories:
  #   observed, possible, probable, confirmed.
  if (lumped == FALSE) {
    codes <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
              "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
              "F", "O", "NC", "NULL", "")
  }

  if (lumped == TRUE) {
    codes <- list(observed = c("F", "NULL", ""),
                  possible = c("H", "S"),
                  probable = c("S7", "M", "P", "T", "C", "N", "A", "B"),
                  confirmed = c("PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF",
                                "FY", "FS", "CF", "NE", "NY")
                  )
  }
  return(codes)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
visit_checklist <- function(sampling_event_identifier) {
  # Opens the webpage for a checklist.
  ebirdURL <- 'https://ebird.org/checklist/'
  ChecklistLink <- paste0(ebirdURL, sampling_event_identifier)
  browseURL(ChecklistLink)
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

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
map_needed_highest <- function(species, source = "GAP", database = "AtlasCache",
                               priority_only = FALSE) {
  # Returns a ggplot2 map of where breeding observations are needed and the
  #   highest reported breeding category.
  #
  # Description:
  # Returns a map showing the highest breeding code reported per block and the
  #   blocks within which the species was predicted to occur but has not yet
  #   been reported.  The function is written with options to choose a
  #   prediction source (US GAP or eBird), database (AtlasCache or EBD), whether
  #   to use only NCBA records, and whether to return results for non-priority
  #   blocks.
  #
  # Arguments:
  # species -- species common name
  # source -- which source for a prediction: GAP or eBird.  GAP is default.
  # database -- which database to get observations from: AtlasCache or EBD.
  #   AtlasCache is the default.
  # priority_only -- TRUE or FALSE whether to omit non-priority blocks from the
  #   map. Default is FALSE.
  library(auk)

  # Get a blocks data frame with simple features
  blocks <- get_blocks(spatial = TRUE, fields = c("ID_BLOCK_CODE", "PRIORITY"))

  # Get all the species observations
  obs <- get_observations(species = species) %>%
    to_EBD_format() %>%
    auk_unique()

  # Get a spatial data frame of blocks needed in
  if (source == "GAP") {
    season <- "summer"
  }
  if (source == "eBird") {
    season <- "breeding"
  }

  needed <- blocks_needed(species, source = source, season = season,
                          database = database, observations = obs)

  # Get a data frame of highest category per block
  highest <- highest_category(species = species, dataframe = NULL) %>%
    filter(is.na(highest_category) == FALSE)

  # Exclude non-priority blocks if asked to do so
  if (priority_only == TRUE) {
    blocks <- filter(blocks, PRIORITY == 1)
    needed <- filter(needed, PRIORITY == 1)
    highest <- filter(highest, PRIORITY == 1)
  }

  # Make a map with ggplot2
  the_plot <- ggplot() +
    geom_sf(data = blocks, fill = "lightgrey", color = "darkgrey") +
    geom_sf(data = needed, fill = "lightpink", color = "darkgrey") +
    geom_sf(data = highest, aes(fill = highest_category), color = "darkgrey") +
    scale_fill_manual(values = c("C1" = "lightyellow", "C2" = "gold",
                                 "C3" = "lightgreen",
                                 "C4" = "darkgreen"),
                      labels = c("Confirmed", "Probable", "Possible", "Observed"),
                      name = NULL) +
    theme_void() +
    labs(title = str_interp("Highest Breeding Category per Block for ${species}"),
         subtitle = "Blocks needing observations are pink",
         caption = "Data from the NC Bird Atlas") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.054),
          legend.position = c(0.095, 0.22))

  return(the_plot)
}