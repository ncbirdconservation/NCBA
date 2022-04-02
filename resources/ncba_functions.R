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
  library(mongolite)
  # Retrieve credentials
  source(ncba_config)
  
  # Database info
  host <- "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
  uri <- sprintf("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=atlas-3olgg1-shard-0&readPreference=primary&ssl=true", 
                  ncba_db_user, ncba_db_pass, host, database)
  
  # Connect to a specific collection (table)
  m <- mongo(collection=collection, db=database, url=uri)
}

# ------------------------------------------------------------------------------
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
                   "all_species_reported", "group_identifier", "trip_comments", 
                   "breeding_code", "observation_count", "breeding_category", 
                   "has_media", "behavior_category", "subspecies_scientific_name",
                   "subspecies_common_name")
  if (drop == TRUE) {
    x <- dataframe %>% select(all_of(ebd_columns))
  }
  else {
    x <- dataframe
  }
}

# ------------------------------------------------------------------------------
breeding_boxplot <- function(species, data, pallet, out_pdf, no_plot_codes,
                             lump, drop, cex.x.axis = 0.9, cex.y.axis = 0.8) {
  # Produces a boxplot of breeding codes over calendar day.
  #
  # Description:
  #   Produces a boxplot of breeding codes with some customization options.  
  #     Copied from the wbbii_tools repo.
  # 
  # Arguments:
  # species -- common name of the species
  # data -- data frame of ebird or NCBA data
  # pallet -- choose a named RColorBrewer pallet (multiple colors), or a single color (name
  #   or hex); see brewer.pal.info for list and display.brewer.all() to view all
  #   pallets
  # out_pdf -- path and name where to save an output pdf.  Set to NULL if you
  #   don't want to save output.
  # no_plot_codes -- a vector of evidence codes not be plotted. For example, 
  #   c("PE", "UN")
  # lump -- a list of named vectors where the vector name is used to place all
  #   codes in the corresponding vector (e.g. 'S = c("S", "S7", "M")' replaces all
  #   "S", "S7", and "M" with "S"). Note that any code that is not already in
  #   variable "codelevels" in function "chronplot" (below) will need to be added
  #   there.
  # drop -- TRUE or FALSE whether to include unreported codes in the plot
  library(lubridate)
  library(grid)
  library(gridBase)
  library(RColorBrewer)
  
  # Data prep
  ebird <- data # THis should eventually be removed and ebird renamed.
  # put all dates within the same year -- ignores leap year
  ebird$observation_date <- sub("^20\\d\\d", "2016", ebird$observation_date)
  
  # remove white space from evidence codes
  ebird$breeding_code <- trimws(ebird$breeding_code)
  
  # lump evidence codes if lump has been set
  if (is.null("lump") == FALSE) {
    for (i in seq_along(lump)) {
      indx <- ebird$breeding_code %in% lump[[i]]
      ebird[indx, "breeding_code"] <- names(lump)[i]
    }
  }
  
  # remove unneeded evidence codes
  if (is.null("no_plot_codes") == FALSE) {
    ebird <- ebird[! ebird$breeding_code %in% no_plot_codes, ]
  }
  
  # rename columns because ebird names are long
  cols <- c("common_name", "breeding_code", "observation_date")
  newnames <- c("name", "code", "obsdate")
  ebird <- ebird[ebird$common_name == species, cols]
  names(ebird) <- newnames
  
  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%Y-%m-%d")
  
  # set order that box plots will be plotted.
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in.
  # this vector will need updating if any new codes are introduced via "lump".
  codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "", "O", "NC")
  
  if (! all(ebird$code %in% codelevels)) {
    warn <- paste("Not all eBird codes (breeding_code) for",
                  species, "are in codelevels")
    warning(warn)
  }
  
  # associate colors with codelevels
  if (pallet %in% rownames(brewer.pal.info)) {
    n <- brewer.pal.info[pallet, "maxcolors"]
    codecolors <- colorRampPalette(brewer.pal(n, pallet))(length(codelevels))
  } else {
    codecolors <- rep(pallet, length(codelevels))
  }
  
  names(codecolors) <- codelevels
  
  # used droplevels so that codes that where not observed are not plotted;
  # remove droplevels if you'd like unobserved codes to be included on the plot
  if (drop == TRUE) {
    ebird$code <- droplevels(factor(ebird$code, levels = codelevels,
                                    ordered = TRUE))
  } 
  
  # plot "empty" box plot
  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = cex.y.axis, xaxt = "n",
          data = ebird, border = "white", main = species, las = 2,
          xlab = "Date", ylab = "Breeding Codes", show.names = TRUE)
  
  date0 <- round_date(min(ebird$obsdate), "month")
  date1 <- round_date(max(ebird$obsdate), "month")
  labels <- seq(from = date0, to = date1, by = "month")
  
  if (length(unique(month(ebird$obsdate))) == 1) {
    labels <- c(min(ebird$obsdate), max(ebird$obsdate))
    labels <- unique(labels)  # in case there's only one obs
  } else {
    # limit labels to those within observed range
    int <- interval(min(ebird$obsdate), max(ebird$obsdate))
    labels <- labels[labels %within% int]
    
    if (nrow(ebird) > 1 && length(labels) == 1) {
      labels <- unique(c(min(ebird$obsdate), max(ebird$obsdate)))
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
  grid.text(names(labels), x = unit(labels, "native"), y = unit(-0.7, "lines"),
            just = "right", rot = 65, gp = gpar(cex = cex.x.axis))
  popViewport(3)
  
  # add tick marks
  axis(1, labels, labels = FALSE)
  
  # uncomment this to label the x axis a second time for sanity check
  # because grid.text can be difficult to understand
  # axis(1, labels, format(labels, "%m/%d"), col.axis = "red", las = 2)
  
  # select colors for stripchart
  # should be able to use "codecolors[levels(ebird$code)]",  but
  # that's giving an issue matching the empty string...
  col <- codecolors[names(codecolors) %in% levels(ebird$code)]
  
  stripchart(obsdate ~ code, data = ebird, vertical = FALSE, method = "jitter",
             pch = 16, col = col, add = TRUE)
  
  #set boxplot color and partial transparency (where alpha is opacity)
  #run mycol to get the color code, then paste it into the next line
  #mycol <- rgb(245, 245, 245, max = 255, alpha = 0, names = "ltgrayclear")
  #mycol
  
  # plot
  boxplot(obsdate ~ code, horizontal = TRUE,  col = "#F5F5F500", yaxt = "n", 
          xaxt = "n", data = ebird, add = TRUE)
}


# ------------------------------------------------------------------------------
get_all_checklists <- function(ncba_config, drop_ncba_col=TRUE){
  # Get a data frame of checklists from the NCBAdb.
  # 
  # Parameters:
  # ncba_config -- Config file with NCBA MongoDB username and password
  # drop_ncba_col -- Setting to TRUE will drop columns from the NCBA database
  #   that are not provided by eBird. List of columns found in eBird Sampling 
  #   Dataset on 2/18/2022. 
  #
  # Example:
  # lists <- get_all_checklists("~/Documents/NCBA/Scripts/ncba_config.R",
  #                             drop_ncba_col=FALSE)
  library(tidyverse)
  
  # Connect to the NCBA database
  connection <- connect_ncba_db(ncba_config, "ebd_mgmt", "ebd")
  
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
lists_by_week <- function(checklists){
  # Return a figure of checklists per week
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ observation_date field
  #
  # Example:
  # week.figure <- lists_by_week(get_all_checklists(config, drop_ncba_col=TRUE))
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
counties <- function(){
  # Read in a county spatial data frame in EPSG 6542
  st_as_sf(map("county", plot = FALSE, fill = TRUE)) %>%
  subset(grepl("north carolina", ID)) %>%
  mutate(county = str_to_title(str_replace(ID, "north carolina,", ""))) %>%
  st_transform(6542) %>%
  select(-c(ID))
}


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
  # coords.map <- plot_checklists_coords(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(coords.map)
  library(tidyverse)
  ggplot(data=checklists) +
    geom_point(mapping=aes(y=latitude, x=longitude), color="darkgreen",
               shape=3) + 
    labs(title="",
         caption="Checklists from before 2021 were not included in this summary") +
    ylab("latitude") + 
    xlab("longitude")
}

# ------------------------------------------------------------------------------
effort_distance_boxplot <- function(checklists){
  # Describe the distribution of effort_distance_km values as a boxplot
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ effort_distance_km.
  #
  # Example:
  # coord.plot <- plot_checklists_coords(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(coord.plot)
  boxplot <- ggplot(data=checklists) +
    geom_boxplot(mapping=aes(y=effort_distance_km, x=""), 
                 color="darkgreen", 
                 outlier.colour="blue", show.legend=TRUE) + 
    coord_flip() + 
    labs(title="",
         caption="Checklists from before 2021 are not included") +
    ylab("Kilometers") + 
    scale_y_continuous(n.breaks=12)
}

# ------------------------------------------------------------------------------
duration_minutes_boxplot <- function(checklists){
  # Describe the distribution of effort_minutes values as a box plot.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ effort_minutes.
  #
  # Example:
  # effort_distance_boxplot <- plot_checklists_coords(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(cords.map)
  boxplot <- ggplot(data=checklists) +
    geom_boxplot(mapping=aes(y=duration_minutes, x=""), 
                 color="darkblue", 
                 outlier.colour="orange", show.legend=TRUE) + 
    coord_flip() + 
    labs(title="", caption="") +
    ylab("Minutes") + 
    xlab("") +
    scale_y_continuous(n.breaks=12)
}

# ------------------------------------------------------------------------------
start_time_boxplot <- function(checklists){
  # Describe the distribution of checklist start times as a box plot.
  # 
  # Parameters:
  # checklists -- data frame of checklists w/ start time_observations_started.
  #
  # Example:
  # start.box <- start_time_boxplot(get_all_checklists(config, 
  #                                                        drop_ncba_col=TRUE))
  # plot(start.box)
  library(hms)
  start_df <- checklists %>%
    select(time_observations_started) %>%
    mutate(time=hour(as_hms(time_observations_started)))
  
  boxplot <- ggplot(data=start_df) +
    geom_boxplot(mapping=aes(y=time, x=""), color="darkblue", 
                 outlier.colour="magenta", show.legend=TRUE) + 
    coord_flip() + 
    labs(title="",
         caption="Checklists from before 2021 are not included") +
    ylab("Start Hour") + 
    scale_y_continuous(n.breaks=12)
}

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
    labs(title="", caption="")
}

# ------------------------------------------------------------------------------
records_as_sf <- function(records_df, kind, method){# DRAFT DRAFT DRAFT
  # Create new simple features (spatial data frame) of checklists.  Output can
  #   be plotted, but the primary use will be as input for other functions.
  # 
  #   Description: 
  #   Checklist records often need to be assigned geometries for visualization
  #   and spatial analyses, and different methods could be used.  Checklists
  #   can be represented as points or polygons and polygons could be drawn as 
  #   buffers around the checklist coordinates (circles) or buffers drawn around
  #   checklist tracks.  Buffer length is meant to represent locational
  #   uncertainty and can be approximated in different ways that are currently
  #   supported.  Stationary or short lists should likely be buffered 100 m or
  #   more to account for area surveyed.  Lists traveling > 5 km are
  #   problematic so removed here.  Null effort_distance_km
  #   values are filled with zero, which assumes those records are stationary
  #   counts.
  #
  #   Parameters:
  #   records_df -- data frame of checklists with latitude, longitude, 
  #     checklists_id or sampling_event_identifier, atlas_block, protocol_type,
  #     and effort_distance_km columns.
  #   kind -- "checklists" or "observations" to identify what type of records are
  #     in the data frame.  Individual species data will be observations.
  #   method -- how to represent each record spatially.  Options are "points",
  #     "point-radius", and "buffered-tracks".
  #   
  #   Results:
  #   A spatial (simple features) data frame with columns for checklist_id or 
  #     sampling_event_identifier, atlas_block, protocol_type, 
  #     effort_distance_km, latitude, longitude.
  
  library(sf)
  
  if (kind == "checklists"){
    records_df <- records_df %>%
      select(checklist_id, atlas_block, protocol_type, effort_distance_km,
             latitude, longitude)
  } else {
    records_df <- records_df %>%
      select(sampling_event_identifier, atlas_block, protocol_type, 
             effort_distance_km, latitude, longitude, observation_count)
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
      replace_na(list(effort_distance_km=0)) %>%
      filter(effort_distance_km <= 5) %>%
      mutate(buffer_length = (effort_distance_km + 0.1)*1000) %>%
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
  # spatial subregions, such as counties or atlas blocks, a checklist should
  # be attributed to.
  #
  # Parameters:
  # records_sf -- a data frame of observations from EBD or the atlas cache.
  #
  # blocks_sf -- a spatial data frame of atlas blacks
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