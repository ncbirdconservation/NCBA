# utility function to use throughout


#####################################################################################
# MongoDB
# this is a read only account
HOST = "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
DB = "ebd_mgmt"
COLLECTION = "ebd"
source("ncba_config.r")
# other relevant collections include: blocks and ebd_taxonomy

URI = sprintf("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=atlas-3olgg1-shard-0&readPreference=primary&ssl=true",USER, PASS, HOST, DB)

# connect to a specific collection (table)
m <- mongo(COLLECTION, url = URI, options = ssl_options(weak_cert_validation = T))
m_spp <- mongo("ebd_taxonomy", url = URI, options = ssl_options(weak_cert_validation = T))
m_blocks <- mongo("blocks", url = URI, options = ssl_options(weak_cert_validation = T))
m_sd <- mongo("safe_dates", url = URI, options = ssl_options(weak_cert_validation = T))

get_safe_dates <- function(){
  sd <- m_sd$find("{}","{}")

  #ADD JULIAN DATE COLUMNS
  sd$B_SAFE_START_JULIAN <- apply(sd['B_SAFE_START_DATE'],1,function(x){yday(x[1])})
  sd$B_SAFE_END_JULIAN <- apply(sd['B_SAFE_END_DATE'],1,function(x){yday(x[1])})

  return(sd[c('TAX_NO','COMMON_NAME','B_SAFE_START_JULIAN', 'B_SAFE_END_JULIAN')])
}

safe_dates <- get_safe_dates()

# this query follows JSON based query syntax (see here for the basics: https://jeroen.github.io/mongolite/query-data.html#query-syntax)
# TESTING INFO
# low checklist block -> "PAMLICO_BEACH-CW" or "GRIMESLAND-NW"
# this works:
#   get_mongo_data('{"ID_NCBA_BLOCK":"GRIMESLAND-CW"}', '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}', FALSE)
#   get_mongo_data('{"OBSERVATIONS.COMMON_NAME":"Cerulean Warbler"}', '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1, "OBSERVATIONS.COMMON_NAME":1, "OBSERVATIONS.OBSERVATION_COUNT":1, "OBSERVATIONS.BEHAVIOR_CODE":1, "OBSERVATIONS.BREEDING_CATEGORY":1}')

get_ebd_data <- function(query="{}", filter="{}"){
# Retrieves data from MongoDB Atlas implementation
#
# Description:
#   Returns a dataframe of records from the NC Bird Atlas MongoDB implementation. If OBSERVATION fields are included in the requested output, flattens the dataframe. If a species is specificed, all observations from the checklist are returned.
#
# Arguments:
# query -- JSON formatted MongoDB query
# fitler -- JSON formatted "project" parameter in MongoDB format
#
# Examples:
#   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns from checklists in the GRIMESLAND-CW block
#     get_ebd_data('{"ID_NCBA_BLOCK":"GRIMESLAND-CW"}', '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')
#   2. Retrieve OBSERVATION_DATE, SAMPLING_EVENT_IDENTIFIER, OBSERVATIONS.COMMON_NAME, OBSERVATIONS.OBSERVATION_COUNT, OBSERVATIONS.BEHAVIOR_CODE, OBSERVATIONS.BREEDING_CATEGORY for all Cerulean Warbler detections.
#     get_ebd_data('{"OBSERVATIONS.COMMON_NAME":"Cerulean Warbler"}', '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1, "OBSERVATIONS.COMMON_NAME":1, "OBSERVATIONS.OBSERVATION_COUNT":1, "OBSERVATIONS.BEHAVIOR_CODE":1, "OBSERVATIONS.BREEDING_CATEGORY":1}')

#specify the default  fields to return if no filter passed
  # do not run if no query passed
  if (query != "{}"){
    sortquery <- '{"OBSERVATION_DATE":1, "TIME_OBSERVATIONS_STARTED":1, "SAMPLING_EVENT_IDENTIFIER":1}'
    if (grepl("OBSERVATIONS", filter, fixed=TRUE) | filter=="{}"){
      # WORKING VERSION - downloads and returns all checklist obs
      if (filter == "{}") {
        # DEFINE DEFAULT FILTER that excludes unused fields
        filter <- '{"ALL_SPECIES_REPORTED":1,"ATLAS_BLOCK":1,"BCR_CODE":1,"COUNTRY":1,"COUNTRY_CODE":1,"COUNTY":1,"COUNTY_CODE":1,"DURATION_MINUTES":1,"EFFORT_AREA_HA":1,"EFFORT_DISTANCE_KM":1,"GROUP_IDENTIFIER":1,"IBA_CODE":1,"ID_BLOCK_CODE":1,"ID_NCBA_BLOCK":1,"LAST_EDITED_DATE":1,"LATITUDE":1,"LOCALITY":1,"LOCALITY_ID":1,"LOCALITY_TYPE":1,"LONGITUDE":1,"MONTH":1,"NUMBER_OBSERVERS":1,"OBSERVATIONS":1,"OBSERVATION_DATE":1,"OBSERVER_ID":1,"PRIORITY_BLOCK":1,"PROJECT_CODE":1,"PROTOCOL_CODE":1,"PROTOCOL_TYPE":1,"SAMPLING_EVENT_IDENTIFIER":1,"STATE":1,"STATE_CODE":1,"TIME_OBSERVATIONS_STARTED":1,"TRIP_COMMENTS":1,"USFWS_CODE":1,"YEAR":1}'

        # fields excluded
        # "GEOM.coordinates":1,"GEOM.type":1,"NCBA_APPROVED":1,"NCBA_BLOCK":1,"NCBA_COMMENTS":1,"NCBA_REVIEWED":1,"NCBA_REVIEWER":1,"NCBA_REVIEW_DATE":1,
      }

      print("getting Observations from AtlasCache")
      mongodata <- m$find(query, filter)
      # mongodata <- m$find(query, filter, sort=sortquery) #sorting breaks for big queries

      if (nrow(mongodata)>0) {
        print("unnesting observation records")
        mongodata <- unnest(mongodata, cols = (c(OBSERVATIONS)))

        #ADD SEASON COLUMN FROM SAFE DATES TABLE AND POPULATE
        gen_breeding_start <- yday("2021-05-01")
        gen_breeding_end <- yday("2021-08-30")

        mongodata$SEASON <- apply(mongodata[c('OBSERVATION_DATE','COMMON_NAME')],1, function(x) {
          odj <- yday(x[1])
          sd <- filter(sd,COMMON_NAME == x[2])

          if (nrow(sd) == 0 ) {
            begin <- gen_breeding_start
            end <- gen_breeding_end
          } else {
            begin <- sd['B_SAFE_START_JULIAN']
            end <- sd['B_SAFE_END_JULIAN']
          }

          season <- "Non-Breeding"
          if (begin <= odj & odj <= end){
            season <- "Breeding"
          }
          return(season)

        })
      } # Expand observations if records returned


      # EXAMPLE/TESTING
      print("AtlasCache records retrieved")
      # print(head(mongodata))
      # USE aggregation pipeline syntax to return only needed observations
      # pipeline <- str_interp('[{$match: ${query}}, {$project:${filter}}, {$unwind: {path: "$OBSERVATIONS"}}]')
      #
      # mongodata <- m$aggregate(pipeline) %>%
      # unnest(cols = (c(OBSERVATIONS)))

    } else {
      mongodata <- m$find(query, filter)
    }
    return(mongodata)
  }
}




get_block_data <- function() {
  # Retrieves block data table from MongoDB Atlas implementation
  blockdata <- m_blocks$find("{}","{}")
  return(blockdata)

}

# DEPRECATED
# get_block_checklists <- function(block = "", portal = FALSE) (
#   # Retrieves data from MongoDB Atlas implementation
#   #
#   # Description:
#   #   Returns a dataframe of records from the NC Bird Atlas MongoDB implementation in a format to plot on the map.
#   #
#   # Arguments:
#   # block -- string that corresponds to the ID_NCBA_BLOCK field
#   #
#   # Examples:
#   #   1. Retrieve checklists submitted to the portal from the GRIMESLAND-CW block
#   #     get_block_checklists('GRIMESLAND-CW', TRUE)
#
#   if (block != ""){
#     if (portal) {
#       query <- str_interp('{"ID_NCBA_BLOCK":"${block}", "PROJECT_CODE":"EBIRD_ATL_NC"}')
#     } else {
#       query <- str_interp('{"ID_NCBA_BLOCK":"${block}"}')
#     }
#     print(query)
#
#     filter <- '{"LATITUDE":1, "LONGITUDE":1, "SAMPLING_EVENT_IDENTIFIER":1, "LOCALITY_ID":1, "OBSERVATION_DATE":1}'
#     print(filter)
#
#     return(get_ebd_data(query, filter))
#   }
#
# )
#####################################################################################
# Species
get_spp_obs <- function(species, filter){
  # wrapper function for retrieving species records
  #
  # Description:
  #   Returns datafram of requested observations from the EBD collection
  # Arguments:
  # species -- Common name of the species data to be retrieved
  # fitler -- JSON formatted "project" parameter in MongoDB format
  #
  # Examples:
  #   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns from checklists where Cerulean Warbler was observed
  #     get_spp_obs('Cerulean Warbler', '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')

  query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${species}"}')
  results <- get_ebd_data(query, filter) %>%
    filter(COMMON_NAME == species) #remove other obervations from the checklist

  return(results)
}

# Get Species List
get_spp_list <- function(query="{}",filter="{}"){

  mongodata <- m_spp$find(query, filter)

  return(mongodata)
}

species_list = get_spp_list(filter='{"PRIMARY_COM_NAME":1}')$PRIMARY_COM_NAME



#####################################################################################
# Block level summaries

# block_data <- read.csv("input_data/blocks.csv") %>% filter(COUNTY == "WAKE")
block_data <- get_block_data()
# priority_block_geojson <- readLines("input_data/blocks_priority.geojson")
# priority_block_data <- block_data

priority_block_data <- filter(block_data, PRIORITY == 1)[c("ID_NCBA_BLOCK", "ID_BLOCK_CODE", "NW_X", "NW_Y", "SE_X", "SE_Y", "PRIORITY", "COUNTY", "REGION")]

# priority_block_data <- block_data %>%
  # filter(PRIORITY == 1) %>%
  # select(ID_NCBA_BLOCK, ID_BLOCK_CODE, NW_X, NW_Y, SE_X, SE_Y, PRIORITY, COUNTY, REGION)

print("filtering block records")
# print(head(priority_block_data))

# priority_bock_geojson$style = list(
#   weight = 2,
#   color = ncba_blue,
#   fillOpacity = 0
# )



block_hours_month <- read.csv("input_data/block_month_year_hours.csv")
block_hours_total <- read.csv("input_data/block_total_hours.csv")


get_block_hours <- function(id_ncba_block) {
  # place holder for function to summarize hours in blocks
  #
  # Description:
  #   Returns datafram of requested observations from the EBD collection
  # Arguments:
  # species -- Common name of the species data to be retrieved
  # fitler -- JSON formatted "project" parameter in MongoDB format
  #
  # Examples:
  #   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns from checklists where Cerulean Warbler was observed
  #     get_spp_obs('Cerulean Warbler', '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')
  print(id_ncba_block)
  if (length(id_ncba_block) >0){
    result <- filter(block_hours_month, ID_NCBA_BLOCK == id_ncba_block)
  }
  if (length(result)>0){
    return(result)

  }
}
