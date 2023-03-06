# utility function to use throughout

#############################################################################
# MongoDB
# this is a read only account
HOST = "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
DB = "ebd_mgmt"
COLLECTION = "ebd"
source("ncba_config.r")
# other relevant collections include: blocks and ebd_taxonomy

URI = sprintf(
  paste0("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=",
    "atlas-3olgg1-shard-0&readPreference=primary&ssl=true"),
  USER,
  PASS,
  HOST,
  DB)

# connect to a specific collection (table)
m <- mongo(
  COLLECTION,
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_spp <- mongo(
  "ebd_taxonomy",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_blocks <- mongo(
  "blocks",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_sd <- mongo(
  "safe_dates",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_blocksum <- mongo(
  "BLOCK_SUMMARIES",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

get_safe_dates <- function(){
  sd <- m_sd$find("{}","{}")

  #ADD JULIAN DATE COLUMNS
  sd$B_SAFE_START_JULIAN <- apply(
    sd['B_SAFE_START_DATE'],1,function(x){yday(x[1])})
  
  sd$B_SAFE_END_JULIAN <- apply(
    sd['B_SAFE_END_DATE'],1,function(x){yday(x[1])})

  return(
    sd[
      c(
        'TAX_NO',
        'COMMON_NAME',
        'B_SAFE_START_JULIAN',
        'B_SAFE_END_JULIAN'
      )
    ]
  )
}

safe_dates <- get_safe_dates()

# this query follows JSON based query syntax
#   (see here for the basics:
#   https://jeroen.github.io/mongolite/query-data.html#query-syntax)

# TESTING INFO
# low checklist block -> "PAMLICO_BEACH-CW" or "GRIMESLAND-NW"
# this works:
#   get_mongo_data(
#     '{"ID_NCBA_BLOCK":"GRIMESLAND-CW"}',
#     '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}',
#     FALSE)
#   get_mongo_data(
#     '{"OBSERVATIONS.COMMON_NAME":"Cerulean Warbler"}',
#     '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1',
#     '"OBSERVATIONS.COMMON_NAME":1, "OBSERVATIONS.OBSERVATION_COUNT":1,'
#     '"OBSERVATIONS.BEHAVIOR_CODE":1, "OBSERVATIONS.BREEDING_CATEGORY":1}')

aggregate_ebd_data <- function (pipeline) {
  # Perform aggregation on ebd collection in MongoDB Atlas implementation
  #
  # Description:
  #   Returns records resulting from the passed aggregation pipeline
  #
  # Arguments:
  # pipeline -- valid JSON formatted aggregation pipeline

  mongodata <- m$aggregate(pipeline)
  return(mongodata)
}

get_ebd_data <- function(query="{}", filter="{}", sd=safe_dates){
# Retrieves data from MongoDB Atlas implementation
#
# Description:
#   Returns a dataframe of records from the NC Bird Atlas MongoDB
#     implementation. If OBSERVATION fields are included in the requested
#     output, flattens the dataframe. If a species is specificed, all
#     observations from the checklist are returned.
#
# Arguments:
# query -- JSON formatted MongoDB query
# fitler -- JSON formatted "project" parameter in MongoDB format
#
# Examples:
#   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns
#       from checklists in the GRIMESLAND-CW block
#       get_ebd_data('{"ID_NCBA_BLOCK":"GRIMESLAND-CW"}',
#         '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')
#   2. Retrieve OBSERVATION_DATE, SAMPLING_EVENT_IDENTIFIER, OBSERVATIONS.
#       COMMON_NAME, OBSERVATIONS.OBSERVATION_COUNT, OBSERVATIONS.
#       BEHAVIOR_CODE, OBSERVATIONS.BREEDING_CATEGORY
#       for all Cerulean Warbler detections.
#       get_ebd_data('{"OBSERVATIONS.COMMON_NAME":"Cerulean Warbler"}',
#         '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1,',
#         '"OBSERVATIONS.COMMON_NAME":1, "OBSERVATIONS.OBSERVATION_COUNT":1',
#         '"OBSERVATIONS.BEHAVIOR_CODE":1, ',
#         '"OBSERVATIONS.BREEDING_CATEGORY":1}')

#specify the default  fields to return if no filter passed
  # do not run if no query passed
  if (query != "{}"){
    # don't pass blank queries!
    sortquery <- paste0(
      '{"OBSERVATION_DATE":1, ',
      '"TIME_OBSERVATIONS_STARTED":1, ',
      '"SAMPLING_EVENT_IDENTIFIER":1}')
    if (grepl("OBSERVATIONS", filter, fixed=TRUE) | filter=="{}"){
      # WORKING VERSION - downloads and returns all checklist obs
      if (filter == "{}") {
        # DEFINE DEFAULT FILTER that excludes unused fields
        filter <- paste0(
          '{"ALL_SPECIES_REPORTED":1,"ATLAS_BLOCK":1,"BCR_CODE":1,',
          '"COUNTRY":1,"COUNTRY_CODE":1,"COUNTY":1,"COUNTY_CODE":1,',
          '"DURATION_MINUTES":1,"EFFORT_AREA_HA":1,"EFFORT_DISTANCE_KM":1,',
          '"GROUP_IDENTIFIER":1,"IBA_CODE":1,"ID_BLOCK_CODE":1,',
          '"ID_NCBA_BLOCK":1,"LAST_EDITED_DATE":1,"LATITUDE":1,',
          '"LOCALITY":1,"LOCALITY_ID":1,"LOCALITY_TYPE":1,"LONGITUDE":1,',
          '"MONTH":1,"NUMBER_OBSERVERS":1,"OBSERVATIONS":1,',
          '"OBSERVATION_DATE":1,"OBSERVER_ID":1,"PRIORITY_BLOCK":1,',
          '"PROJECT_CODE":1,"PROTOCOL_CODE":1,"PROTOCOL_TYPE":1,',
          '"SAMPLING_EVENT_IDENTIFIER":1,"STATE":1,"STATE_CODE":1,',
          '"TIME_OBSERVATIONS_STARTED":1,"TRIP_COMMENTS":1,',
          '"USFWS_CODE":1,"YEAR":1, "EBD_NOCTURNAL":1}')

        # fields excluded
        # "GEOM.coordinates":1,"GEOM.type":1,"NCBA_APPROVED":1,"NCBA_BLOCK":1,
        # "NCBA_COMMENTS":1,"NCBA_REVIEWED":1,"NCBA_REVIEWER":1,
        # "NCBA_REVIEW_DATE":1,
      }

      print("getting Observations from AtlasCache")
      mongodata <- m$find(query, filter)
      # mongodata <- m$find(
      #   query,
      #   filter,
      #   sort=sortquery) #sorting breaks for big queries

      if (nrow(mongodata)>0) {
        print("unnesting observation records")
        mongodata <- unnest(mongodata, cols = (c(OBSERVATIONS)))

        #ADD SEASON COLUMN FROM SAFE DATES TABLE AND POPULATE
        gen_breeding_start = yday("2021-04-01")
        gen_breeding_end = yday("2021-08-31")

        print("adding Season (Breeding = April 1 - Aug 31)")
        mongodata$SEASON <- apply(
          mongodata[c('OBSERVATION_DATE','COMMON_NAME')],1,
          function(x) {
            odj = yday(x[1]) #Convert observation_date to julian day
            #lookup spp safe dates (if any)
            # spp_s_d = sd[sd$COMMON_NAME == x[2],]
            # 
            # if (nrow(spp_s_d) == 0 ) {
            #   begin = gen_breeding_start
            #   end = gen_breeding_end
            # } else {
            #   begin = spp_s_d['B_SAFE_START_JULIAN']
            #   end = spp_s_d['B_SAFE_END_JULIAN']
            # }

            if ( gen_breeding_start <= odj & odj <= gen_breeding_end){
              season = "Breeding"
            } else {
              if(yday("2021-08-31") <= odj & odj <=yday("2021-10-31") | yday("2021-03-01") <= odj & odj <=yday("2021-03-31")){ 
                      season = "Migration"
              } else {
              season = "Non-Breeding"
              }
            }
            
            return(season)

          })
        print("Season (Breeding or Winter)")
      } # Expand observations if records returned


      # EXAMPLE/TESTING
      print("AtlasCache records retrieved")
      # print(head(mongodata))
      # USE aggregation pipeline syntax to return only needed observations
      # pipeline <- str_interp(
        # '[{$match: ${query}}, {$project:${filter}},',
        # '{$unwind: {path: "$OBSERVATIONS"}}]')
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
  filter <- paste0(
    '{"_id": 1, "COUNTY": 1, "GEOM": 1, "ID_BLOCK": 1, "ID_BLOCK_CODE": 1,',
    ' "ID_EBD_NAME": 1, "ID_NCBA_BLOCK": 1, "ID_OLD_ID": 1, "NW_X": 1, ',
    '"NW_Y": 1, "PRIORITY": 1, "QUADID": 1, "QUAD_BLOCK": 1, "QUAD_NAME": 1, ',
    '"REGION": 1, "SE_X": 1, "SE_Y": 1, "SUBNAT2": 1, "TYPE": 1, ',
    '"ID_S123_NOSPACES_TEMP": 1, "ID_S123_SPACES_TEMP": 1}')

  # blockdata <- m_blocks$find("{}","{}")
  blockdata <- m_blocks$find("{}",filter)
  return(blockdata)
}

get_block_gap_spp <- function(blockid){
# Retrieves list of species for the passed block id
  filter <- '{"GAP_SPP":1}'
  query <- '{"_id":"${blockid}"}'
  blockdata <- m_blocks$find("{}",filter)

# add code here to unnest data (like observations above)

  return(blockdata)

}

# DEPRECATED
# get_block_checklists <- function(block = "", portal = FALSE) (
#   # Retrieves data from MongoDB Atlas implementation
#   #
#   # Description:
#   #   Returns a dataframe of records from the NC Bird Atlas MongoDB 
#       implementation in a format to plot on the map.
#   #
#   # Arguments:
#   # block -- string that corresponds to the ID_NCBA_BLOCK field
#   #
#   # Examples:
#   #   1. Retrieve checklists submitted to the portal
#           from the GRIMESLAND-CW block
#   #     get_block_checklists('GRIMESLAND-CW', TRUE)
#
#   if (block != ""){
#     if (portal) {
#       query <- 
#           str_interp(
  #           '{"ID_NCBA_BLOCK":"${block}", "PROJECT_CODE":"EBIRD_ATL_NC"}')
#     } else {
#       query <- str_interp('{"ID_NCBA_BLOCK":"${block}"}')
#     }
#     print(query)
#
#     filter <- str_interp('{"LATITUDE":1, "LONGITUDE":1, ',
#       '"SAMPLING_EVENT_IDENTIFIER":1, "LOCALITY_ID":1, ',
#       '"OBSERVATION_DATE":1}'
#     print(filter)
#
#     return(get_ebd_data(query, filter))
#   }
#
# )
#############################################################################
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
  #   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns from
  #       checklists where Cerulean Warbler was observed
  #     get_spp_obs(
  #       'Cerulean Warbler',
  #        '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')

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



###############################################################################
# Block level summaries

# block_data <- read.csv("input_data/blocks.csv") %>% filter(COUNTY == "WAKE")
block_data <- get_block_data()
# priority_block_geojson <- readLines("input_data/blocks_priority.geojson")
# priority_block_data <- block_data

priority_block_data <- filter(
  block_data, PRIORITY == 1)[c(
    "ID_NCBA_BLOCK",
    "ID_BLOCK_CODE",
    "NW_X",
    "NW_Y",
    "SE_X",
    "SE_Y",
    "PRIORITY",
    "COUNTY",
    "REGION")]

print("filtering block records")

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
  #   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns
  #       from checklists where Cerulean Warbler was observed

  print(id_ncba_block)
  if (length(id_ncba_block) >0){
    result <- filter(block_hours_month, ID_NCBA_BLOCK == id_ncba_block)
  }
  if (length(result)>0){
    return(result)

  }
}

### Retrieving Mongo Block Summary Table

blocksum <- m_blocksum$find(
  fields = '{ "ID_NCBA_BLOCK": true, "county": true, "region": true, "breeding.hrsDiurnal": true, "breeding.hrsNocturnal": true,
              "wintering.hrsDiurnal": true, "wintering.hrsNocturnal": true, 
            "breeding.sppCountConfirmed": true, "breeding.sppCountProbable": true, "breeding.sppCountPossible": true, "breeding.sppCountDetected": true }',)

blocksum <-  tibble(blocksum)

blocksum <- blocksum %>% 
  unnest_wider("breeding", names_sep = "_") %>% 
  unnest_wider("wintering", names_sep = "_") 


blocksum <- as.data.frame(blocksum)