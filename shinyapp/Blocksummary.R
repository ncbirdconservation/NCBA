### Obtaining Block Summary Collection
### 
### # Login info for MongoDB instance

USER = "ncba_ruser"
PASS = "Sternacaspia"

library(mongolite)

# POTENTIALLY CHANGE TO NEW MONGO DATA API IN THE FUTURE - BENEFITS?

# curl --location --request POST 'https://data.mongodb-api.com/app/data-blvuy/endpoint/data/beta/action/findOne' \
# --header 'Content-Type: application/json' \
# --header 'Access-Control-Request-Headers: *' \
# --header 'api-key: 61ae20ce84cf5f477d2d8394' \
# --data-raw '{
#     "collection":"safe_dates",
#     "database":"ebd_mgmt",
#     "dataSource":"Cluster0",
#     "projection": {"_id": 1}
# }'
# MongoDB
# this is a read only account
HOST = "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
DB = "ebd_mgmt"
COLLECTION = "ebd"

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

m_blocksum <- mongo(
  "BLOCK_SUMMARIES",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

alldata <- m_blocksum$find('{}')

str(alldata)
