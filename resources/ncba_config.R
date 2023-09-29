# Login info for MongoDB instance

USER = "ncba_ruser"
PASS = "Peucaeaaestivalis"

ncba_db_user = "ncba_ruser"
ncba_db_pass = "Peucaeaaestivalis"

# Working directory
work_dir <- "~/Documents/NCBA/Analyses/"
# Code repo 
code_dir <- "C:\Users\skanderson\OneDrive - State of North Carolina\@@ncba\NCBA"
# EBird Basic Dataset Copy 
EBD_observations <- "/Volumes/NMTARR1/Datasets/EBD/ebd_US-NC_202101_202312_smp_relJul-2023.txt"
# EBird Sampling Event Dataset path 
EBD_sampling <- "/Volumes/NMTARR1/Datasets/EBD/ebd_US-NC_202101_202312_smp_relJul-2023_sampling.txt"



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
