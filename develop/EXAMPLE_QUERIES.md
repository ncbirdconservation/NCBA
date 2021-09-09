# Example Queries for the MongoDB Atlas implementation
General help for Mongo queries is available here: https://docs.mongodb.com/manual/tutorial/query-documents/
Help for Mongolite R library: https://jeroen.github.io/mongolite/


### Get all Brown Creeper Records
{"OBSERVATIONS.COMMON_NAME":"Brown Creeper"}}

Note that adding additional criteria are by default "AND" relationships

### Retrieve all 2021 records for Brown Creeper
{"YEAR":2021, "OBSERVATIONS.COMMON_NAME":"Brown Creeper"}}

### Retrieve Brown Creeper Records, and only return a few fields
query = '{"OBSERVATIONS.COMMON_NAME":"Brown Creeper"}}'
filter = '{"SAMPLING_EVENT_IDENTIFIER":1, "OBSERVATION_DATE": 1, "NCBA_BLOCK":1, "DURATION_MINUTES": 1, "PROJECT_CODE":1, "LATITUDE":1, "LONGITUDE":1, "OBSERVATIONS.&":1}'
