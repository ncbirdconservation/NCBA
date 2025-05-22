library("DT")
library("dplyr")

# Eastern Towhee	S226281850	35.9012918	-80.4017382
# Eastern Towhee	S226266803	35.913145	-80.4108275

block_s7_table <- data.frame(
  COMMON_NAME = c("Eastern Towhee", "Eastern Towhee"),
  CHECKLIST = c("S226281850", "S226266803"),
  LATITUDE = c(35.9012918,35.913145),
  LONGITUDE = c(-80.4017382,-80.4108275)
)

table_out <- block_s7_table %>%
  mutate(
    # CHECKLIST = paste0(
    #   '<a href="https://www.ebird.org/checklist/',
    #   CHECKLIST,
    #   '" target="_blank">',
    #   CHECKLIST, '</a>'
    # ),
    # COORDS = paste0(
    #   '<a href="https://www.google.com/maps/@',
    #   LATITUDE,',',LONGITUDE,
    #   '" target="_blank">',LATITUDE,', ',LONGITUDE,
    #   '</a>'
    # ),
    CHECKLIST_LINK = paste0(
      "https://www.ebird.org/checklist/",CHECKLIST
    ),
    LATITUDE = NULL,
    LONGITUDE = NULL
  )

# print(block_s7_table)
print(table_out)






# if(!require(googlesheets4)) install.packages(
#   "googlesheets4", repos = "http://cran.us.r-project.org")

# gs4_auth(cache = ".secrets", email = "ncbirdatlas@gmail.com")

# sheet_url <- "https://docs.google.com/spreadsheets/d/1NVBSHU5cOTCHmW067cPD8NQne60g-P3cAwY04RYPiWw/edit?usp=sharing"

# bn_data <- read_sheet(sheet_url)
# block <- "BEAR_CREEK-SE"

# test <- bn_data[
#     bn_data$ID_NCBA_BLOCK == block & bn_data$ACCESS == "Public",
#     c("SEASON", "PRIORITY", "CRITERIA", "DESCRIPTION")
#     ]

# datatable(
#   test,
#   list(
#     paging = FALSE,
#     searching = FALSE,
#     rownames = FALSE,
#     selection = "none"
#     )
# )