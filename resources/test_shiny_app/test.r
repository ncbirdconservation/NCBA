library(tidyverse)
dt1 <- "2024-01-01"
dt2 <- "2024-02-01"

df <- data.frame(c(
  "2024-01-01",
  "2023-01-01",
  "2024-02-14",
  "2021-02-14"
))
colnames(df) <- c("observation_date")

# df$observation_date <- as.POSIXlt(df$observation_date, format = "$Y-%m-%d")

df <- df %>%
  mutate(obsdate = as.POSIXlt(observation_date, format = "%Y-%m-%d")) %>%
  mutate(obsdate = obsdate$yday + 1)
print(df)

tmp <- as.POSIXlt(dt2, format = "%Y-%m-%d")
print(tmp)

print(tmp$yday)
