# Testing parsing nested JSON for block summary data
# see block_summary_abbotsburg.json for example data

if (!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
if (!require(jsonlite)) install.packages(
  "jsonlite", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
if (!require(tidyr)) install.packages(
  "tidyr", repos = "http://cran.us.r-project.org")

transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  return(t_df)
}

bs <- jsonlite::read_json("block_summary_shallotte.json")
print
summary <- tibble(block = bs)

breeding_stats <- summary %>%
    unnest_wider(block) %>%
    unnest_wider(breeding) %>%
    select(
        sppCountConfirmed,
        sppCountProbable,
        sppCountPossible,
        sppCountDetected,
        hrsDiurnal,
        hrsNocturnal,
        sppPctConfirmed,
        obsCount,
        checkCount,
        bConfirmedSpp,
        bDiurnalHrs,
        bNocturnalHrs,
        sppCountGapMissing,
        sppCountGapPredicted
        )


  # transpose_df(breeding_stats)
print(breeding_stats)


breeding_missing <- summary %>%
    unnest_wider(block) %>%
    unnest_wider(breeding) %>%
    select(sppGapMissing) %>%
    unnest_longer(sppGapMissing)

nonbreeding_missing <- summary %>%
    unnest_wider(block) %>%
    unnest_wider(nonbreeding) %>%
    select(sppGapMissing) %>%
    unnest_longer(sppGapMissing)
