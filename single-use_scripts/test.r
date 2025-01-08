library("here")

source(here("resources", "ncba_functions.R"))

obs <- get_observations(species = "Northern Bobwhite")
head(obs)

breeding_boxplot(species = "Northern Bobwhite")
