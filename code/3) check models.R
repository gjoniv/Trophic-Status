library(tidyverse)
library(brms)
library(tidybayes)


lake_level_brm = readRDS(file = "models/lake_level_brm.rds")
trophic_guild_and_state_brm = readRDS(file = "models/trophic_guild_and_state_brm.rds")
trophic_state_brm = readRDS(file = "models/trophic_state_brm.rds")


# posterior predictive checks
pp_check(lake_level_brm)
pp_check(trophic_guild_and_state_brm)
pp_check(trophic_state_brm)
