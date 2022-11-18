library(tidyverse)
library(brms)
library(tidybayes)

# load models
lake_level_brm = readRDS(file = "models/lake_level_brm.rds")
trophic_guild_and_state_brm = readRDS(file = "models/trophic_guild_and_state_brm.rds")

# load data
datzoo_trophic_state_only = readRDS(file = "data/datzoo_trophic_state_only.rds")
datzoo_trophic_guild_and_state = readRDS(file = "data/datzoo_trophic_guild_and_state.rds")


# make data grid to simulate over

# lake level brm
biomass_sims_lake_level = tibble(log10_zoo_mean_biomass_c = seq(min(lake_level_brm$data$log10_zoo_mean_biomass_c),
                                                                max(lake_level_brm$data$log10_zoo_mean_biomass_c),
                   length.out = 20))


# trophic guild and state brm
biomass_sims_trophic = tibble(log10_zoo_mean_biomass_c = seq(min(trophic_guild_and_state_brm$data$log10_zoo_mean_biomass_c),
                           max(trophic_guild_and_state_brm$data$log10_zoo_mean_biomass_c),
                           length.out = 20))

trophic_state_sims = unique(trophic_guild_and_state_brm$data$trophic_state)
feeding_group_sims = unique(trophic_guild_and_state_brm$data$feeding_group)

biomass_and_trophic_sims = biomass_sims_trophic %>% 
  expand_grid(trophic_state = trophic_state_sims) %>% 
  expand_grid(feeding_group = feeding_group_sims)


# sample posteriors and conditional posteriors

lake_level_posts = as_draws_df(lake_level_brm) %>% saveRDS("posteriors/lake_level_posts.rds")

lake_level_conditional_posts = biomass_sims_lake_level %>% 
  add_epred_draws(lake_level_brm) %>% 
  saveRDS("posteriors/lake_level_conditional_posts.rds")

trophic_feeding_posts = as_draws_df(trophic_guild_and_state_brm) %>% saveRDS("posteriors/trophic_feeding_posts.rds")

trophic_feeding_conditional_posts = biomass_and_trophic_sims %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  saveRDS("posteriors/trophic_feeding_conditional_posts.rds")
