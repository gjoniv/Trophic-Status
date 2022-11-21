library(tidyverse)
library(brms)
library(tidybayes)

# load models
lake_level_brm = readRDS(file = "models/lake_level_brm.rds")
trophic_state_brm = readRDS(file = "models/trophic_state_brm.rds")
trophic_guild_and_state_brm = readRDS(file = "models/trophic_guild_and_state_brm.rds")

## extract slopes
# lake level

lake_level_slopes = tibble(log10_zoo_mean_biomass_c = c(0, 0.0001)) %>% 
  add_epred_draws(lake_level_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = log10_zoo_mean_biomass_c, values_from = .epred) %>% 
  mutate(slope = 1e4*(`1e-04` - `0`)) %>%
  # median_qi(slope) %>% 
  mutate(model = "log_a ~ log_m")

# lake trophic state
trophic_slopes = tibble(log10_zoo_mean_biomass_c = c(0, 0.0001)) %>% 
  expand_grid(trophic_state_brm$data %>% distinct(trophic_state)) %>% 
  add_epred_draws(trophic_state_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = log10_zoo_mean_biomass_c, values_from = .epred) %>% 
  mutate(slope = 1e4*(`1e-04` - `0`)) %>% 
  group_by(trophic_state) %>% 
  # median_qi(slope) %>% 
  mutate(model = "log_a ~ log_m*trophic")

# feeding group
feeding_group_slopes = tibble(log10_zoo_mean_biomass_c = c(0, 0.0001)) %>% 
  expand_grid(trophic_guild_and_state_brm$data %>% distinct(feeding_group)) %>%
  mutate(trophic_state = NA) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  group_by(feeding_group, log10_zoo_mean_biomass_c, .draw) %>% 
  summarize(.epred = mean(.epred)) %>% 
  pivot_wider(names_from = log10_zoo_mean_biomass_c, values_from = .epred) %>% 
  mutate(slope = 1e4*(`1e-04` - `0`)) %>% 
  group_by(feeding_group) %>% 
  # median_qi(slope) %>% 
  mutate(model = "log_a ~ log_m*trophic*feeding",
         trophic_state = "averaged over")


# feeding group and trophic interaction
feeding_trophic_slopes = tibble(log10_zoo_mean_biomass_c = c(0, 0.0001)) %>% 
  expand_grid(trophic_guild_and_state_brm$data %>% distinct(trophic_state, feeding_group)) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = log10_zoo_mean_biomass_c, values_from = .epred) %>% 
  mutate(slope = 1e4*(`1e-04` - `0`)) %>% 
  group_by(trophic_state, feeding_group) %>% 
  # median_qi(slope) %>% 
  mutate(model = "log_a ~ log_m*trophic*feeding")


# combined
all_slopes = bind_rows(lake_level_slopes,
                       trophic_slopes,
                       feeding_group_slopes,
                       feeding_trophic_slopes) %>% 
  mutate(id = paste0(trophic_state, feeding_group, model),
         id_no = as.factor(as.integer(as.factor(id))))


all_slopes_summary = all_slopes %>% 
  group_by(model, trophic_state, feeding_group) %>% 
  median_qi(slope)

write_csv(all_slopes_summary, file = "tables/all_slopes_summary.csv")
