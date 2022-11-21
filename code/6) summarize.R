library(tidyverse)
library(brms)
library(tidybayes)

# load models
lake_level_brm = readRDS(file = "models/lake_level_brm.rds")
trophic_state_brm = readRDS(file = "models/trophic_state_brm.rds")
trophic_guild_and_state_brm = readRDS(file = "models/trophic_guild_and_state_brm.rds")

# extract marginal slopes -------------------------------------------------

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
         trophic_state = "average")


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
         id_no = as.factor(as.integer(as.factor(id))),
         parameter = "slope")


# extract marginal intercepts ---------------------------------------------
# lake level
lake_level_intercept = tibble(log10_zoo_mean_biomass_c = c(0)) %>% 
  add_epred_draws(lake_level_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>%
  mutate(model = "log_a ~ log_m")

# lake trophic state
trophic_intercept = tibble(log10_zoo_mean_biomass_c = c(0)) %>% 
  expand_grid(trophic_state_brm$data %>% distinct(trophic_state)) %>% 
  add_epred_draws(trophic_state_brm) %>% 
  ungroup() %>% 
  mutate(model = "log_a ~ log_m*trophic")

# feeding group
feeding_group_intercept = tibble(log10_zoo_mean_biomass_c = c(0)) %>% 
  expand_grid(trophic_guild_and_state_brm$data %>% distinct(feeding_group)) %>%
  mutate(trophic_state = NA) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  mutate(model = "log_a ~ log_m*trophic*feeding",
         trophic_state = "average")


# feeding group and trophic interaction
feeding_trophic_intercept = tibble(log10_zoo_mean_biomass_c = c(0)) %>% 
  expand_grid(trophic_guild_and_state_brm$data %>% distinct(trophic_state, feeding_group)) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  mutate(model = "log_a ~ log_m*trophic*feeding")


# combined
all_intercepts = bind_rows(lake_level_intercept,
                       trophic_intercept,
                       feeding_group_intercept,
                       feeding_trophic_intercept) %>% 
  mutate(id = paste0(trophic_state, feeding_group, model),
         id_no = as.factor(as.integer(as.factor(id))),
         parameter = "intercept") 


# make table --------------------------------------------------------------

slope_table = all_slopes %>% 
  group_by(model, feeding_group, trophic_state) %>% 
  median_qi(slope) %>% 
  select(-.width, -.point, -.interval) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(slope = paste0(slope, " (", .lower, " to ", .upper, ")")) %>% 
  select(-.lower, -.upper)


intercept_table = all_intercepts %>% 
  group_by(model, feeding_group, trophic_state) %>% 
  median_qi(.epred) %>% 
  select(-.width, -.point, -.interval) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(intercept = paste0(.epred, " (", .lower, " to ", .upper, ")")) %>% 
  select(-.lower, -.upper, -.epred)

slope_and_intercept_table = left_join(slope_table, intercept_table) %>% 
  mutate(group = case_when(trophic_state == "average" ~ "main",
                           is.na(trophic_state) ~ "main",
                           TRUE ~ "interaction")) %>% 
  mutate(trophic_state = fct_relevel(trophic_state, "Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic"),
         feeding_group = fct_relevel(feeding_group, "OMNI", "HERB", "PRED")) %>% 
  arrange(desc(group), model, feeding_group, trophic_state)
  

write_csv(slope_and_intercept_table, file = "tables/slope_and_intercept_table.csv")
