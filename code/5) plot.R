library(tidyverse)
library(brms)
library(tidybayes)


# get posts and data
lake_level_conditional_posts = readRDS("posteriors/lake_level_conditional_posts.rds")
trophic_feeding_conditional_posts = readRDS("posteriors/trophic_feeding_conditional_posts.rds")
trophic_guild_and_state_brm = readRDS(file = "models/trophic_guild_and_state_brm.rds")
trophic_conditional_posts = readRDS(file = "posteriors/trophic_conditional_posts.rds")
trophic_state_brm = readRDS(file = "models/trophic_state_brm.rds")

# load data
datzoo_trophic_state_only = readRDS(file = "data/datzoo_trophic_state_only.rds")
datzoo_trophic_guild_and_state = readRDS(file = "data/datzoo_trophic_guild_and_state.rds")


# plot lake level

lake_level_conditional_posts %>% 
  group_by(log10_zoo_mean_biomass_c) %>% 
  median_qi() %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_state_only, aes(y = log10_zoo_total_density_c)) +
  labs(y = "log10 Zooplankton Density",
       x = "log10 Zooplankton Biomass") +
  geom_abline(slope = -0.75, intercept = 0)


# plot trophic state only 

trophic_reference_lines = datzoo_trophic_state_only %>% distinct(trophic_state) %>% 
  mutate(log10_zoo_mean_biomass_c = 0) %>% 
  add_epred_draws(trophic_state_brm) %>% 
  group_by(trophic_state) %>% 
  summarize(intercept = median(.epred)) %>% 
  mutate(slope = -0.75)

trophic_conditional_posts %>% 
  group_by(trophic_state, log10_zoo_mean_biomass_c) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred)) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_state_only, aes(y = log10_zoo_total_density_c)) +
  labs(y = "log10 Zooplankton Density",
       x = "log10 Zooplankton Biomass") +
  geom_abline(data = trophic_reference_lines, aes(slope = slope, intercept = intercept))  +
  facet_wrap(~trophic_state)


# plot feeding group only 

feeding_group_medians = trophic_feeding_conditional_posts %>% 
  group_by(.draw, feeding_group, log10_zoo_mean_biomass_c) %>% 
  summarize(.epred = mean(.epred)) %>% 
  group_by(feeding_group, log10_zoo_mean_biomass_c) %>% 
  median_qi(.epred)

feeding_reference_lines = datzoo_trophic_guild_and_state %>% distinct(feeding_group, trophic_state) %>% 
  mutate(log10_zoo_mean_biomass_c = 0) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  group_by(feeding_group, .draw) %>% 
  summarize(.epred = mean(.epred)) %>% 
  group_by(feeding_group) %>% 
  summarize(intercept = median(.epred)) %>% 
  mutate(slope = -0.75)

feeding_group_medians %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred, fill = feeding_group, color = feeding_group)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_guild_and_state, aes(y = log10_zoo_total_density_c)) +
  labs(y = "log10 Zooplankton Density",
       x = "log10 Zooplankton Biomass") +
  geom_abline(data = feeding_reference_lines, aes(slope = slope, intercept = intercept)) +
  facet_grid(~feeding_group)

  


# plot feedig group by trophic state
trophic_medians = trophic_feeding_conditional_posts %>% 
  group_by(log10_zoo_mean_biomass_c, feeding_group, trophic_state) %>% 
  median_qi(.epred)

feeding_trophic_reference_lines = datzoo_trophic_guild_and_state %>% distinct(feeding_group, trophic_state) %>% 
  mutate(log10_zoo_mean_biomass_c = 0) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  group_by(feeding_group, trophic_state) %>% 
  summarize(intercept = median(.epred)) %>% 
  mutate(slope = -0.75)
  
trophic_medians %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred, fill = feeding_group, color = feeding_group)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_guild_and_state, aes(y = log10_zoo_total_density_c)) +
  labs(y = "log10 Zooplankton Density",
       x = "log10 Zooplankton Biomass") +
  geom_abline(data = feeding_trophic_reference_lines, aes(slope = slope, intercept = intercept)) +
  facet_grid(trophic_state~feeding_group)
