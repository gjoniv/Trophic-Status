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

fig2 = lake_level_conditional_posts %>% 
  group_by(log10_zoo_mean_biomass_c) %>% 
  median_qi() %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_state_only, aes(y = log10_zoo_total_density_c),
             shape = 21, size = 0.1) +
  labs(y = "log10 abundance (centered)",
       x = "log10 body size (centered)") +
  geom_abline(slope = -0.75, intercept = 0, linetype = "dashed") +
  theme_default() +
  annotate(geom = "text", label = "observed", x = 2, y = -0.6, size = 2) +
  annotate(geom = "text", label = "predicted (-0.75)", x = 1.8, y = -1.85, size = 2)


ggsave(fig2, file = "plots/fig2.jpg", dpi = 500, width = 3, height = 3)


# plot trophic state only 

trophic_reference_lines = datzoo_trophic_state_only %>% distinct(trophic_state) %>% 
  mutate(log10_zoo_mean_biomass_c = 0) %>% 
  add_epred_draws(trophic_state_brm) %>% 
  group_by(trophic_state) %>% 
  summarize(intercept = median(.epred)) %>% 
  mutate(slope = -0.75) %>% 
  mutate(trophic_state = case_when(trophic_state == "Hypereutrophic" ~ "a) Hypereutrophic",
                                   trophic_state == "Eutrophic" ~ "b) Eutrophic",
                                   trophic_state == "Mesotrophic" ~ "c) Mesotrophic",
                                   TRUE ~ "d) Oligotrophic"))

fig3 = trophic_conditional_posts %>% 
  group_by(trophic_state, log10_zoo_mean_biomass_c) %>% 
  median_qi(.epred) %>% 
  mutate(trophic_state = case_when(trophic_state == "Hypereutrophic" ~ "a) Hypereutrophic",
                                   trophic_state == "Eutrophic" ~ "b) Eutrophic",
                                   trophic_state == "Mesotrophic" ~ "c) Mesotrophic",
                                   TRUE ~ "d) Oligotrophic")) %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred)) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_state_only %>% 
               mutate(trophic_state = case_when(trophic_state == "Hypereutrophic" ~ "a) Hypereutrophic",
                                                trophic_state == "Eutrophic" ~ "b) Eutrophic",
                                                trophic_state == "Mesotrophic" ~ "c) Mesotrophic",
                                                TRUE ~ "d) Oligotrophic")), aes(y = log10_zoo_total_density_c),
             size = 0.1) +
  labs(y = "log10 abundance (centered)",
       x = "log10 body size (centered)") +
  geom_abline(data = trophic_reference_lines, aes(slope = slope, intercept = intercept), linetype = "dashed")  +
  facet_wrap(~trophic_state, nrow = 1) +
  theme_default()

ggsave(fig3, file = "plots/fig3.jpg", width = 7, height = 2.2, dpi = 600)


# plot feeding group only 

feeding_group_medians = trophic_feeding_conditional_posts %>% 
  group_by(.draw, feeding_group, log10_zoo_mean_biomass_c) %>% 
  summarize(.epred = mean(.epred)) %>% 
  group_by(feeding_group, log10_zoo_mean_biomass_c) %>% 
  median_qi(.epred) %>% 
  mutate(feeding_group = case_when(feeding_group == "OMNI" ~ "a) Omnivores",
                                   feeding_group == "HERB" ~ "b) Herbivores",
                                   TRUE ~ "c) Predators"))

feeding_reference_lines = datzoo_trophic_guild_and_state %>% distinct(feeding_group, trophic_state) %>% 
  mutate(log10_zoo_mean_biomass_c = 0) %>% 
  add_epred_draws(trophic_guild_and_state_brm) %>% 
  group_by(feeding_group, .draw) %>% 
  summarize(.epred = mean(.epred)) %>% 
  group_by(feeding_group) %>% 
  summarize(intercept = median(.epred)) %>% 
  mutate(slope = -0.75) %>% 
  mutate(feeding_group = case_when(feeding_group == "OMNI" ~ "a) Omnivores",
                                   feeding_group == "HERB" ~ "b) Herbivores",
                                   TRUE ~ "c) Predators"))

fig4 = feeding_group_medians %>% 
  ggplot(aes(x = log10_zoo_mean_biomass_c, y = .epred)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = datzoo_trophic_guild_and_state%>% 
               mutate(feeding_group = case_when(feeding_group == "OMNI" ~ "a) Omnivores",
                                                feeding_group == "HERB" ~ "b) Herbivores",
                                                TRUE ~ "c) Predators")), aes(y = log10_zoo_total_density_c),
             size = 0.05) +
  labs(y = "log10 abundance (centered)",
       x = "log10 body size (centered)") +
  geom_abline(data = feeding_reference_lines, aes(slope = slope, intercept = intercept), linetype = "dashed") +
  facet_grid(~feeding_group) +
  theme_default()
  

ggsave(fig4, file = "plots/fig4.jpg", dpi = 600, width = 7, height = 2.5)


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
