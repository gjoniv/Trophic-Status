library(tidyverse)
library(brms)
library(tidybayes)

# load data
datzoo_trophic_state_only = readRDS(file = "data/datzoo_trophic_state_only.rds")
datzoo_trophic_guild_and_state = readRDS(file = "data/datzoo_trophic_guild_and_state.rds")

# fit models --------------------------------------------------------------

# prior justification - zoop density is typically between 0.1 to 60 ind/L with a mean of ~1 in these systems: "Doubek, J. P., Carey, C. C., Lavender, M., Winegardner, A. K., Beaulieu, M., Kelly, P. T., ... & Stockwell, J. D. (2019). PloS one, 14(1), e0209567."
# That leads to an intercept prior of ~ normal(0, 1) after log10 transformation and centering
# the slope prior is -0.75 from metabolic scaling

lake_level_brm = brm(log10_zoo_total_density_c ~ log10_zoo_mean_biomass_c,
                     data = datzoo_trophic_state_only,
                     family = gaussian(),
                     prior = c(prior(normal(0, 1), class = "Intercept"),
                               prior(normal(-0.75, 0.1), class = "b")))

trophic_state_brm = brm(log10_zoo_total_density_c ~ log10_zoo_mean_biomass_c*trophic_state,
                        data = datzoo_trophic_state_only,
                        family = gaussian(),
                        prior = c(prior(normal(0, 1), class = "Intercept"),
                                  prior(normal(-0.75, 0.1), coef = "log10_zoo_mean_biomass_c"),
                                  prior(normal(0, 1), class = "b")))

trophic_guild_and_state_brm = brm(log10_zoo_total_density_c ~ log10_zoo_mean_biomass_c*feeding_group*trophic_state,
                                  data = datzoo_trophic_guild_and_state,
                                  family = gaussian(),
                                  prior = c(prior(normal(0, 1), class = "Intercept"),
                                            prior(normal(-0.75, 0.1), coef = "log10_zoo_mean_biomass_c"),
                                            prior(normal(0, 1), class = "b")))


saveRDS(lake_level_brm, file = "models/lake_level_brm.rds") 
saveRDS(trophic_state_brm, file = "models/trophic_state_brm.rds")
saveRDS(trophic_guild_and_state_brm, file = "models/trophic_guild_and_state_brm.rds")

    


