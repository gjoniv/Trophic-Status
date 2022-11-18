
# Set working directory

# setwd('~/Dropbox/Savina_size_scaling_project/1. data/NLA2012')

# Libraries 
# library(dplyr)
# library(purrr)
# library(broom)
library(janitor)
# library(ggplot2)
library(ggthemes)
library(tidyverse)

# Read in data files
chemdat <-  read.csv('NLA2012/nla2012_waterchem_wide.csv')
physdat <- read.csv('NLA2012/nla2012_wide_profile_08232016.csv') %>%
  clean_names()
phytodat <- read.csv('NLA2012/nla2012_wide_phytoplankton_count_2020_04_25.csv') %>%
  clean_names()
zoodat_meta <- read.csv('NLA2012/nla2012_zooptaxa_wide_10272015.csv') %>%
  select(TARGET_TAXON, FFG) %>%
  rename(feeding_group = FFG)

keydat <- read.csv('NLA2012/nla_2012_condition_categories.csv') %>%
  select(UID, TROPHIC_STATE) %>%
  rename(trophic_state = TROPHIC_STATE) %>%
  rename(uid = UID)

zoodat_orig <- read.csv('NLA2012/nla2012_wide_zooplankton_count_2020_04_30.csv') %>%
  left_join(., zoodat_meta, by = 'TARGET_TAXON') %>%
  clean_names()


### Chemical data processing ####
# Selecting chemical data to merge using UID as identified 

chemdat <- select(chemdat, c(UID, PTL_RESULT, NITRATE_N_RESULT, SILICA_RESULT)) %>%
  # filter(visit_no == 1) %>%
  group_by(UID) %>%
  summarise(phos_total_mean = mean(PTL_RESULT, na.rm = TRUE),
            nitrate_mean = mean(NITRATE_N_RESULT, na.rm = TRUE),
            silicate_mean = mean(SILICA_RESULT, na.rm = TRUE)) %>%
  rename(uid = UID)

### Physical data processing
physdat <- physdat %>% 
  filter(visit_no == 1) %>%
  select(., c(uid, temperature)) %>%
  group_by(uid) %>%
  summarise(temp_mean = mean(temperature, na.rm = TRUE))

### Phytoplankton data processing  (using UID as identifier as the chem data doesnt have site_ID variable)
# filter data only to Visit no 1 and biovol > 0

# Phytoplankton mean biovolume and total density per lake
phytodatsummary <- phytodat %>%
  filter(., visit_no == 1) %>%
  filter(., density != 'NA') %>%
  filter(., biovolume != 'NA') %>%
  filter(., biovolume > 0) %>% 
  group_by(uid) %>%
  summarise(phyto_mean_biovol = sum(biovolume)/sum(density),
            phyto_se_biovol = sd(rep(biovolume, density)) / sqrt(sum(density)),
            phyto_total_density = sum(density))

### Zooplankton data processing
### I. Zooplankton data processing for trophic guild
zoodatsummary <- zoodat_orig %>%
  filter(., visit_no == 1) %>%
  filter(., sample_type == 'ZOFN' | sample_type == 'ZOCN') %>%
  mutate(biomass = as.numeric(as.character(na_if(biomass, '#DIV/0!')))) %>%
  mutate(density = as.numeric(as.character(na_if(density, '#DIV/0!')))) %>%
  filter(., density != 'NA') %>%
  filter(., biomass != 'NA')  %>%
  filter(!is.na(density)) %>%
  filter(!is.na(biomass)) %>%
  group_by(uid) %>%
  summarise(log10_zoo_mean_biomass = log10(sum(biomass)/sum(density)),
            log10_zoo_total_density = log10(sum(density)))

ccsr_mergedat <- left_join(zoodatsummary, keydat, by = "uid") 

### II. Zooplankton data processing for feeding group
zoodatffg <- zoodat_orig %>%
  filter(., visit_no == 1) %>%
  filter(., sample_type == 'ZOFN' | sample_type == 'ZOCN') %>%
  mutate(biomass = as.numeric(as.character(na_if(biomass, '#DIV/0!')))) %>%
  mutate(density = as.numeric(as.character(na_if(density, '#DIV/0!')))) %>%
  filter(., density != 'NA') %>%
  filter(., biomass != 'NA') %>%
  filter(!is.na(density)) %>%
  filter(!is.na(biomass)) %>% 
  filter(biomass != 0 & density != 0) %>%
  group_by(uid, feeding_group) %>%
  summarise(log10_zoo_mean_biomass = log10(sum(biomass, na.rm = T)/sum(density, na.rm = T)),
            log10_zoo_total_density = log10(sum(density, na.rm = T))) %>%
  filter(feeding_group == 'HERB' | feeding_group == 'OMNI' | feeding_group == 'PRED') %>% 
  ungroup %>% 
  mutate(mean_log10_biomass = mean(log10_zoo_mean_biomass, na.rm = T),
         mean_log10_density = mean(log10_zoo_total_density, na.rm = T),
         log10_zoo_mean_biomass_c = log10_zoo_mean_biomass - mean_log10_biomass,
         log10_zoo_total_density_c = log10_zoo_total_density - mean_log10_density)

### III. Merging CCSR datasets for trophic status  x trophic guild

datzoo_trophic_guild_and_state <- left_join(zoodatffg, keydat, by = "uid") %>%
  filter(., trophic_state == 'Oligotrophic' | trophic_state == 'Mesotrophic' | trophic_state == 'Eutrophic'| trophic_state == 'Hypereutrophic')


datzoo_trophic_state_only <- ccsr_mergedat %>%
  filter(., trophic_state == 'Oligotrophic' | trophic_state == 'Mesotrophic' | trophic_state == 'Eutrophic'| trophic_state == 'Hypereutrophic') %>%
  filter(., log10_zoo_total_density != 'NA') %>%
  filter(., log10_zoo_mean_biomass != '-Inf')  %>%
  filter(., log10_zoo_total_density != '-Inf') %>%
  filter(., log10_zoo_mean_biomass != 'NA') %>% 
  ungroup %>% 
  mutate(mean_log10_biomass = mean(log10_zoo_mean_biomass, na.rm = T),
         mean_log10_density = mean(log10_zoo_total_density, na.rm = T),
         log10_zoo_mean_biomass_c = log10_zoo_mean_biomass - mean_log10_biomass,
         log10_zoo_total_density_c = log10_zoo_total_density - mean_log10_density)

saveRDS(datzoo_trophic_guild_and_state, file = "data/datzoo_trophic_guild_and_state.rds")
saveRDS(datzoo_trophic_state_only, file = "data/datzoo_trophic_state_only.rds")

