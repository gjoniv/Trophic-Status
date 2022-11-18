
# Set working directory

setwd('~/Dropbox/Savina_size_scaling_project/1. data/NLA2012')

# Libraries 
library(dplyr)
library(purrr)
library(broom)
library(janitor)
library(ggplot2)
library(ggthemes)

# Read in data files
chemdat <-  read.csv('nla2012_waterchem_wide.csv')
physdat <- read.csv('nla2012_wide_profile_08232016.csv') %>%
  clean_names()
phytodat <- read.csv('nla2012_wide_phytoplankton_count_2020_04_25.csv') %>%
  clean_names()
zoodat_meta <- read.csv('nla2012_zooptaxa_wide_10272015.csv') %>%
  select(TARGET_TAXON, FFG) %>%
  rename(feeding_group = FFG)

keydat <- read.csv('nla_2012_condition_categories.csv') %>%
  select(UID, TROPHIC_STATE) %>%
  rename(trophic_state = TROPHIC_STATE) %>%
  rename(uid = UID)

zoodat_orig <- read.csv('nla2012_wide_zooplankton_count_2020_04_30.csv') %>%
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
  group_by(uid) %>%
  summarise(zoo_mean_biomass = log10(sum(biomass)/sum(density)),
            zoo_total_density = log10(sum(density)))

ccsr_mergedat <- left_join(zoodatsummary, keydat, by = "uid") 

### II. Zooplankton data processing for feeding group
zoodatffg <- zoodat_orig %>%
  filter(., visit_no == 1) %>%
  filter(., sample_type == 'ZOFN' | sample_type == 'ZOCN') %>%
  mutate(biomass = as.numeric(as.character(na_if(biomass, '#DIV/0!')))) %>%
  mutate(density = as.numeric(as.character(na_if(density, '#DIV/0!')))) %>%
  filter(., density != 'NA') %>%
  filter(., biomass != 'NA')  %>%
  group_by(uid, feeding_group) %>%
  summarise(zoo_mean_biomass = log10(sum(biomass)/sum(density)),
            zoo_total_density = log10(sum(density)))

### III. Merging CCSR datasets for trophic status  x feeding gropu

datzoo1 <- left_join(zoodatffg, keydat, by = "uid") 

#DATASET FEEDING GROUP
datzoo <- ccsr_mergedat %>%
#filter(., feeding_group == 'HERB' | feeding_group == 'OMNI' | feeding_group == 'PRED') %>%
  filter(., trophic_state == 'Oligotrophic' | trophic_state == 'Mesotrophic' | trophic_state == 'Eutrophic'| trophic_state == 'Hypereutrophic') %>%
  filter(., zoo_total_density != 'NA') %>%
  filter(., zoo_mean_biomass != '-Inf')  %>%
  filter(., zoo_total_density != '-Inf') %>%
  filter(., zoo_mean_biomass != 'NA')  

#all group slope
slope <- lm(zoo_total_density ~ zoo_mean_biomass, datzoo)
summary(slope)

#Figure 2
ggplot(data = zoodatsummary, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color = "black", size = 1.5, alpha = 0.5) + 
  geom_smooth(color = "black", method = "lm", se = TRUE) + 
  labs(x = "log body size", y = "log abundance", color = "Legend") + theme_few()

Your Walk di inglese che non sei#all group slope x trophic guild
slope <- lm(zoo_total_density ~ zoo_mean_biomass * feeding_group, datzoo)
summary(slope)

  #all group slope  x trophic status
  slope <- lm(zoo_total_density ~ zoo_mean_biomass * trophic_state, datzoo)
  summary(slope)

  #all group slope  x trophic status  x trophic guild
  slope <- lm(zoo_total_density ~ zoo_mean_biomass * trophic_state * feeding_group, datzoo)
  summary(slope)

  #Figure 3
  ggplot(datzoo, aes(zoo_mean_biomass, zoo_total_density, colour=feeding_group, fill=feeding_group)) + 
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE) + 
  geom_point(size=1.5) + ylab("log abundance") + 
  xlab("log body size") + theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
                                  text = element_text(size = 12, family = "Tahoma"), 
                                  axis.title = element_text(face="bold"), axis.text.x=element_text(size = 11), 
                                  legend.position = "top") + theme_few() + 
  scale_color_manual(values=c("blue", "green", "red")) + labs(fill = "trophic state")
  
  datzooffg <- zoodatffg %>%
  filter(., feeding_group == 'HERB' | feeding_group == 'OMNI' | feeding_group == 'PRED') %>%
  #filter(., trophic_state == 'Oligotrophic' | trophic_state == 'Mesotrophic' | trophic_state == 'Eutrophic'| trophic_state == 'Hypereutrophic') %>%
  filter(., zoo_total_density != 'NA') %>%
  filter(., zoo_mean_biomass != '-Inf')  %>%
  filter(., zoo_total_density != '-Inf') %>%
  filter(., zoo_mean_biomass != 'NA')
  
  #FIGURE 4A
  
  datzooffg <- zoodatffg %>%
    filter(., feeding_group == 'HERB' | feeding_group == 'OMNI' | feeding_group == 'PRED') %>%
    #filter(., trophic_state == 'Oligotrophic' | trophic_state == 'Mesotrophic' | trophic_state == 'Eutrophic'| trophic_state == 'Hypereutrophic') %>%
    filter(., zoo_total_density != 'NA') %>%
    filter(., zoo_mean_biomass != '-Inf')  %>%
    filter(., zoo_total_density != '-Inf') %>%
    filter(., zoo_mean_biomass != 'NA')

  #OMNIVORES SLOPE
  datomni <- datzooffg[datzooffg$feeding_group=="OMNI",]
  slope <- lm(zoo_total_density ~ zoo_mean_biomass, datomni)
  summary(slope)
  
  ggplot(data = datomni, 
         aes(x = zoo_mean_biomass, 
             y = zoo_total_density)) + 
    geom_point(color = "black", size = 2, alpha = 0.5) + 
    geom_smooth(color = "black", method = "lm", se = TRUE) + 
    labs(x = "log body size", y = "log abundance", color = "Legend") + theme_few()
  
  #FIGURE 4B
  #HERBIVORE SLOPE
  datherb <-datzooffg[datzooffg$feeding_group=="HERB",]
  slope <- lm(zoo_total_density ~ zoo_mean_biomass, datherb)
  summary(slope)
  
  ggplot(data = datherb, 
         aes(x = zoo_mean_biomass, 
             y = zoo_total_density)) + 
    geom_point(color = "black", size = 2, alpha = 0.5) + 
    geom_smooth(color = "black", method = "lm", se = TRUE) + 
    labs(x = "log body size", y = "log abundance", color = "Legend") + theme_few()
  
  #FIGURE 4C
  #PREDATOR SLOPE
  datpred <- datzooffg[datzooffg$feeding_group=="PRED",]
  slope <- lm(zoo_total_density ~ zoo_mean_biomass, datpred)
  summary(slope)
  
  ggplot(data = datpred, 
         aes(x = zoo_mean_biomass, 
             y = zoo_total_density)) + 
    geom_point(color = "black", size = 2, alpha = 0.5) + 
    geom_smooth(color = "black", method = "lm", se = TRUE) + 
    labs(x = "log body size", y = "log abundance", color = "Legend") + theme_few()
  
  #DATASET FEEDING GROUP
  datzoo1 <- datzoo1 %>%
    filter(., feeding_group == 'HERB' | feeding_group == 'OMNI' | feeding_group == 'PRED') %>%
    filter(., trophic_state == 'Oligotrophic' | trophic_state == 'Mesotrophic' | trophic_state == 'Eutrophic'| trophic_state == 'Hypereutrophic') %>%
    filter(., zoo_total_density != 'NA') %>%
    filter(., zoo_mean_biomass != '-Inf')  %>%
    filter(., zoo_total_density != '-Inf') %>%
    filter(., zoo_mean_biomass != 'NA') 

#FIGURES FOR THE MANUSCRIPT
ggplot(datherb, aes(zoo_mean_biomass, zoo_total_density, colour=trophic_state, fill=trophic_state)) + 
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE) + 
  geom_point(size=1.5) + ylab("log abundance") + 
  xlab("log body size") + theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
                                text = element_text(size = 12, family = "Tahoma"), 
                                axis.title = element_text(face="bold"), axis.text.x=element_text(size = 11), 
                                legend.position = "top") + theme_few() + 
  scale_color_manual(values=c("blue", "purple", "green", "red")) + labs(fill = "trophic state") 

ggplot(datpred, aes(zoo_mean_biomass, zoo_total_density, colour=trophic_state, fill=trophic_state)) + 
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE) + 
  geom_point(size=1.5) + ylab("log abundance") + 
  xlab("log body size") + theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
                                text = element_text(size = 12, family = "Tahoma"), 
                                axis.title = element_text(face="bold"), axis.text.x=element_text(size = 11), 
                                legend.position = "top") + theme_few() + 
  scale_color_manual(values=c("blue", "purple", "green", "red")) + labs(fill = "trophic state") 


ggplot(data = datpredolig, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color = "purple", size = 2, alpha = 0.5) + 
  geom_smooth(color = "purple", method = "lm", se = TRUE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()

datpredmeso <- datpred[datpred$trophic_state=="Mesotrophic",]
slope <- lm(zoo_total_density ~ zoo_mean_biomass, datpredmeso)
summary(slope)

ggplot(data = datpredmeso, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color = "blue", size = 2, alpha = 0.5) + 
  geom_smooth(color = "blue", method = "lm", se = TRUE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()

datpredeu <- datpred[datpred$trophic_state=="Eutrophic",]
slope <- lm(zoo_total_density ~ zoo_mean_biomass, datpredeu)
summary(slope)

ggplot(data = datpredeu, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color = "green", size = 2, alpha = 0.5) + 
  geom_smooth(color = "green", method = "lm", se = TRUE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()

#Herbivorous slope
datherb <- datzoo[datzoo$feeding_group=="HERB",]
slope <- lm(zoo_total_density ~ zoo_mean_biomass, datherb)
summary(slope)

#Omnivorous slope
datomni <- datzoo[datzoo$feeding_group=="OMNI", ]
slope <- lm(zoo_total_density ~ zoo_mean_biomass, datomni)
summary(slope)


ggplot(data = datpred, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color ="blue", size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", color ="blue", se = FALSE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()


ggplot(data = datomni, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color ="red", size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", color ="red", se = FALSE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()


ggplot(data = datherb, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color = "green", size = 2, alpha = 0.5) + 
  geom_smooth(color = "green", method = "lm", se = TRUE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()

ggplot(data = datpredolig, 
       aes(x = zoo_mean_biomass, 
           y = zoo_total_density)) + 
  geom_point(color = "green", size = 2, alpha = 0.5) + 
  geom_smooth(color = "green", method = "lm", se = TRUE) + 
  labs(x = "log body size", y = "log abundance") + theme_few()
