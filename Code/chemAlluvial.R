# Make alluvial figures grouping mating pairs by paternal and maternal linalool chemotypes and phenotypes. ####
# Haley Carter

# set up ####
# file paths:
scent_data <- "Data/scent_data.csv"
mating_data <- "Data/by_maternal.csv"
# libraries
library(tidyverse)
library(ggalluvial)
library(patchwork)
library(ggpubr)

# data ####
corridor_data <- read.csv(scent_data)
by_maternal <- read.csv(mating_data)
# add the linalool phenotype data to mating pairs
by_maternal <- by_maternal %>%
  left_join(select(corridor_data, Name, linalool_phenotype), by = c("Mother.ID" = "Name")) %>% 
  left_join(select(corridor_data, Name, linalool_phenotype, site), by = c("Candidate.father.ID" = "Name"))
by_maternal <- by_maternal[!is.na(by_maternal$linalool_phenotype.y), ]
by_maternal <- by_maternal %>% mutate(matChemotype = recode(chemotype.x, "1" = "linalool", "0" = "none"), patChemotype = recode(chemotype.y, "1" = "linalool", "0" = "none"))
# create just Rouse dataframe
by_maternalRouse <- by_maternal[by_maternal$site == "Rouse", ]

# plotting ####
# define color palettes
lin_palette = c( "#595E63", "lightgray", "white")
lin_palette2 = c( "darkgrey", "white")
# create all populations alluvial plot coded by linalool chemotype and scaled by number of mating pairs
chemotype <- by_maternal %>% group_by(patChemotype) %>% 
  ggplot(aes(axis1 = matChemotype,
             axis2 = patChemotype,
             fill = patChemotype)) +
  geom_alluvium(color = "lightgrey") +
  geom_stratum(width = 0.25) +
  geom_text(stat = "stratum", size = 4,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Maternal", "Paternal"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = lin_palette2) +
  theme_classic() +
  ylab("Number of mating pairs") +
  coord_flip() +
  theme(legend.position = "none")
# create just Rouse alluvial plot coded by linalool chemotype and scaled by number of mating pairs
chemotypeRouse <- by_maternalRouse %>% group_by(patChemotype) %>% 
  ggplot(aes(axis1 = matChemotype,
             axis2 = patChemotype,
             fill = patChemotype)) +
  geom_alluvium(color = "lightgrey") +
  geom_stratum(width = 0.25) +
  geom_text(stat = "stratum", size = 4,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Maternal", "Paternal"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = lin_palette2) +
  theme_classic() +
  ylab("Number of mating pairs") +
  coord_flip() +
  theme(legend.position = "none")

# create all population alluvial plot coded by linalool phenotypes and scaled by number of mating pairs
phenotype <- by_maternal %>% group_by(linalool_phenotype.y) %>% 
  ggplot(aes(axis1 = linalool_phenotype.x,
             axis2 = linalool_phenotype.y,
             fill = linalool_phenotype.y)) +
  geom_alluvium(color = "lightgrey") +
  geom_stratum(width = .25) +
  geom_text(stat = "stratum",
            size = 4,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Maternal", "Paternal"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = lin_palette) +
  theme_classic() +
  ylab("Number of mating pairs") +
  coord_flip() +
  theme(legend.position = "none")
# create just Rouse alluvial plot coded by linalool phenotype and scaled by number of mating pairs
phenotypeRouse <- by_maternalRouse %>% group_by(linalool_phenotype.y) %>% 
  ggplot(aes(axis1 = linalool_phenotype.x,
             axis2 = linalool_phenotype.y,
             fill = linalool_phenotype.y)) +
  geom_alluvium(color = "lightgrey") +
  geom_stratum(width = .25) +
  geom_text(stat = "stratum",
            size = 4,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Maternal", "Paternal"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = lin_palette) +
  theme_classic() +
  ylab("Number of mating pairs") +
  coord_flip() +
  theme(legend.position = "none")
# put them all together in as a panel figure
ggarrange(chemotype, phenotype, chemotypeRouse, phenotypeRouse, ncol = 2, nrow =2, labels = c("A", "B", "C", "D"))

# create Null visualization ####
sum(by_maternal$chemotype.x)
sum(by_maternal$chemotype.y)
table(by_maternal$linalool_phenotype.x)
table(by_maternal$linalool_phenotype.y)
nullChemo <- tribble(~pat, ~mat, ~freq,
                     "none", "none", 8.5,
                     "none", "linalool", 23,
                     "linalool", "none", 8.5,
                     "linalool", "linalool", 23)
nullPheno <- tribble(~pat, ~mat, ~freq,
                     "none", "none", 7.3,
                     "none", "low", 8.6,
                     "none", "high", 6.6,
                     "low", "none", 7.3,
                     "low", "low", 8.6,
                     "low", "high", 6.6,
                     "high", "none", 7.3,
                     "high", "low", 8.6,
                     "high", "high", 6.6)
nullchemotype<-ggplot(nullChemo, aes(axis1=mat,
                      axis2=pat,
                      y=freq,
                      fill=pat))+
  geom_alluvium(color = "lightgrey") +
  geom_stratum(width = 0.25) +
  geom_text(stat = "stratum", size = 4,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Maternal", "Paternal"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = lin_palette2) +
  theme_classic() +
  ylab("Number of mating pairs") +
  coord_flip() +
  theme(legend.position = "none")
nullchemotype+chemotype
nullphenotype<-ggplot(nullPheno, aes(axis1=mat,
                                     axis2=pat,
                                     y=freq,
                                     fill=pat))+
  geom_alluvium(color = "lightgrey") +
  geom_stratum(width = 0.25) +
  geom_text(stat = "stratum", size = 4,
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Maternal", "Paternal"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = lin_palette) +
  theme_classic() +
  ylab("Number of mating pairs") +
  coord_flip() +
  theme(legend.position = "none")
nullphenotype
nullphenotype+phenotype
(nullchemotype+nullphenotype)/(chemotype+phenotype)
(nullchemotype+nullphenotype)/(chemotype+phenotype)/(chemotypeRouse+phenotypeRouse)
