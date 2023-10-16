#### Figure 2 ####
# Haley Carter

# libraries
library(tidyverse)
library(sf)
library(patchwork)
library(car)
library(ggalluvial)
library(ggpubr)

setwd("~/Documents/GitHub/pollen_dispersal_and_floral_scent/")

# data ####
corridorData <- read.csv("Data/morph_data.csv")
by_offspring <- read.csv("Data/by_offspring.csv")
matingPopulation <- read.csv("Data/by_matTrim.csv")

# combine for distance and paternal population
by_offspringT <- left_join(by_offspring, select(corridorData, Name, site), by = c("Candidate.father.ID" = "Name"))

# group by mating pair instead of offspring
by_mat <- select(by_offspringT, -Offspring.ID)
by_mat <- unique(by_mat)

# histograms 
range<-ggplot(by_mat, aes(dist/1000)) +
  geom_histogram(bins=50) +
  labs(x = "Distance (km)", y = "Number of \nmating pairs")+
  theme_minimal() # basically between pop freq
rouse<-ggplot(by_mat[by_mat$site == "Rouse", ], aes(dist)) +
  geom_histogram(bins = 50) +
  labs(x = "Distance (m)", y = "Number of \nmating pairs")+
  theme_minimal() # just Rouse, 50 bins, clearer fall off?

range / rouse

# alluvial plot ####
# data prep
matingPopulation <- matingPopulation %>% 
  mutate(siteCode = case_when(site == "HB" ~ "HB",
                              site == "Walsenburg" ~ "WALS",
                              site == "Rouse" ~ "ROU",
                              site == "Ludlow" ~ "LUD",
                              site == "Berwind" ~ "BER"),
         matchemo = case_when(chemotype.x == 0 ~ "lin-",
                              chemotype.x == 1 ~ "lin+"))
matingPopulation$siteCode <- factor(matingPopulation$siteCode, levels = c("HB", "WALS", "ROU", "LUD", "BER"))

# plot
popAlluvial <- ggplot(matingPopulation, aes(axis1 = siteCode, axis2=matchemo, fill=site)) +
  geom_alluvium(color="black") +
  scale_fill_manual(values = c("#0072B2","#E69F00", "#F0E442", "lightgrey", "#009E73", "#56B4E9", "grey44"), na.value = "white") +
  geom_stratum()+
  scale_x_discrete(limits = c("Paternal
  Population", "Maternal
  Chemotype"),
                   expand = c(.05, .05)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  labs(fill = "Population") +
  ylab("Number of mating pairs")+
  theme(legend.position="none", text = element_text(size = 12))
popAlluvial

# figure 2 ####

popAlluvial + (range/rouse) + plot_annotation(tag_level = "A")
