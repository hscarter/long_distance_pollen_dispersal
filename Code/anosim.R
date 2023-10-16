# This code contains anosim analysis of morphology data by chemotype, and by population ####
# Haley Carter

# file paths:
scent_data <- "Data/scent_data.csv"
morph_data <- "Data/morph_data.csv"

library(vegan)
library(ecodist)

# read in scent data
corridor_data <- read.csv(scent_data)
# read in morphology data
corridor_morph <- read.csv(morph_data)

# ANOSIM
#extract columns with morph data
names(corridor_morph)
corridorTrim <- na.omit(corridor_morph[, c(2, 3, 18:25)])
traits <- corridorTrim[, 3:ncol(corridorTrim)]
traits.matrix <- as.matrix(traits)

# square root transform the traits matrix...
sqrt.traits.dist <- distance(sqrt(traits.matrix), "bray-curtis")
# anosim by population
ano.sqrt <- anosim(sqrt.traits.dist, corridorTrim$site)
ano.sqrt #sig, R = 0.1184, p = 0.001

#anosim by chemotype 
anolinmorph.sqrt <- anosim(sqrt.traits.dist, corridorTrim$chemotype)
anolinmorph.sqrt # r = -0.02206, p = 0.917
