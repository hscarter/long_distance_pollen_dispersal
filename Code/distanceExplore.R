# Summarise pollen dispersal distances between populations ####
# Haley Carter

# libraries
library(dplyr)

# data ####
corridorData <- read.csv("Data/morph_data.csv")

by_offspring <- read.csv("Data/by_offspring.csv")

# combine for distance and paternal population
by_offspringT <- left_join(by_offspring, select(corridorData, Name, site), by = c("Candidate.father.ID" = "Name"))

# group by mating pair instead of offspring
by_mat <- select(by_offspringT, -Offspring.ID)
by_mat <- unique(by_mat)
table(by_mat$site)

# summarise pollen disperal distances
by_mat %>% group_by(site) %>% 
  summarise(count = n(), meanDist = mean(dist), sdDist = sd(dist), se = sd(dist)/sqrt(n()))
