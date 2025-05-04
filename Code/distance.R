# Calculate distances between plants and mating pairs ####
# Haley Carter

# note this code also includes some explorating of mating distances
# by chemotype which was not included in the manuscript

# the rouse distance matrix created starting in line 108 was used to determine
# nearest neighbors and to manually count mating frequency btw neighbors

# libraries
library(tidyverse)
library(sf)
library(patchwork)
library(car)
library(ggalluvial)
library(ggpubr)

setwd("~/Documents/DispersalPaper/long_distance_pollen_dispersal/")

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

# distance chemotype overall ####
corridor <- corridorData %>%
  dplyr::select(Name, site, chemotype, Lon, Lat) %>% 
  filter(complete.cases(.)) %>% 
  mutate(namechem = paste(Name, chemotype, sep = "_"))
# convert to spatial
corridor_sf <- st_as_sf(corridor, coords = c("Lon", "Lat"), crs = 4326)
# calculate distance matrix
distances <- st_distance(corridor_sf)
row.names(distances) <- corridor$namechem
# save as dataframe
distances_df <- as.data.frame(distances)
names(distances_df) <- corridor$namechem

# summarise and calculated average distances
dist_num <- distances_df %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(Name = row.names(.)) %>% 
  select(Name, everything()) %>% 
  separate(Name, into = c("Name", "chemotype"))
# between all plants
ave_all <- dist_num %>% 
  summarise(across(3:last_col(), mean)) %>% 
  t()
# add standard deviation
std_all <- dist_num %>% 
  summarise(across(3:last_col(), sd)) %>% 
  t()

# between chemotypes
lin_dist <- dist_num %>% 
  group_by(chemotype) %>% 
  summarise(across(2:last_col(), mean)) %>% 
  t()
lin_dist <- lin_dist[2:nrow(lin_dist), ]
# add standard deviation
lin_std <- dist_num %>% 
  group_by(chemotype) %>% 
  summarise(across(2:last_col(), sd)) %>% 
  t()
lin_std <- lin_std[2:nrow(lin_std), ]

# reformat with average distances added to individual data
dist_df <- corridor %>% 
  select(Name, site, chemotype)
dist_df <- cbind(dist_df, ave_all, std_all, lin_dist, lin_std)
names(dist_df) <- c("Name", "site", "chemotype", "ave_dist", "std_dist", "lin_m_dist", "lin_p_dist", "lin_m_std", "lin_p_std")
# convert to numerical
dist_df <- dist_df %>% 
  mutate(across(6:9, as.numeric))

# rename chemotypes for labeling
dist_long <- dist_long %>% 
  mutate(chemCode = case_when(chemotype == 0 ~ "lin- plants",
                              chemotype == 1 ~ "lin+ plants"))


# calculate summary distances
dist_long %>% 
  group_by(type, chemotype) %>% 
  summarise(average = mean(average_distance),
            n = n(),
            sd = sd(average_distance),
            se = sd/sqrt(n))

# clustering in Rouse ####
rouse <- corridorData %>%
  dplyr::select(Name, site, chemotype, Lon, Lat) %>% 
  filter(complete.cases(.), site == "Rouse") %>% 
  mutate(namechem = paste(Name, chemotype, sep = "_"))
# conver to spatial
rouse_sf <- st_as_sf(rouse, coords = c("Lon", "Lat"), crs = 4326)
# calculate distance matrix
rouse_distances <- st_distance(rouse_sf)
row.names(rouse_distances) <- rouse$namechem
# save as dataframe
rouse_df <- as.data.frame(rouse_distances)
names(rouse_df) <- rouse$namechem

# calculate averages and standard deviations
rouse_dist_num <- rouse_df %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(Name = row.names(.)) %>% 
  select(Name, everything()) %>% 
  separate(Name, into = c("Name", "chemotype"))

ave_all_rouse <- rouse_dist_num %>% 
  summarise(across(3:last_col(), mean)) %>% 
  t()

std_all_rouse <- rouse_dist_num %>% 
  summarise(across(3:last_col(), sd)) %>% 
  t()

# add between chemotype mating distances (not included in manuscript)
lin_dist_rouse <- rouse_dist_num %>% 
  group_by(chemotype) %>% 
  summarise(across(2:last_col(), mean)) %>% 
  t()
lin_dist_rouse <- lin_dist_rouse[2:nrow(lin_dist_rouse), ]

lin_std_rouse <- rouse_dist_num %>% 
  group_by(chemotype) %>% 
  summarise(across(2:last_col(), sd)) %>% 
  t()
lin_std_rouse <- lin_std_rouse[2:nrow(lin_std_rouse), ]

rouse_dist_df <- rouse %>% 
  select(Name, site, chemotype)

# reformat into dataframe
rouse_dist_df <- cbind(rouse_dist_df, ave_all_rouse, std_all_rouse, lin_dist_rouse, lin_std_rouse)
names(rouse_dist_df) <- c("Name", "site", "chemotype", "ave_dist", "std_dist", "lin_m_dist", "lin_p_dist", "lin_m_std", "lin_p_std")

# add ave distance to indiv plants
rouse_dist_df <- rouse_dist_df %>% 
  mutate(across(6:9, as.numeric))

# pivot for plotting
rouse_dist_long <- rouse_dist_df %>% 
  pivot_longer(cols = c(ave_dist, lin_m_dist, lin_p_dist), names_to = "type", values_to = "average_distance") %>% 
  mutate(labels = case_when(type == "ave_dist" ~ "All",
                            type == "lin_m_dist" ~ "lin-",
                            type == "lin_p_dist" ~ "lin+"))

rouse_dist_long <- rouse_dist_long %>% 
  mutate(combos = paste(chemotype, type, sep = "_"))

# calculate summary within Rouse by chemotype
rouse_dist_long %>% 
  group_by(type, chemotype) %>% 
  summarise(average = mean(average_distance),
            n = n(),
            sd = sd(average_distance),
            se = sd/sqrt(n))






