# Summarise pollen dispersal distances between populations and create figure 5 ####
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
# convert to numerica
dist_df <- dist_df %>% 
  mutate(across(6:9, as.numeric))
# pivot longer for plotting
dist_long <- dist_df %>% 
  pivot_longer(cols = c(ave_dist, lin_m_dist, lin_p_dist), names_to = "type", values_to = "average_distance") %>% 
  mutate(labels = case_when(type == "ave_dist" ~ "All",
                            type == "lin_m_dist" ~ "lin-",
                            type == "lin_p_dist" ~ "lin+"))
# add which combo dist measure is for instead of chemotype of indiv plants
dist_long <- dist_long %>% 
  mutate(combos = paste(chemotype, type, sep = "_"))

# anova models
amod<-aov(average_distance ~ type, data = dist_long)
summary(amod) # p = 0.019
TukeyHSD(amod) # lin+ diff than lin-, p = 0.014

amod2 <- aov(average_distance ~ type * chemotype, data = dist_long)
summary(amod2) # both sig

# with all combinations
amod3 <- aov(average_distance ~ combos, data = dist_long)
summary(amod3)
TukeyHSD(amod3)

par(mfrow=c(2,2))
plot(amod3)
par(mfrow=c(1,1))

# rename chemotypes for figure
dist_long <- dist_long %>% 
  mutate(chemCode = case_when(chemotype == 0 ~ "lin- plants",
                              chemotype == 1 ~ "lin+ plants"))

# plot distances across range
rangeChemDist <- ggplot(dist_long, aes(labels, average_distance/1000))+
  geom_boxplot(fill="lightgrey", color = "grey43", width = 0.3)+
  theme_bw()+
  labs(x ="", y = "Distance (km)")+
  facet_wrap(vars(chemCode))

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

# anova model
amod3Rouse <- aov(average_distance ~ combos, data = rouse_dist_long)
summary(amod3Rouse)
TukeyHSD(amod3Rouse)

par(mfrow=c(2,2))
plot(amod3Rouse)
par(mfrow=c(1,1))

# change chemotype names for labels on plot
rouse_dist_long <- rouse_dist_long %>% 
  mutate(chemCode = case_when(chemotype == 0 ~ "lin- plants",
                              chemotype == 1 ~ "lin+ plants"))

# plot ave distances within rouse
rouseChemDist <-ggplot(rouse_dist_long, aes(labels, average_distance))+
  geom_boxplot(fill="lightgrey", color = "grey43", width = 0.3)+
  theme_bw()+
  labs(x ="", y = "Distance (m)")+
  facet_wrap(vars(chemCode))

# plot range and rouse together
rangeChemDist / rouseChemDist

ggplot(rouse_dist_long, aes(labels, average_distance))+
  geom_violin(fill="lightgrey", color = "grey43")+
  theme_bw()+
  labs(x ="", y = "Distance (m)")+
  facet_wrap(vars(chemotype))

# calculate summary
rouse_dist_long %>% 
  group_by(type, chemotype) %>% 
  summarise(average = mean(average_distance),
            n = n(),
            sd = sd(average_distance),
            se = sd/sqrt(n))

# distance chemotype by cross ####
# change data for plot labels
by_mat <-  by_mat %>% 
  mutate(patchemotype = case_when(patchem == 0 ~ "lin-",
                                  patchem == 1 ~ "lin+"),
         matchemotype = case_when(matchem == 0 ~ "lin- maternal lines",
                                  matchem == 1 ~ "lin+ maternal lines"))

# plot for range
mateDistRange<-ggplot(by_mat[!is.na(by_mat$patchem),], aes(patchemotype, dist/1000))+
  geom_boxplot(fill = "lightgrey", color = "grey43", width = 0.3)+
  theme_bw()+
  labs(x = "Pollen donor chemotype", y = "Distance (km)")+
  facet_wrap(vars(matchemotype))

# plot for rouse
mateDistRouse<-ggplot(by_mat[!is.na(by_mat$patchem) & by_mat$site == "Rouse",], aes(patchemotype, dist))+
  geom_boxplot(fill = "lightgrey", color = "grey43", width = 0.3)+
  theme_bw()+
  labs(x = "Pollen donor chemotype", y = "Distance (m)")+
  facet_wrap(vars(matchemotype))

#mateDistBTWpops<-ggplot(by_mat[!is.na(by_mat$patchem) & by_mat$site != "Rouse",], aes(as.factor(patchem), dist))+
#  geom_boxplot(fill = "lightgrey", color = "grey43", width = 0.3)+
#  theme_bw()+
#  labs(x = "Pollen donor chemotype", y = "Distance (m) between\n inter-population mating pairs")+
#  facet_wrap(vars(as.factor(matchem)))

mateDistRange / mateDistRouse 

# all together for figure 5
ggarrange(rangeChemDist, mateDistRange, rouseChemDist, mateDistRouse, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))

####



