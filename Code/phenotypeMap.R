# Create map of Rouse with plants colored by linalool phenotype ####
# Haley Carter

# libraries
library(tidyverse)
library(sf)
library(ggrepel)
library(patchwork)
library(ggspatial)
library(maps)
library(mapdata)

# read in data ####
corridor_scent <- read.csv("Data/scent_data.csv")
corridor_morph <- read.csv("Data/morph_data.csv")

by_maternal <- read.csv("Data/by_maternal.csv")
by_offspring <- read.csv("Data/by_offspring.csv")
# remove offspring without paternal chemotypes
by_offspring <- by_offspring[!is.na(by_offspring$patchem),]
# rename offspring column
names(by_offspring)[1] <- "Offspring"
# combine as map data for Rouse ####
mapData <- left_join(select(corridor_morph,
                            Name, site,
                            chemotype,
                            Lon, Lat),
                     select(corridor_scent,
                            Name, linalool_phenotype),
                     by = c("Name" = "Name")) %>% 
  filter(site == "Rouse")
# convert to spatial
mapData <- mapData[complete.cases(mapData),]
mapSF <- st_as_sf(mapData, coords = c("Lon", "Lat"), remove = T, crs = 4326)

# set linalool color palette
lin_palette = c("#595E63", "lightgray", "white")
lin_palette2 = c("black", "darkgrey", "white" )

# rouseMap ####
rouMap <- ggplot() +
  geom_sf(data=mapSF, inherit.aes=F, aes(fill = linalool_phenotype), shape = 21, alpha = 0.5, size =3) +
  annotation_scale(pad_y=unit(0.5, "cm"))+
  scale_fill_manual(values = lin_palette2)+
  geom_rect(aes(xmin=-104.7068,
                xmax=-104.7059, ymin=37.5336,
                ymax=37.5348), color="red",
            fill = NA)+
  scale_x_continuous(breaks=seq(-104.7060, -104.7068, by=-0.0008))+
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0, angle = 0, vjust = 1), legend.position = "bottom") +
  labs(fill = "Phenotype",
       x = "Longitude",
       y = "Latitude")
rouMap