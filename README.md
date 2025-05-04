# Data and code for Ecology manuscript
Haley Carter

This repository houses data and code associated with the manuscript 
"Hawkmoths provide extensive long-distance pollen dispersal to a small population of a rare evening primrose" 
by Haley S. Carter, Robert A. Raguso, Jeremie B. Fant, Maria Sol Balbuena, Geoffrey T. Broadhead, and Krissa A. Skogen.

# Data
* by\_maternal.csv All mating pairs with number of offspring and linalool chemotype information for both parental individuals. 
* by\_matTrim.csv Mating pair data with pollen donor site information.
* by\_offspring.csv Offspring individuals with parental morphology, georeferences, and linalool information. 
* latency\_females.csv Data from the behavioral assays with Hyles lineata.
* latency\_males.csv Data from the behavioral assays with Hyles lineata.
* morph\_data.csv Floral morphology data for all 205 potential parental individuals from the field populations. 
* scent\_data.csv Floral scent data for all 205 potential parental individuals from the field populations.
* wind\_tunnel.xlsx Data from the behavioral assays with Hyles lineata.
* RouseDistance.xlsx Distance matrix of Rouse plants.
* parentalGenotypes.xlsx Microsattelite genotypes for parental plants.
* offspringGenotypes.xlsx Microsattelite genotypes for offspring plants.

# Code files
* anosim.R - Analysis of similarities in floral trait data between chemotypes and between populations. 
* betweenPopulations.R - Frequency of between population mating events overall and by chemo- and phenotypes.
* distance.R - Calculating distances between mating pairs and all plants in Rouse.
* figure3.R - Hyles behavioral supplement code and figures.
* figure1.R - Code for panel B of figure 1.
* figure2.R - Code for all panels of figure 2.
* floralRewards.R - Mixed effect models of floral rewards (nectar and sucrose).
