# Data and code for Annals of Botany manuscript
Haley Carter


# Data
* by\_maternal.csv Dataset S1: All mating pairs with number of offspring and linalool chemotype information for both parental individuals. 
* by\_matTrim.csv Dataset S2: Mating pair data with pollen donor site information.
* by\_offspring.csv Dataset S3: Offspring individuals with parental morphology, georeferences, and linalool information. 
* f2\_scent\_data.csv Dataset S4: Linalool emissions data for greenhouse plants.
* germinationF2.csv Dataset S5: Data on individual plants of the F2 generation including an identifier for the cross, an identifier for the individual plant, the date it was plated and which plate per cross, the date it was initially checked for germination and whether it had germinated on that date, whether or not it received partial embryo-extraction and when, whether it germinated and when, and the date it was planted into a seedling tray if applicable.
* Goffspring.csv Dataset S6: Individual cross identifiers for the created of the F2 generation, when the manual cross took place, which plants the F1 parents were, the date the resulting fruit was harvested and how many seeds it produced.
* greenhouseParents.csv Dataset S7: A list of the F1 parental individuals for F2 offspring plants.
* latency\_females.csv Dataset S11: Data from the behavioral assays with Hyles lineata.
* latency\_males.csv Dataset S12: Data from the behavioral assays with Hyles lineata.
* morph\_data.csv Dataset S8: Floral morphology data for all 205 potential parental individuals from the field populations. 
* planted\_mortality\_F2.csv Dataset S9: Seedling survival data from the F2 generation.
* scent\_data.csv Dataset S10: Floral scent data for all 205 potential parental individuals from the field populations.
* wind\_tunnel.xlsx Dataset S13: Data from the behavioral assays with Hyles lineata.

# Code files
* anosim.R - Analysis of similarities in floral trait data between chemotypes and between populations. 
* betweenPopulations.R - Frequency of between population mating events overall and by chemo- and phenotypes.
* bootstrapping\_frequencies\_figure3.R - Calculates 95% confidence intervals surrounding observed mating frequencies, calculates expected frequencies given germination and survival rates, and produces figure 3.
* distance\_figure5.R - Geographic clustering analysis and figure 5.
* Figure\_S2.R - Hyles behavioral supplement code and figures.
* figure1.R - Code for panel B of figure 1.
* figure2.R - Code for all panels of figure 2.
* floralRegressions\_figure4.R - Analysis of assortative mating by floral morphology and figure 4. 
* floralRewards.R - Mixed effect models of floral rewards (nectar and sucrose).
* germinationF2.R - Germination and seedling survival rates between and within chemotype crosses from F2 greenhouse generation.