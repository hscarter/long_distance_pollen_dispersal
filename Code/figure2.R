# Create alluvial graph with population information (figure 2) ####
# Haley Carter

# libraries
library(tidyverse)
library(ggalluvial)

# data ####
matingPopulation <- read.csv("Data/by_matTrim.csv")

matingPopulation$site <- factor(matingPopulation$site, levels = c("HB", "Walsenburg", "Rouse", "Ludlow", "Berwind"))

# plot ####
popJchemM <- ggplot(matingPopulation, aes(axis1=chemotype.y, axis2 = site, axis3=chemotype.x, fill=site)) +
  geom_alluvium(color="black") +
  scale_fill_manual(values = c("#0072B2","#E69F00", "#F0E442", "lightgrey", "#009E73", "#56B4E9", "grey44"), na.value = "white") +
  geom_stratum(width=c(rep(0.3,2), rep(0.5,5), rep(0.3,2)))+
  scale_x_discrete(limits = c("Paternal
  Chemotype", "Paternal
  Population", "Maternal
  Chemotype"),
                   expand = c(.05, .05)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  labs(fill = "Population") +
  ylab("Number of mating pairs")+
  theme(legend.position="bottom")
popJchemM
