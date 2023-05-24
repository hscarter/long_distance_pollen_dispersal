# Stats tests for chemo and phenotype relationships between maternal and paternal plants ####
# Haley Carter

# libraries
library(dplyr)

# data
by_offspring <- read.csv("Data/by_offspring.csv")

by_matLin <- read.csv("Data/by_matTrim.csv")

# binary logistic regression for chemotype
chem <- glm(matchem~patchem, by_offspring, family = "binomial")
summary(chem)

# chi square test for phenotype
phenoChem <- table(by_matLin$chemotype.x, by_matLin$patLin)
chisq.test(phenoChem) # p = 0.1704
phenoPheno <- table(by_matLin$matLin, by_matLin$patLin)
chisq.test(phenoPheno) # p = 0.4233

