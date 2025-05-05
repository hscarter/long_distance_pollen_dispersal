# Mixed effects models of floral rewards ####

# file paths ####
scent_data <- "Data/scent_data.csv"
morph_data <- "Data/morph_data.csv"

# libraries ####
library(dplyr)
library(ggplot2)
library(lme4)
library(ggpubr)

# data ####
# read in scent data
corridor_data <- read.csv(scent_data)
# read in morphology data
corridor_morph <- read.csv(morph_data)

# add scent data to morphology for all floral data together
chem_cols <- names(corridor_data[c(4:27, 31)])
all.floral <- left_join(select(corridor_morph, Name, site, floral.flare, filiment.length, style.length, tube.length, nectar.length, sucrose.equiv., corolla.mean), select(corridor_data, all_of(chem_cols), Name), by = c("Name" = "Name"))
all.floral <- na.omit(all.floral)
all.floral$site <- as.factor(all.floral$site)
all.floral<- all.floral %>% 
  mutate(chemotype = case_match(linalool_phenotype,
                            "high" ~ "1",
                            "low" ~ "1",
                            "none" ~ "0"))
                    
# mixed effect models ####                   
mixNecLin <- lmer(nectar.length~chemotype + (1 | site), data = all.floral)
summary(mixNecLin)
confint(mixNecLin)
#Lin   2.5%.    97.5%
#- 18.853975  43.676036
#+ 17.313516  53.667327
mixSucLin <- lmer(sucrose.equiv.~chemotype+(1|site), data=all.floral)
summary(mixSucLin)
confint(mixSucLin)
#Lin   2.5%.    97.5%
#- 30.3039557  34.692395
#+ 28.6025102  35.906659
table(all.floral$chemotype)
