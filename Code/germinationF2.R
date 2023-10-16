# Germination and survival between crosses ####
# Haley Carter
# july 20th 2023

library(tidyverse)
library(ggalluvial)
library(car)

germ_path <- "Data/germinationF2.csv"
lin_path <- "Data/f2_scent_data.csv"
parents_path <- "Data/greenhouseParents.csv"
seedling_path <- "Data/planted_mortality_F2.csv"
seeds_path <- "Data/Goffspring.csv"

# get parents for each cross ID
parent_data <- read_csv(parents_path)
parent_data <- parent_data %>% 
  separate(Sample, into=c("crossID", "indivID")) %>% 
  select(crossID, Maternal, Paternal) %>% 
  unique()

# add linalool data to parent data
linalool_data <- read_csv(lin_path)
parent_data <- parent_data %>% 
  left_join(linalool_data, by = c("Maternal"="sampleName")) %>% 
  left_join(linalool_data, by = c("Paternal"="sampleName")) %>%
  rename(matEmit = sampleConc.x.x, patEmit = sampleConc.x.y) %>%
  mutate(matchem = case_when(matEmit > 0 ~ 1, matEmit == 0 ~ 0), patchem = case_when(patEmit > 0 ~ 1, patEmit == 0 ~ 0), crosschem = paste(matchem, patchem, sep = "x")) 

# seed set ####
seeds <- read_csv(seeds_path)
seeds <- seeds %>% 
  mutate(ID = str_replace(ID, "G", ""),
         ID = str_pad(ID, width = 2, side="left", pad="0"),
         ID = paste("G", ID, sep = "")) %>% 
  left_join(select(parent_data, crossID, matchem, patchem, crosschem), by = c("ID" = "crossID"))

seeds_complete <- seeds %>% 
  filter(!str_detect(crosschem, "NA"))

# visualize seed data ####
ggplot(seeds_complete, aes(crosschem, num_seeds)) +
  geom_boxplot()+
  labs(x = "Maternal chemotype x pollen donor chemotype",
       y = "Number of seeds")+
  theme_minimal()

ggplot(seeds_complete, aes(crosschem, num_seeds)) +
  geom_violin()

ggplot(seeds_complete, aes(num_seeds))+
  geom_histogram(bins = 40)+
  facet_grid(rows = vars(crosschem))+
  theme_bw()

hist(seeds_complete$num_seeds)

# model seed set by cross type ####
seedMod <- glm(num_seeds~crosschem, family = "gaussian", data = seeds_complete)
summary(seedMod)
Anova(seedMod) # chi2 = 0.86, df = 3, p = 0.84

par(mfrow = c(2,2))
plot(seedMod) # decent
par(mfrow = c(1,1))

# germination data ####
germ_data <- read_csv(germ_path)
# add parental chemistry
germ_data <- germ_data %>% 
  left_join(select(parent_data, crossID, matchem, patchem, crosschem), by = c("cross_id" = "crossID"))
# create germinated and planted columns
germ_data <- germ_data %>% 
  mutate(germinated = case_when(
    date_germ == "lost" ~ 0,
    date_germ == "mold" ~ 0,
    date_germ == "rotten" ~ 0,
    is.na(date_germ) ~ 0,
    .default = 1
  ), planted = case_when(
    date_plant == "mold" ~ 0,
    date_plant == "rotten" ~ 0,
    is.na(date_plant) ~ 0,
    .default = 1
  ))

germ_data <- germ_data %>% 
  mutate(seedID = paste(cross_id, indiv_id, sep = "_")) %>% 
  left_join(linalool_data, by = c("seedID" = "sampleName")) %>% 
  mutate(flowered = case_when(is.na(sampleConc.x) ~ 0,
                              !is.na(sampleConc.x) ~ 1))

germ_data$embryo_extracted <- factor(germ_data$embryo_extracted)
germ_data$germinated <- factor(germ_data$germinated)
germ_data$planted <- factor(germ_data$planted)
germ_data$matchem <- factor(germ_data$matchem)
germ_data$patchem <- factor(germ_data$patchem)
germ_data$flowered <- factor(germ_data$flowered)

# remove where parental chemotype is missing and where embryo_extracted is NA
germ_data_complete <- germ_data %>% 
  filter(!is.na(matchem), !is.na(patchem), !is.na(embryo_extracted), !str_detect(crosschem, "NA")) 

# germ rates ####
germ_data_complete %>% 
  mutate(germ_numeric = as.numeric(germinated)-1) %>% 
  group_by(embryo_extracted, crosschem) %>% 
  summarize(mean_germ = mean(germ_numeric, na.rm = T),
            sd_germ = sd(germ_numeric, na.rm = T),
            se_germ = sd_germ/n())

# germination by mat and pat chem barplots ####
# count
ggplot(germ_data_complete, aes(matchem, germinated)) +
  geom_col(aes(fill=germinated)) +
  facet_grid(cols = vars(embryo_extracted), rows = vars(patchem))
# proportion
ggplot(germ_data_complete, aes(matchem)) +
  geom_bar(aes(fill=germinated), position = "fill") +
  facet_grid(cols = vars(embryo_extracted), rows = vars(patchem))+
  theme_minimal()+
  theme(legend.position = "bottom")
# grouped differently
ggplot(germ_data_complete, aes(crosschem)) +
  geom_bar(aes(fill = germinated), position = "fill") +
  facet_grid(cols = vars(embryo_extracted)) +
  theme_minimal()+
  theme(legend.position = "bottom")

# germination modeled by embryo_extract*crosstype ####
germModint <- glm(germinated~embryo_extracted*crosschem, family = "binomial", data = germ_data_complete)
summary(germModint)
Anova(germModint, 3) # keep interaction

par(mfrow = c(2,2))
plot(germModint)
par(mfrow = c(1,1))

odds_ratios_cross <- exp(germModint$coefficients)
odds_ratios_cross

# embryo extraction 26 times more likely to germinate
# as compared to lin- X lin - without embryo extraction
# lin- x lin+ 2.3 times more likely to germ
# lin+ x lin- 1.70 times more likely to germ
# lin+ x lin+ 2.13 times more likely to germ

# with embryo extraction
# lin- x lin+ 1.0008 (non sig)
# lin+ x lin- 7.17 
# lin+ x lin+ 0.67 (non sig)

germMod2 <- glm(germinated~embryo_extracted*matchem*patchem, family = "binomial", data = germ_data_complete)
summary(germMod2)
Anova(germMod2, 3)

# embryo ext increase germ
# linalool production in maternal increase germ
# linalool production in pollen donor increase germ
# interaction between embryo ext and mat lin increase germ
# interaction between mat and pat lin decrease germ (marginal)
# interaction between all three decrease germ

# as the expected change in the log of the odds of the response occurring corresponding to a 1-unit increase in the predictor holding constant all other predictors in the model. Log odds ratios are typically challenging to interpret, so sometimes people exponentiate the coefficients, which yields odds ratios.

odds_ratios <- exp(germMod2$coefficients)
odds_ratios

# odds of germinated 26 times higher with partial embryo extraction
# 1.7 times higher with maternal linalool production
# 2.3 times higher with paternal linalool production
# 7.17 times higher with ext and mat lin, but 1.0008 ext and pat lin
# with mat and pat lin + interaction 0.54 (half as likely)
# interaction ext and mat and pat lin 0.09

germ_data_complete %>% 
  mutate(germ_numeric = as.numeric(germinated)-1) %>% 
  group_by(embryo_extracted, crosschem) %>% 
  summarize(mean_germ = mean(germ_numeric, na.rm = T),
            sd_germ = sd(germ_numeric, na.rm = T),
            se_germ = sd_germ/n())


# seedling data ####
seedling <- read_csv(seedling_path)
# calculated prop survived per plate per cross
seedling_survival <- seedling %>% 
  mutate(survived = case_when(planted == "yes" ~ 1,
                             planted == "no" ~ 0)) %>% 
  group_by(cross_id, plate) %>% 
  summarise(n = n(),
            n_survive = sum(survived),
            prop_survive = n_survive/n)
# all cross type info
seedling_survival <- seedling_survival %>% 
  left_join(select(parent_data, crossID, matchem, patchem, crosschem), by = c("cross_id" = "crossID"))
seedling_survival_complete <- seedling_survival %>% filter(!str_detect(crosschem, "NA"))

# need embryo extraction as well... 
seedling$seedID <- paste(seedling$cross_id, seedling$indiv_id, sep = "_")
seedling_embryo <- seedling %>% 
  left_join(select(germ_data, seedID, embryo_extracted), by = "seedID") %>% 
  left_join(select(parent_data, crossID, matchem, patchem, crosschem), by = c("cross_id" = "crossID"))
seedling_embryo_complete <- seedling_embryo %>% filter(!str_detect(crosschem, "NA"))

seedling_embryo_complete$planted <- factor(seedling_embryo_complete$planted)

table(seedling_embryo_complete$planted)

# survival rates ####
seedling_embryo_complete %>% 
  mutate(survive = case_when(planted == "yes" ~ 1,
                             planted == "no" ~ 0)) %>% 
  group_by(crosschem) %>% 
  summarize(mean_survive = mean(survive, na.rm = T),
            sd_survive = sd(survive, na.rm = T),
            se_survive = sd_survive/n())

# seedling survival model ####
bySeedMod <- glm(planted~embryo_extracted*crosschem, family = "binomial", data = seedling_embryo_complete)
Anova(bySeedMod, 3) # only cross chem sig
summary(bySeedMod)

seedModcross <- glm(planted~crosschem, family = "binomial", data = seedling_embryo_complete)
Anova(seedModcross)
summary(seedModcross)

odds_ratios_seedling <- exp(seedModcross$coefficients)
odds_ratios_seedling
# only sig is crosschem1x0 0.1749271 

# plots of survival by cross type ####
ggplot(seedling_embryo_complete, aes(crosschem)) +
  geom_bar(aes(fill = planted), position = "fill")+
  theme_minimal()

ggplot(seedling_survival_complete, aes(crosschem, prop_survive))+
  geom_boxplot()

ggplot(seedling_survival_complete, aes(crosschem, prop_survive))+
  geom_violin(fill = "lightgrey")+
  theme_minimal()

