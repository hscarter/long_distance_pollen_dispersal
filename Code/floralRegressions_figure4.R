# Floral traits ####
# Haley Carter

# file paths ####
by_off_path <- "Data/by_offspring.csv"
scent_data <- "Data/scent_data.csv"
morph_data <- "Data/morph_data.csv"

# libraries ####
library(tidyverse)
library(rstatix)
library(patchwork)
library(ggpubr)

# data ####
by_off <- read.csv(by_off_path)
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

all.floral$site <- factor(all.floral$site, levels = c("HB", "Walsenburg", "Rouse", "Ludlow", "Berwind"))

#######################################################
# reproductive isolation ####
by_off_complete <- by_off %>% 
  filter(!str_detect(cross, "NA"))

by_off_complete %>%
  group_by(cross) %>% 
  summarise(n=n())

# 0X0      19
# 0X1      15
# 1X0      24
# 1X1      45

RIm = 19/(15+24+19)
RIp = 45/(15+24+45)
RIm # 0.3275862
RIp # 0.5357143

total = 19+15+24+45
45/total # 43.7% of offspring had both lin+ parents
any_p = 15+24+45
any_m = 19+15+24
any_p/total # 81.6% of offspring had at least one lin+ parent
any_m/total # 56.3% of offspring had at least one lin- parent

#####################################################

# add herkogamy to all floral, and offspring data
all.floral$herkogamy <- all.floral$style.length - (all.floral$tube.length + all.floral$filiment.length)

by_off$mat_herkogamy <- by_off$mat_style.length - (by_off$mat_tube.length + by_off$mat_filiment.length)

by_off$pat_herkogamy <- by_off$pat_style.length - (by_off$pat_tube.length + by_off$pat_filiment.length)

# it doesn't make sense that floral tubes would be longer than styles. going to disregard those rows for morphology questions, probably measured where ovary starts incorrectly or entered data wrong

all.floral <- all.floral %>%
  filter(style.length > tube.length)

by_off <- by_off %>% 
  filter(mat_style.length > mat_tube.length & pat_style.length > pat_tube.length)

by_off_complete <- by_off %>% 
  filter(!str_detect(cross, "NA"))

#####################################################
# t tests by site #### 
# tube length
all.floral %>% 
  group_by(site) %>% 
  t_test(tube.length~chemotype) # ludlow
# corolla width
all.floral %>% 
  group_by(site) %>% 
  t_test(corolla.mean~chemotype) # rouse
# filament length
all.floral %>% 
  group_by(site) %>% 
  t_test(filiment.length~chemotype) # rouse
# nectar amount
all.floral %>% 
  group_by(site) %>% 
  t_test(nectar.length~chemotype) # no sig
# sucrose amount
all.floral %>% 
  group_by(site) %>% 
  t_test(sucrose.equiv.~chemotype) # no sig
# floral flare
all.floral %>% 
  group_by(site) %>% 
  t_test(floral.flare~chemotype) # rouse, marginally sig
# style length
all.floral %>% 
  group_by(site) %>% 
  t_test(style.length~chemotype) # no sig
# herkogamy
all.floral %>% 
  group_by(site) %>% 
  t_test(herkogamy~chemotype) # walsenburg

# t tests within maternal plants ####
# corolla width
by_off_complete %>% 
  t_test(mat_corolla.mean~matchem) # sig
# tube length
by_off_complete %>% 
  t_test(mat_tube.length~matchem)
# filament length
by_off_complete %>% 
  t_test(mat_filiment.length~matchem) # sig
# nectar amount
by_off_complete %>% 
  t_test(mat_nectar.length~matchem) # sig
# sucrose amount
by_off_complete %>% 
  t_test(mat_sucrose.equiv.~matchem) 
# floral flare
by_off_complete %>% 
  t_test(mat_floral.flare~matchem) # sig
# style length
by_off_complete %>% 
  t_test(mat_style.length~matchem) # sig
# herkogamy
by_off_complete %>% 
  t_test(mat_herkogamy~matchem) # sig

# t tests within paternal plants ####
# corolla width
by_off_complete %>% 
  t_test(pat_corolla.mean~patchem) 
# tube length
by_off_complete %>% 
  t_test(pat_tube.length~patchem)
# filament length
by_off_complete %>% 
  t_test(pat_filiment.length~patchem)
# nectar amount
by_off_complete %>% 
  t_test(pat_nectar.length~patchem)
# sucrose amount
by_off_complete %>% 
  t_test(pat_sucrose.equiv.~patchem) 
# floral flare
by_off_complete %>% 
  t_test(pat_floral.flare~patchem) # sig
# style length
by_off_complete %>% 
  t_test(pat_style.length~patchem)
# herkogamy
by_off_complete %>% 
  t_test(pat_herkogamy~patchem)

# t test pats within Rouse ####
rouseIDs <- all.floral[all.floral$site == "Rouse", 'Name']
# corolla width
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_corolla.mean~patchem)
# tube length
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_tube.length~patchem)
# filament length
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_filiment.length~patchem)
# nectar amount
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_nectar.length~patchem)
# sucrose amount
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_sucrose.equiv.~patchem)
# floral flare
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_floral.flare~patchem)
# style length
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_style.length~patchem)
# herkogamy
by_off_complete %>% 
  filter(Candidate.father.ID %in% rouseIDs) %>% 
  t_test(pat_herkogamy~patchem)

#######################################################
# how much larger? summmary tables ####
all.floral %>% #these are the traits that were diff within Rouse
  filter(site == "Rouse") %>% 
  group_by(chemotype) %>% 
  summarize(mean_corolla = mean(corolla.mean),
            se_corolla = sd(corolla.mean)/sqrt(n()),
            mean_filament = mean(filiment.length),
            se_filament = sd(filiment.length)/sqrt(n()),
            mean_flare = mean(floral.flare),
            se_flare = sd(floral.flare)/sqrt(n()))

all.floral %>% 
  filter(site == "Rouse") %>% 
  group_by(chemotype) %>% 
  summarize(mean_style = mean(style.length),
            se_style = sd(style.length)/sqrt(n()),
            mean_nectar = mean(nectar.length),
            se_nectar = sd(nectar.length)/sqrt(n()),
            mean_sucrose = mean(sucrose.equiv.),
            se_sucrose = sd(sucrose.equiv.)/sqrt(n()),
            mean_tube = mean(tube.length),
            se_tube = sd(tube.length)/sqrt(n()))

by_off_complete %>% 
  group_by(matchem) %>% 
  summarize(mean_corolla = mean(mat_corolla.mean),
            se_corolla = sd(mat_corolla.mean)/sqrt(n()),
            mean_filament = mean(mat_filiment.length),
            se_filament = sd(mat_filiment.length)/sqrt(n()),
            mean_flare = mean(mat_floral.flare),
            se_flare = sd(mat_floral.flare)/sqrt(n()))

by_off_complete %>% 
  group_by(matchem) %>% 
  summarize(mean_style = mean(mat_style.length),
            se_style = sd(mat_style.length)/sqrt(n()),
            mean_nectar = mean(mat_nectar.length),
            se_nectar = sd(mat_nectar.length)/sqrt(n()),
            mean_sucrose = mean(mat_sucrose.equiv.),
            se_sucrose = sd(mat_sucrose.equiv.)/sqrt(n()),
            mean_tube = mean(mat_tube.length),
            se_tube = sd(mat_tube.length)/sqrt(n()))

########################################################
# pivot all floral data longer to plot ####
# (should have done this first to make t tests like three lines)
all.floral.long <- pivot_longer(all.floral,
                                cols = c(floral.flare,
                                         filiment.length,
                                         style.length,
                                         tube.length,
                                         nectar.length,
                                         sucrose.equiv.,
                                         corolla.mean,
                                         herkogamy),
                                values_to = "value",
                                names_to = "measurement") %>% 
  mutate(chemotype = case_when(chemotype == 0 ~ "lin-",
                               chemotype == 1 ~ "lin+"),
         label = case_when(measurement == "corolla.mean" ~ "Corolla",
                           measurement == "filiment.length" ~ "Filament",
                           measurement == "floral.flare" ~ "Flare",
                           measurement == "herkogamy" ~ "Herkogamy",
                           measurement == "nectar.length" ~ "Nectar",
                           measurement == "style.length" ~ "Style",
                           measurement == "sucrose.equiv." ~ "Sucrose",
                           measurement == "tube.length" ~ "Tube"))

# pivot by off longer by maternal traits to plot
by_off_long_mat <- pivot_longer(by_off_complete,
                                cols = c(mat_floral.flare,
                                         mat_filiment.length,
                                         mat_style.length,
                                         mat_tube.length,
                                         mat_nectar.length,
                                         mat_sucrose.equiv.,
                                         mat_corolla.mean,
                                         mat_herkogamy),
                                values_to = "mat_value",
                                names_to = "mat_measurement") %>% 
  mutate(matchem = case_when(matchem == 0 ~ "lin-",
                             matchem == 1 ~ "lin+"),
         label = case_when(mat_measurement == "mat_corolla.mean" ~ "Corolla",
                           mat_measurement == "mat_filiment.length" ~ "Filament",
                           mat_measurement == "mat_floral.flare" ~ "Flare",
                           mat_measurement == "mat_herkogamy" ~ "Herkogamy",
                           mat_measurement == "mat_nectar.length" ~ "Nectar",
                           mat_measurement == "mat_style.length" ~ "Style",
                           mat_measurement == "mat_sucrose.equiv." ~ "Sucrose",
                           mat_measurement == "mat_tube.length" ~ "Tube"))

# pivot by_off longer by paternal
by_off_long_pat <- pivot_longer(by_off_complete,
                                cols = c(pat_floral.flare,
                                         pat_filiment.length,
                                         pat_style.length,
                                         pat_tube.length,
                                         pat_nectar.length,
                                         pat_sucrose.equiv.,
                                         pat_corolla.mean,
                                         pat_herkogamy),
                                values_to = "pat_value",
                                names_to = "pat_measurement") %>% 
  mutate(patchem = case_when(patchem == 0 ~ "lin-",
                             patchem == 1 ~ "lin+"),
         label = case_when(pat_measurement == "pat_corolla.mean" ~ "Corolla",
                           pat_measurement == "pat_filiment.length" ~ "Filament",
                           pat_measurement == "pat_floral.flare" ~ "Flare",
                           pat_measurement == "pat_herkogamy" ~ "Herkogamy",
                           pat_measurement == "pat_nectar.length" ~ "Nectar",
                           pat_measurement == "pat_style.length" ~ "Style",
                           pat_measurement == "pat_sucrose.equiv." ~ "Sucrose",
                           pat_measurement == "pat_tube.length" ~ "Tube"))

#######################################################
# boxplots ####
# Rouse
rouseboxes<-ggplot(all.floral.long[all.floral.long$site == "Rouse",], aes(chemotype, value))+
  geom_boxplot()+
  geom_jitter(color="darkgrey", shape=21, width=0.3)+
  stat_summary(fun="mean", color = "black", shape = 21)+
  facet_wrap(vars(label), nrow = 1,  scales = "free")+
  theme_minimal()+
  theme(axis.title = element_blank())
# all plants across populations
allboxes<-ggplot(all.floral.long, aes(chemotype, value))+
  geom_boxplot()+
  geom_jitter(color="darkgrey", shape=21, width=0.3, alpha =0.6)+
  stat_summary(fun="mean", color = "black", shape = 21)+
  facet_wrap(vars(label), nrow = 1,  scales = "free")+
  theme_minimal()+
  theme(axis.title = element_blank())
# maternal plants
matboxes<-ggplot(by_off_long_mat, aes(matchem, mat_value))+
  geom_boxplot()+
  geom_jitter(color="darkgrey", shape=21, width=0.3, alpha =0.7)+
  stat_summary(fun="mean", color = "black", shape = 21)+
  facet_wrap(vars(label), nrow = 1,  scales = "free")+
  theme_minimal()+
  theme(axis.title = element_blank())
# paternal plants (not included in final figure, too much to look at)
patboxes<-ggplot(by_off_long_pat, aes(patchem, pat_value))+
  geom_boxplot()+
  geom_jitter(color="darkgrey", shape=21, width=0.3, alpha =0.7)+
  stat_summary(fun="mean", color = "black", shape = 21)+
  facet_wrap(vars(label), nrow = 1,  scales = "free")+
  theme_minimal()+
  theme(axis.title = element_blank())

#ggarrange(allboxes, rouseboxes, matboxes, patboxes, nrow = 4, labels = c("A", "B", "C", "D"))

ggarrange(allboxes, rouseboxes, matboxes, nrow = 3, labels = c("A", "B", "C"))

########################################################
# paternal by maternal regression ####
# linear models for each trait
corollaM <- lm(pat_corolla.mean~mat_corolla.mean, by_off)
flareM <- lm(pat_floral.flare~mat_floral.flare, by_off)
filimentM <- lm(pat_filiment.length~mat_filiment.length, by_off)
styleM <- lm(pat_style.length~mat_style.length, by_off)
tubeM <- lm(pat_tube.length~mat_tube.length, by_off)
nectarM <- lm(pat_nectar.length~mat_nectar.length, by_off)
sucroseM <- lm(pat_sucrose.equiv.~mat_sucrose.equiv., by_off)
herkogamyM <- lm(pat_herkogamy~mat_herkogamy, by_off)
# check summaries
summary(corollaM) # not sig
summary(flareM) # not sig
summary(filimentM) # not sig
summary(styleM) # not sig
summary(tubeM) # not sig
summary(nectarM) # not sig
summary(sucroseM) # not sig
summary(herkogamyM) # not sig

corollaChemM <- lm(pat_corolla.mean~mat_corolla.mean*matchem, by_off)
flareChemM <- lm(pat_floral.flare~mat_floral.flare*matchem, by_off)
filimentChemM <- lm(pat_filiment.length~mat_filiment.length*matchem, by_off)
styleChemM <- lm(pat_style.length~mat_style.length*matchem, by_off)
tubeChemM <- lm(pat_tube.length~mat_tube.length*matchem, by_off)
nectarChemM <- lm(pat_nectar.length~mat_nectar.length*matchem, by_off)
sucroseChemM <- lm(pat_sucrose.equiv.~mat_sucrose.equiv.*matchem, by_off)
herkChemM <- lm(pat_herkogamy~mat_herkogamy*matchem, by_off)

summary(corollaChemM) # all sig
summary(flareChemM) # all sig
summary(filimentChemM)
summary(styleChemM) # style and matchem sig
summary(tubeChemM) 
summary(nectarChemM) 
summary(sucroseChemM) 
summary(herkChemM) # only herkogamy sig

corollaChemMP <- lm(pat_corolla.mean~mat_corolla.mean*matchem*patchem, by_off)
flareChemMP <- lm(pat_floral.flare~mat_floral.flare*matchem*patchem, by_off)
filimentChemMP <- lm(pat_filiment.length~mat_filiment.length*matchem*patchem, by_off)
styleChemMP <- lm(pat_style.length~mat_style.length*matchem*patchem, by_off)
tubeChemMP <- lm(pat_tube.length~mat_tube.length*matchem*patchem, by_off)
nectarChemMP <- lm(pat_nectar.length~mat_nectar.length*matchem*patchem, by_off)
sucroseChemMP <- lm(pat_sucrose.equiv.~mat_sucrose.equiv.*matchem*patchem, by_off)
herkChemMP <- lm(pat_herkogamy~mat_herkogamy*matchem*patchem, by_off)

summary(corollaChemMP) # matchem and mat interaction sig
summary(flareChemMP)
summary(filimentChemMP) # all sig
summary(styleChemMP) # style sig
summary(tubeChemMP) 
summary(nectarChemMP) 
summary(sucroseChemMP) # three way interaction sig
summary(herkChemMP) # mat herk, matchem, interactions with patchem

# distributions
hist(by_off$pat_corolla.mean)
hist(by_off$pat_floral.flare)
hist(by_off$pat_filiment.length)
hist(by_off$pat_tube.length)
hist(by_off$pat_nectar.length)
hist(by_off$pat_sucrose.equiv.) 

# model fit
par(mfrow = c(2,2))
plot(corolla)
plot(flare)
plot(filiment)
plot(style)
plot(tube)
plot(nectar)
plot(sucrose)
par(mfrow = c(1,1)) # they don't look amazing but many are better than expected

# plot ####
par(mfrow = c(3, 3))
plot(mat_corolla.mean~pat_corolla.mean, by_off)
plot(mat_floral.flare~pat_floral.flare, by_off)
plot(mat_filiment.length~pat_filiment.length, by_off)
plot(mat_style.length~pat_style.length, by_off)
plot(mat_tube.length~pat_tube.length, by_off)
abline(tube)
plot(mat_nectar.length~pat_nectar.length, by_off)
plot(mat_sucrose.equiv.~pat_sucrose.equiv., by_off)
par(mfrow = c(1,1))

# with cross as covariate #### 
corolla_cross <- lm(mat_corolla.mean~pat_corolla.mean*cross, by_off_complete)
summary(corolla_cross)

flare_cross <- lm(mat_floral.flare~pat_floral.flare*cross, by_off_complete)
summary(flare_cross)

filiment_cross <- lm(mat_filiment.length~pat_filiment.length*cross, by_off_complete)
summary(filiment_cross) # lots of sig there now

style_cross <- lm(mat_style.length~pat_style.length*cross, by_off_complete)
summary(style_cross)

tube_cross <- lm(mat_tube.length~pat_tube.length*cross, by_off_complete)
summary(tube_cross) # no sig now

nectar_cross <- lm(mat_nectar.length~pat_nectar.length*cross, by_off_complete)
summary(nectar_cross)

sucrose_cross <- lm(mat_sucrose.equiv.~pat_sucrose.equiv.*cross, by_off_complete)
summary(sucrose_cross) # one cross type

herkogamy_cross <- lm(mat_herkogamy~pat_herkogamy*cross, by_off_complete)
summary(herkogamy_cross)

# with cross as covariate, modeling pat traits ####


ggplot(by_off_complete, aes(pat_filiment.length, mat_filiment.length, color = cross))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_wrap(vars(cross))

ggplot(by_off_complete, aes(pat_sucrose.equiv., mat_sucrose.equiv., color = cross))+
  geom_point()+
  #geom_smooth(method = "lm")+
  theme_minimal()+
  facet_wrap(vars(cross))

# models ####
hist(all.floral$corolla.mean)
corolla_mod <- glm(corolla.mean~chemotype*site, data=all.floral)
summary(corolla_mod) # only wals, no chem

ggplot(all.floral, aes(chemotype, corolla.mean))+
  geom_boxplot()+
  facet_wrap(vars(site))

hist(all.floral$tube.length)
tube_mod <- glm(tube.length~chemotype*site, data=all.floral)
summary(tube_mod) # no sig

hist(all.floral$nectar.length) # less norm
hist(sqrt(all.floral$nectar.length)) # better
nectar_mod <- glm(nectar.length~chemotype*site, data=all.floral)
summary(nectar_mod) # rouse, berwind marg, no chem
nectar_modSQ <- glm(sqrt(nectar.length)~chemotype*site, data = all.floral)
summary(nectar_modSQ) # rouse (stronger), ludlow marg, berwind sig, no chem

hist(all.floral$sucrose.equiv.) # less norm
hist(sqrt(all.floral$sucrose.equiv.)) # not much better
sucrose_mod <- glm(sucrose.equiv.~chemotype*site, data = all.floral)
summary(sucrose_mod) # no sig
sucrose_modSQ <- glm(sqrt(sucrose.equiv.)~chemotype*site, data = all.floral)
summary(sucrose_modSQ) # no sig

hist(all.floral$filiment.length)
filiment_mod <- glm(filiment.length~chemotype*site, data=all.floral)
summary(filiment_mod) # no sig

hist(all.floral$floral.flare)
flare_mod <- glm(floral.flare~chemotype*site, data=all.floral)
summary(flare_mod) # no sig

hist(all.floral$style.length)
style_mod <- glm(style.length~chemotype*site, data=all.floral)
summary(style_mod) # no sig


# histograms

ggplot()

ggplot(all.floral[all.floral$site == "Rouse",], aes(style.length))+
  geom_histogram(bins = 10)+
  facet_grid(rows = vars(chemotype))+
  theme_minimal()


ggplot(all.floral[all.floral$site == "Rouse",], aes(style.length, color = chemotype))+
  geom_density()+
  geom_vline(xintercept = mean())
  theme_minimal()


