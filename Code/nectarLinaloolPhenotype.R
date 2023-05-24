# nectar test

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
  mutate(chemotype = recode(linalool_phenotype,
                            "high" = "1",
                            "low" = "1",
                            "none" = "0"))

# plots (figure 4) ####

sucrose<-ggplot(all.floral, aes(chemotype,
                       sucrose.equiv.))+
  geom_boxplot()+
  geom_jitter(color="darkgrey", width=0.3)+
  stat_summary(fun=mean,color="black", shape=21)+
  theme_minimal()+
  labs(x="Linalool chemotype",
       y="Sucrose equivalency")+
  facet_grid(cols=vars(site))
nectar<- ggplot(all.floral, aes(chemotype,
                       nectar.length))+
  geom_boxplot()+
  geom_jitter(color="darkgrey", width=0.3)+
  stat_summary(fun=mean,color="black", shape=21)+
  theme_minimal()+
  labs(x="",
       y="Amount of nectar (mm)")+
  facet_grid(cols=vars(site))
ggarrange(nectar, sucrose, nrow=2)
ggsave("/Users/haley/Documents/GradSchool/Dissertation/Chapter1Mating/Figures/figures/figure4.jpg")
                    
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
