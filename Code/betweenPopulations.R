#### Separate Rouse and other populations, look at proportion of lin+ in pops and flow to maternal plants in Rouse ####

# libraries
library(dplyr)

# read in data
by_offspringTrim <- read.csv("Data/by_offspring.csv")
corridor_full <- read.csv("Data/morph_data.csv")

# convert paternal ID to numeric
by_offspringTrim <- by_offspringTrim[!is.na(by_offspringTrim$patchem),]
by_offspringTrim$fatherNumeric <- as.numeric(by_offspringTrim$Candidate.father.ID)
# separate Rouse from others
withinRouse <- by_offspringTrim[which(by_offspringTrim$fatherNumeric >= 11 & by_offspringTrim$fatherNumeric <= 32),]

outsideRouse <- by_offspringTrim[which(by_offspringTrim$fatherNumeric < 11 | by_offspringTrim$fatherNumeric > 31),]

# add paternal site info from corridor data
by_offspring <- left_join(by_offspring, dplyr::select(corridorFull, Name, site), by = c("Candidate.father.ID" = "Name"))
withinRouse2 <- by_offspring[by_offspring$site == "Rouse",] # good they're the same dim

# get mating pairs with pollen from Berwind (actually Berlow)
fromBerwind <- by_offspring[by_offspring$site == "Berwind",] # 24 
table(fromBerwind$matchem) # equal split of matchem 12 and 12
table(fromBerwind$patchem) # 0 17, 1, 5 (2 missing chemotype)

# get mating pairs with pollen from Wals
fromWals <- by_offspring[by_offspring$site == "Walsenburg",] # 12 almost all matchem is linalool

# get mating pairs with pollen from HB
fromHB <- by_offspring[by_offspring$site == "HB",] # 5, all matchem is linalool!

# look at lin+ proportions
table(corridorFull[corridorFull$site == "HB", "chemotype"]) # 77%
table(corridorFull[corridorFull$site == "Walsenburg", "chemotype"]) # 83%
table(corridorFull[corridorFull$site == "Rouse", "chemotype"]) #54%
table(corridorFull[corridorFull$site == "Berwind", "chemotype"]) # 35%
table(fromBerwind$cross)

# set lon to numeric to use to separate Berwind and Ludlow
fromBerwind$PATLONG <- as.numeric(fromBerwind$pat_Lon)

fromBerwind <- fromBerwind[abs(fromBerwind$PATLONG) > 104.5,]

fromLudlow <- fromBerwind[abs(fromBerwind$PATLONG) > 104.55,]

fromBerwind <- fromBerwind[abs(fromBerwind$PATLONG) < 104.55,]

BERLOW <- corridorFull[corridorFull$site == "Berwind",]
BERWIND <- BERLOW[2:36,]
LUDLOW <- BERLOW[37:nrow(BERLOW),]
table(BERWIND$chemotype) #0.31
table(LUDLOW$chemotype) #0.36
table(fromBerwind$matchem)
table(fromLudlow$matchem)
table(fromBerwind$patchem)
table(fromLudlow$patchem)

# calculate mean and se of pollen disp from outside of focal population
mean(outsideRouse$dist)
sd(outsideRouse$dist) / sqrt(length(outsideRouse))
