#### Mating by chemotype ####
# Haley Carter

# data
by_maternal <- read.csv("Data/by_maternal.csv", header = TRUE)
by_matTrim <- by_maternal[c(1:11, 13:48, 50:70), ] #remove NAs
by_matRouse <- read.csv("Data/by_maternalRouse.csv")

# Pollen movement to maternal lines frequency and confidence intervals via bootstrapping ####
Lml <- by_matTrim[by_matTrim$chemotype.y == "1", 'chemotype.x']
Lml <- unlist(Lml, use.names = F)
Lml <- as.numeric(as.character(Lml))
mean(Lml) #0.744
1-mean(Lml) #0.256
Nml <- by_matTrim[by_matTrim$chemotype.y == "0", 'chemotype.x']
Nml <- unlist(Nml, use.names = F)
Nml <- as.numeric(as.character(Nml))
mean(Nml) #0.56
1-mean(Nml) #0.44

bootmeanLml <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLml[i] <- mean(sample(Lml, length(Lml), replace = TRUE))
}
ciLml <- quantile(bootmeanLml, probs = c(0.025, 0.975))
ciLml
# 0.604 0.860

bootmeanNml <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNml[i] <- mean(sample(Nml, length(Nml), replace = TRUE))
}
ciNml <- quantile(bootmeanNml, probs = c(0.025, 0.975))
ciNml
# 0.36 0.76 - huge range

bootmeanNNml <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNNml[i] <- 1 - mean(sample(Nml, length(Nml), replace = TRUE))
}
ciNNml <- quantile(bootmeanNNml, probs = c(0.025, 0.975))
ciNNml
# 0.24 0.64

bootmeanLNml <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLNml[i] <- 1 - mean(sample(Lml, length(Lml), replace = TRUE))
}
ciLNml <- quantile(bootmeanLNml, probs = c(0.025, 0.975))
ciLNml
# 0.14 0.395

# test with binary logit regression ####
matNull <- glm(chemotype.x~1, by_matTrim, family = binomial)
matAlt <- glm(chemotype.x~chemotype.y, by_matTrim, family = binomial)
summary(matAlt) #p = 0.121
anova(matNull, matAlt, test = "Chisq") #p=0.1203

# within Rouse bootstrapping ####
LmlR <- by_matRouse[by_matRouse$chemotype.y == "1", 'chemotype.x']
LmlR <- unlist(LmlR, use.names = F)
LmlR <- as.numeric(as.character(LmlR))
mean(LmlR) #0.70833
1-mean(LmlR) #0.2917
NmlR <- by_matRouse[by_matRouse$chemotype.y == "0", 'chemotype.x']
NmlR <- unlist(NmlR, use.names = F)
NmlR <- as.numeric(as.character(NmlR))
mean(NmlR) #0.5833
1-mean(NmlR) #0.4167

bootmeanLmlR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLmlR[i] <- mean(sample(LmlR, length(LmlR), replace = TRUE))
}
ciLmlR <- quantile(bootmeanLmlR, probs = c(0.025, 0.975))
ciLmlR
# 0.500 0.875

bootmeanNmlR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNmlR[i] <- mean(sample(NmlR, length(NmlR), replace = TRUE))
}
ciNmlR <- quantile(bootmeanNmlR, probs = c(0.025, 0.975))
ciNmlR
# 0.3333 0.83333 - huge range

bootmeanNNmlR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNNmlR[i] <- 1 - mean(sample(NmlR, length(NmlR), replace = TRUE))
}
ciNNmlR <- quantile(bootmeanNNmlR, probs = c(0.025, 0.975))
ciNNmlR
# 0.1667 0.66667

bootmeanLNmlR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLNmlR[i] <- 1 - mean(sample(LmlR, length(LmlR), replace = TRUE))
}
ciLNmlR <- quantile(bootmeanLNmlR, probs = c(0.025, 0.975))
ciLNmlR
# 0.125 0.500

# test with binary logit regression Rouse ####
matNullR <- glm(chemotype.x~1, by_matRouse, family = binomial)
matAltR <- glm(chemotype.x~chemotype.y, by_matRouse, family = binomial)
summary(matAltR) #p = 0.455
anova(matNullR, matAltR, test = "Chisq") #p=0.4568

#### proportion received by maternal plants ####
Lm <- by_matTrim[by_matTrim$chemotype.x == "1", 'chemotype.y']
Lm <- unlist(Lm, use.names = F)
Lm <- as.numeric(as.character(Lm))
mean(Lm) #0.70
1-mean(Lm) #0.30
Nm <- by_matTrim[by_matTrim$chemotype.x == "0", 'chemotype.y']
Nm <- unlist(Nm, use.names = F)
Nm <- as.numeric(as.character(Nm))
mean(Nm) #0.5
1-mean(Nm) #0.5

bootmeanLm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLm[i] <- mean(sample(Lm, length(Lm), replace = TRUE))
}
ciLm <- quantile(bootmeanLm, probs = c(0.025, 0.975))
ciLm
# 0.5652 0.826

bootmeanLNm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLNm[i] <- 1 - mean(sample(Lm, length(Lm), replace = TRUE))
}
ciLNm <- quantile(bootmeanLNm, probs = c(0.025, 0.975))
ciLNm
# 0.1739 0.4348

bootmeanNm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNm[i] <- mean(sample(Nm, length(Nm), replace = TRUE))
}
ciNm <- quantile(bootmeanNm, probs = c(0.025, 0.975))
ciNm
# 0.27273 0.727272 - huge range

bootmeanNNm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNNm[i] <- 1 - mean(sample(Nm, length(Nm), replace = TRUE))
}
ciNNm <- quantile(bootmeanNNm, probs = c(0.025, 0.975))
ciNNm
# same as above
