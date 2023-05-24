# Floral traits regressions ####
# Haley Carter

# data ####
by_off <- read.csv("Data/by_offspring.csv")

# linear model of maternal traits by paternal traits ####
corolla <- lm(mat_corolla.mean~pat_corolla.mean, by_off)
flare <- lm(mat_floral.flare~pat_floral.flare, by_off)
filiment <- lm(mat_filiment.length~pat_filiment.length, by_off)
style <- lm(mat_style.length~pat_style.length, by_off)
tube <- lm(mat_tube.length~pat_tube.length, by_off)
nectar <- lm(mat_nectar.length~pat_nectar.length, by_off)
sucrose <- lm(mat_sucrose.equiv.~pat_sucrose.equiv., by_off)

summary(corolla) # p = 0.92
summary(flare) # p = 0.0979
summary(filiment) # p = 0.974
summary(style) # p = 0.771
summary(tube) # p = 0.0306, multiple R^2 = 0.0446, negative rel.
summary(nectar) # p = 0.595
summary(sucrose) # p = 0.669

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
