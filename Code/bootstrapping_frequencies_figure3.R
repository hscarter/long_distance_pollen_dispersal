#### Mating by chemotype ####
# Haley Carter

# libraries
library(tidyverse)

# data
by_maternal <- read.csv("Data/by_maternal.csv", header = TRUE)
by_matTrim <- by_maternal[c(1:11, 13:48, 50:70), ] #remove NAs
corridor <- read.csv("Data/morph_data.csv")
rouseIDs <- corridor %>% 
  filter(site == "Rouse") %>% 
  select(Name)
by_matRouse <- by_matTrim %>% 
  filter(Candidate.father.ID %in% rouseIDs$Name)
by_off <- read.csv("Data/by_offspring.csv")

by_off <-  by_off %>% 
  filter(!is.na(patchem))
by_offR <- by_off %>% 
  filter(Candidate.father.ID %in% by_matRouse$Candidate.father.ID)

#### proportion received by maternal plants ####
Lm <- by_matTrim[by_matTrim$chemotype.x == "1", 'chemotype.y']
Lm <- unlist(Lm, use.names = F)
Lm <- as.numeric(as.character(Lm))
mean(Lm) #0.70 lin+ x lin+
1-mean(Lm) #0.30 lin+ x lin-
Nm <- by_matTrim[by_matTrim$chemotype.x == "0", 'chemotype.y']
Nm <- unlist(Nm, use.names = F)
Nm <- as.numeric(as.character(Nm))
mean(Nm) #0.5 lin- x lin+
1-mean(Nm) #0.5 lin- x lin-

# bootstrap confidence intervals for lin+ x lin+
bootmeanLm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLm[i] <- mean(sample(Lm, length(Lm), replace = TRUE))
}
ciLm <- quantile(bootmeanLm, probs = c(0.025, 0.975))
ciLm
# 0.5652 0.826

bootmeanLm10 <- vector(length = 10000)
for (i in 1:10000) {
  bootmeanLm10[i] <- mean(sample(Lm, length(Lm), replace = TRUE))
}
ciLm10 <- quantile(bootmeanLm10, probs = c(0.025, 0.975))
ciLm10 # same as above

# bootstrap confidence intervals for lin+ x lin-
bootmeanLNm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLNm[i] <- 1 - mean(sample(Lm, length(Lm), replace = TRUE))
}
ciLNm <- quantile(bootmeanLNm, probs = c(0.025, 0.975))
ciLNm
# 0.1739 0.4348

# bootstrap confidence intervals for lin- x lin+
bootmeanNm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNm[i] <- mean(sample(Nm, length(Nm), replace = TRUE))
}
ciNm <- quantile(bootmeanNm, probs = c(0.025, 0.975))
ciNm
# 0.27273 0.727272 - huge range

# bootstrap confidence intervals for lin- x lin-
bootmeanNNm <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNNm[i] <- 1 - mean(sample(Nm, length(Nm), replace = TRUE))
}
ciNNm <- quantile(bootmeanNNm, probs = c(0.025, 0.975))
ciNNm
# same as above

# within Rouse
LmR <- by_matRouse[by_matRouse$chemotype.x == "1", 'chemotype.y']
LmR <- unlist(LmR, use.names = F)
LmR <- as.numeric(as.character(LmR))
mean(LmR) #0.7083333 lin+ x lin+
1-mean(LmR) #0.2916667 lin+ x lin-
NmR <- by_matRouse[by_matRouse$chemotype.x == "0", 'chemotype.y']
NmR <- unlist(NmR, use.names = F)
NmR <- as.numeric(as.character(NmR))
mean(NmR) #0.5833333 lin- x lin+
1-mean(NmR) #0.4166667 lin- x lin-

# bootstrap confidence intervals for lin+ x lin+
bootmeanLmR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLmR[i] <- mean(sample(LmR, length(LmR), replace = TRUE))
}
ciLmR <- quantile(bootmeanLmR, probs = c(0.025, 0.975))
ciLmR
# 0.500 0.875 

# bootstrap confidence intervals for lin+ x lin-
bootmeanLNmR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanLNmR[i] <- 1 - mean(sample(LmR, length(LmR), replace = TRUE))
}
ciLNmR <- quantile(bootmeanLNmR, probs = c(0.025, 0.975))
ciLNmR
# 0.125 0.500 

# bootstrap confidence intervals for lin- x lin+
bootmeanNmR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNmR[i] <- mean(sample(NmR, length(NmR), replace = TRUE))
}
ciNmR <- quantile(bootmeanNmR, probs = c(0.025, 0.975))
ciNmR
# 0.3333333 0.8333333

# bootstrap confidence intervals for lin- x lin-
bootmeanNNmR <- vector(length = 1000000)
for (i in 1:1000000) {
  bootmeanNNmR[i] <- 1 - mean(sample(NmR, length(NmR), replace = TRUE))
}
ciNNmR <- quantile(bootmeanNNmR, probs = c(0.025, 0.975))
ciNNmR
# 0.1666667 0.6666667

# test with binary logit regression ####
chemAlt <- glm(patchem ~ matchem, by_off, family = binomial)
summary(chemAlt) # p = 0.04
# 0.8650   est  0.4280 se  2.021  z 0.0433 p

chemAltR <- glm(patchem ~ matchem, by_offR, family = binomial)
summary(chemAltR) # p = 0.405
# 0.4586 estimate     0.5506 se  0.833  z  0.405 p

# create dataframe ####
type <- c("1x1", "1x0",
          "0x1", "0x0",
          "1x1", "1x0",
          "0x1", "0x0")
lin_pp <- c(0.70, 0.5652, 0.826, "range")
lin_pm <- c(0.30, 0.1739, 0.4348, "range")
lin_mp <- c(0.50, 0.27273, 0.727272, "range")
lin_mm <- c(0.50, 0.27273, 0.727272, "range")
lin_ppr <- c(0.7083333, 0.500, 0.875, "rouse")
lin_pmr <- c(0.2916667, 0.125, 0.500, "rouse")
lin_mpr <- c(0.5833333, 0.3333333, 0.8333333, "rouse")
lin_mmr <- c(0.4166667, 0.1666667, 0.6666667, "rouse")

obs_freq <- as.data.frame(rbind(lin_pp, lin_pm, lin_mp, lin_mm, lin_ppr, lin_pmr, lin_mpr, lin_mmr))
obs_freq$type <- type
names(obs_freq) <- c("frequency", "lower", "upper", "pops", "type")

# adjusted frequencies ####
# save germ rate info for each cross type
lin_mm_germ <- c(type = "0x0", germ = 0.852, sd = 0.357, se = 0.00230)
lin_mp_germ <- c(type = "0x1", germ = 0.930, sd = 0.255, se = 0.000776)
lin_pm_germ <- c(type = "1x0", germ = 0.986, sd = 0.118, se = 0.000555)
lin_pp_germ <- c(type = "1x1", germ = 0.892, sd = 0.312, se = 0.00154)
# put them together as rows in df
frequencies <- bind_rows(lin_mm_germ, lin_mp_germ, lin_pm_germ, lin_pp_germ)
# add lower and upper bounds
frequencies <- frequencies %>% 
  mutate(across(2:ncol(.), as.numeric),
         germ_lower = germ - se,
         germ_upper = germ + se)
# save seedling survival information for each cross type
lin_mm_seed <- c(type = "0x0", seed = 0.905, s_sd = 0.295, s_se = 0.00281)
lin_mp_seed <- c(type = "0x1", seed = 0.774, s_sd = 0.420, s_se = 0.00264)
lin_pm_seed <- c(type = "1x0", seed = 0.717, s_sd = 0.453, s_se = 0.00377)
lin_pp_seed <- c(type = "1x1", seed = 0.737, s_sd = 0.443, s_se = 0.00466)
# put the rows together
seedling <- bind_rows(lin_mm_seed, lin_mp_seed, lin_pm_seed, lin_pp_seed)

# add upper and lower bounds
seedling <- seedling %>% 
  mutate(across(2:ncol(.), as.numeric),
         seed_lower = seed - s_se,
         seed_upper = seed + s_se)

# add no embryo extraction frequecies
frequencies$germ_no_ext <- c(0.180, 0.337, 0.272, 0.319)
frequencies$gne_sd <- c(0.385, 0.474, 0.446, 0.469)
frequencies$gne_se <- c(0.00193, 0.00266, 0.00217, 0.00515)

# upper and lower bounds
frequencies <- frequencies %>% 
  mutate(gne_lower = germ_no_ext - gne_se,
         gne_upper = germ_no_ext + gne_se)

# put germ and seed together
frequencies <- frequencies %>% 
  left_join(seedling, by = "type")

# create survival rate (combo of germ and seedling survival)
frequencies <- frequencies %>% 
  mutate(survive_ext = germ * seed,
         survive_ext_lower = germ_lower * seed_lower,
         survive_ext_upper = germ_upper * seed_upper,
         survive_ne = germ_no_ext * seed,
         survive_ne_lower = gne_lower * seed_lower,
         survive_ne_upper = gne_upper * seed_upper)

# percent of lin+ and lin- plants in the pops
lin_plus_range <- 60
lin_minus_range <- 40
lin_plus_rouse <-  53
lin_minus_rouse <- 47

frequencies$range <- c(40, 60, 40, 60)
frequencies$rouse <- c(47, 53, 47, 53)

# calculated adjusted frequencies
frequencies <- frequencies %>% 
  mutate(adj_range_ext = range*survive_ext,
         adj_range_ext_l = range*survive_ext_lower,
         adj_range_ext_u = range*survive_ext_upper,
         adj_rouse_ext = rouse*survive_ext,
         adj_rouse_ext_l = rouse*survive_ext_lower,
         adj_rouse_ext_u = rouse*survive_ext_upper,
         adj_range_ne = range*survive_ne,
         adj_range_ne_l = range*survive_ne_lower,
         adj_range_ne_u = range*survive_ne_upper,
         adj_rouse_ne = rouse*survive_ne,
         adj_rouse_ne_l = rouse*survive_ne_lower,
         adj_rouse_ne_u = rouse*survive_ne_upper)
frequencies <- frequencies %>%
  separate(type, into = c("matchem", "patchem"), remove = F, sep = "x")

range_counts <- frequencies %>% 
  group_by(matchem) %>% 
  summarise(range_total_ext = sum(adj_range_ext),
            range_total_ext_l = sum(adj_range_ext_l),
            range_total_ext_u = sum(adj_range_ext_u),
            range_total_ne = sum(adj_range_ne),
            range_total_ne_l = sum(adj_range_ne_l),
            range_total_ne_u = sum(adj_range_ne_u))
rouse_counts <- frequencies %>% 
  group_by(matchem) %>% 
  summarise(rouse_total_ext = sum(adj_rouse_ext),
            rouse_total_ext_l = sum(adj_rouse_ext_l),
            rouse_total_ext_u = sum(adj_rouse_ext_u),
            rouse_total_ne = sum(adj_rouse_ne),
            rouse_total_ne_l = sum(adj_rouse_ne_l),
            rouse_total_ne_u = sum(adj_rouse_ne_u))

frequencies <- frequencies %>% 
  left_join(range_counts, by = "matchem") %>% 
  left_join(rouse_counts, by = "matchem")
frequencies <- frequencies %>% 
  mutate(range_percent_ext = adj_range_ext/range_total_ext,
         range_percent_ext_l = adj_range_ext_l/range_total_ext_l,
         range_percent_ext_u = adj_range_ext_u/range_total_ext_u,
         rouse_percent_ext = adj_rouse_ext/rouse_total_ext,
         rouse_percent_ext_l = adj_rouse_ext_l/rouse_total_ext_l,
         rouse_percent_ext_u = adj_rouse_ext_u/rouse_total_ext_u,
         range_percent_ne = adj_range_ne/range_total_ne,
         range_percent_ne_l = adj_range_ne_l/range_total_ne_l,
         range_percent_ne_u = adj_range_ne_u/range_total_ne_u,
         rouse_percent_ne = adj_rouse_ne/rouse_total_ne,
         rouse_percent_ne_l = adj_rouse_ne_l/rouse_total_ne_l,
         rouse_percent_ne_u = adj_rouse_ne_u/rouse_total_ne_u)

# you actually wanted this long...
range_ext <- frequencies %>% 
  select(type, contains("range_percent_ext")) %>% 
  pivot_longer(cols = range_percent_ext,
               names_to = "category",
               values_to = "frequency") %>% 
  rename(lower = range_percent_ext_l, upper = range_percent_ext_u)

range_ne <- frequencies %>% 
  select(type, contains("range_percent_ne")) %>% 
  pivot_longer(cols = range_percent_ne,
               names_to = "category",
               values_to = "frequency") %>% 
  rename(lower = range_percent_ne_l, upper = range_percent_ne_u)

rouse_ext <- frequencies %>% 
  select(type, contains("rouse_percent_ext")) %>% 
  pivot_longer(cols = rouse_percent_ext,
               names_to = "category",
               values_to = "frequency") %>% 
  rename(lower = rouse_percent_ext_l, upper = rouse_percent_ext_u)

rouse_ne <- frequencies %>% 
  select(type, contains("rouse_percent_ne")) %>% 
  pivot_longer(cols = rouse_percent_ne,
               names_to = "category",
               values_to = "frequency") %>% 
  rename(lower = rouse_percent_ne_l, upper = rouse_percent_ne_u)

long_frequencies <- bind_rows(range_ext, range_ne, rouse_ext, rouse_ne)

long_frequencies <- long_frequencies %>%
  separate(category, into = c("pops", "percent", "emb_ext"), remove = F) %>% 
  select(!percent)
long_frequencies$category <- "expected"

# combine obs and exp
obs_freq <- obs_freq %>% 
  mutate(across(1:3, as.numeric),
         category = "observed")

final_freq <- bind_rows(long_frequencies, obs_freq)
final_freq <- final_freq %>% 
  separate(type, into = c("matchem", "patchem"), remove = F, sep = "x")

View(final_freq)

# expected without partial embryo extraction ####


# Figure 3 ####
# rename variables for better plot labels
final_freq <- final_freq %>% 
  mutate(cross = case_when(type == "0x0" ~ "lin- X lin-",
                           type == "0x1" ~ "lin- X lin+",
                           type == "1x0" ~ "lin+ X lin-",
                           type == "1x1" ~ "lin+ X lin+"),
         matchemo = case_when(matchem == 0 ~ "lin- maternal lines",
                              matchem == 1 ~ "lin+ maternal lines"),
         standing_freq_p = case_when(pops == "range" ~ 0.60,
                                     pops == "rouse" ~ 0.53),
         standing_freq_m = case_when(pops == "range" ~ 0.40, pops == "rouse" ~ 0.47),
         emb_ext = case_when(emb_ext == "ext" ~ "extracted", emb_ext == "ne" ~ "not extracted"))

final_freq2 <- final_freq %>% 
  filter(emb_ext %in% c(NA, "ext"))

# fig 3
ggplot(final_freq, aes(cross, frequency, color = emb_ext))+
  geom_hline(aes(yintercept = standing_freq_p), color = "grey", linetype = 3)+
  geom_hline(aes(yintercept = standing_freq_m), color = "grey", linetype = 4)+
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper), linetype = 1)+
  scale_color_brewer(palette = "Set2", na.value = "grey50")+
  facet_grid(cols = vars(category), rows = vars(pops), scales = "free_x") +
  theme_bw()+
  labs(x = "Maternal chemotype x pollen donor chemotype",
       y = "Frequency of realized mating events",
       color = "Partial embryo ext.", cols = c("lin-", "lin+"))+
  theme(legend.position = "bottom", text = element_text(size = 12), panel.grid = element_blank())


