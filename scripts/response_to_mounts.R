library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(glmmTMB)
library(lme4)
library(assortnet)
library(DHARMa)
library(emmeans)
library(janitor)
library(car)


## Histogram of male mating recency
male_mating <- read.csv("data/male_mate_recency.csv") %>% 
               mutate(mating_recency = na_if(mating_recency, "N/A"))

male_mating$mating_recency <- as.numeric(male_mating$mating_recency)

hist(male_mating$mating_recency, breaks = 80, xlab = "Mating recency (time since last mating)", 
     main = "Male matings")

## Histogram of female mating recency
female_mating <- read.csv("data/fem_mate_mount.csv") %>% 
                 mutate(mating_recency = na_if(mating_recency, "N/A")) %>% 
                 unite('ID', c(network, focal_individual), remove = FALSE, sep = "") %>% 
                 filter(behaviour == "mating") %>% 
                 drop_na(mating_recency)
                 #mutate(mating_recency = ifelse(female_mating$mating_recency >= 80, 80, female_mating$mating_recency))

female_mounting <- read.csv("data/fem_mate_mount.csv") %>% 
                   mutate(mating_recency = na_if(mating_recency, "N/A")) %>% 
                   filter(behaviour == "mount") 

female_mating$mating_recency <- as.numeric(female_mating$mating_recency)
female_mounting$mating_recency <- as.numeric(female_mounting$mating_recency)


h_mate_raw <- hist(female_mating$mating_recency, breaks = seq(0, 80, by = 1), xlab = "Mating recency (time since last mating)", 
                 main = "Female matings")

h_mate_raw$density <- h_mate_raw$counts/sum(h_mate_raw$counts)
plot(h_mate_raw, freq = FALSE)


hist(female_mounting$mating_recency, breaks = 80, xlab = "Mating recency (time since last mating)", 
     main = "Female mountings")

## Female response to mountings as a function of mating recency
# Loading data
mount_patch <- read.csv("mount_patch_table.csv")
female_mounting <- read.csv("data/fem_mate_mount.csv") %>% 
                   mutate(mating_recency = na_if(mating_recency, "N/A")) %>% 
                   filter(behaviour == "mount") %>% 
                   left_join(mount_patch, by = "partner_response") %>% 
                   select(-partner_response) %>% 
                   mutate(fem_response = 
                          ifelse(patch_behaviour == "n/a" | patch_behaviour == "moved",
                                                0, 1)) %>% 
                   drop_na(mating_recency)

female_mounting$fem_response <- as.factor(female_mounting$fem_response)
female_mounting$mating_recency <- as.numeric(female_mounting$mating_recency)
female_mounting$fem_response <- as.numeric(female_mounting$fem_response)

female_mounting <- female_mounting %>% 
                   mutate(fem_response = fem_response - 1)

# Model
mount_response_model <- glm(data = female_mounting, fem_response ~ mating_recency, family = binomial(link = "logit"))
summary(mount_response_model)
#plot(mount_response_model)

ggplot(data = female_mounting, aes(y = fem_response, x = mating_recency)) + geom_point(alpha = 0.5, shape = 1) + 
       geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
      





## Scatterplot of remating intervals
female_mating$treatment <- as.factor(female_mating$treatment)


ggplot(data = female_mating, aes(x = treatment, y = mating_recency)) + geom_boxplot(outlier.shape = NA) + 
       geom_jitter()

mean(female_mating$mating_recency)
median(female_mating$mating_recency)       







