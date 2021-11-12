##setwd("C:/Users/janya/Desktop/R/bedbugs/bb_sna_2021")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(glmmTMB)
library(assortnet)
library(DHARMa)
library(emmeans)
library(janitor)

######################### CLEANING AND CREATING DATASET #########################
feed_data <- read.csv("all_data_clean.csv") %>% 
             filter(behaviour == "feeding")

# Calculating everyone's number of matings
fem_mate_data <- read.csv("all_data_clean.csv") %>% 
                         filter(behaviour == "mating") %>% 
                         mutate(mating_value = 1) %>% 
                         group_by(block, network, day, social_partner) %>% 
                         summarize(num_matings = sum(mating_value)) %>% 
                         rename(bug_id = social_partner)

male_mate_data <- read.csv("all_data_clean.csv") %>% 
                  filter(behaviour == "mating") %>% 
                  mutate(mating_value = 1) %>% 
                  group_by(block, network, day, focal_individual) %>% 
                  summarize(num_matings = sum(mating_value)) %>% 
                  rename(bug_id = focal_individual)


mate_data <- rbind(fem_mate_data, male_mate_data)

# Adding number of matings to feed data
feeding <- read.csv("data/feeding.csv") %>% 
           left_join(mate_data, by = c("block", "network", "bug_id", "day")) %>% 
           replace_na(list(num_matings = 0))

# Calculating everyone's number of mountings
mount_data <- read.csv("all_data_clean.csv") %>% 
              filter(behaviour == "mount") %>% 
              mutate(mount_value = 1) %>% 
              group_by(block, network, day, social_partner) %>% 
              summarize(num_mounts = sum(mount_value)) %>% 
              rename(bug_id = social_partner)

feeding <- feeding %>% 
           left_join(mount_data, by = c("block", "network", "bug_id", "day")) %>% 
           replace_na(list(num_mounts = 0))


############################## VISUALIZING DATA ################################
final_feed_data <- read.csv("data/feeding_data.csv", stringsAsFactors = TRUE) %>% 
                   unite('ID', c(network, bug_id), remove = FALSE, sep = "")

final_feed_data$day <- as.factor(final_feed_data$day)


## Plotting re-matings
ggplot(data = final_feed_data, aes(x = feed_status, y = num_rematings)) + geom_boxplot(outlier.shape = NA) + facet_grid(~treatment) + 
       geom_jitter(height = .05, alpha = 0.5, color = "sandybrown") + 
       labs(y = "Number of re-matings per day", x = "Feeding status") + 
       theme(text = element_text(size = 16))


## Re-matings model
remating_model <- glm(data = final_feed_data, num_rematings ~ feed_status*treatment) 


summary(remating_model)
plot(remating_model)


## Plotting mountings
ggplot(data = final_feed_data, aes(x = feed_status, y = num_mounts)) + geom_boxplot(outlier.shape = NA) + facet_grid(~treatment) + 
       geom_jitter(height = .05, alpha = 0.5, color = "sandybrown") + 
       labs(y = "Number of mountings received per day", x = "Feeding status") + 
       theme(text = element_text(size = 16))

## Mounting model

mount_model <- glmer(data = final_feed_data, num_mounts ~ feed_status*treatment)
summary(mount_model)

plot(mount_model)





















