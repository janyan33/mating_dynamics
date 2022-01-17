setwd("C:/Users/janya/Desktop/R/bedbugs/mating_dynamics")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(glmmTMB)
library(lme4)
library(assortnet)
library(DHARMa)
library(emmeans)
library(janitor)
library(car)

############################## LOADING DATA IN ################################
feed_data <- read.csv("data/feeding_data.csv", stringsAsFactors = TRUE) %>% 
                   unite('ID', c(network, bug_id), remove = FALSE, sep = "")  %>%  #ID = combination of network and letter
                   filter(sex == "female") %>% 
                   select(-X)

feed_data$ID <- as.factor(feed_data$ID)
feed_data$block <- as.factor(feed_data$block)

############ REMATINGS ###########
## Plotting re-matings
hist(feed_data$num_rematings, breaks = 8) # Raw data histogram

ggplot(data = feed_data, aes(x = feed_status, y = num_rematings)) + geom_boxplot(outlier.shape = NA) + facet_grid(~treatment) + 
       geom_jitter(height = .05, alpha = 0.5, color = "sandybrown") + 
       labs(y = "Remating rate", x = "Feeding status") + 
       theme(text = element_text(size = 16))

## Re-mating model
remating_model <- glmmTMB(data = feed_data, num_rematings ~ 1 + feed_status*treatment + (1|block) + (1|ID), 
                          family = nbinom2)


summary(remating_model)
plot(simulateResiduals(remating_model))

############ MOUNTINGS ###########
## Plotting mountings
hist(feed_data$num_mounts, breaks = 30) # Raw data histogram

ggplot(data = feed_data, aes(x = feed_status, y = num_mounts)) + geom_boxplot(outlier.shape = NA) + facet_grid(~treatment) + 
       geom_jitter(height = .05, alpha = 0.5, color = "sandybrown") + 
       labs(y = "Mounting rate", x = "Feeding status") + 
       theme(text = element_text(size = 16))


## Mounting model
mount_model <- glmmTMB(data = feed_data, num_mounts ~ feed_status*treatment + block + (1|ID), 
                       family = nbinom2())

summary(mount_model)
plot(simulateResiduals(mount_model))
