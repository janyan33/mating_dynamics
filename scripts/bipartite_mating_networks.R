setwd("C:/Users/janya/Desktop/R/bedbugs")

library(tidyverse)
library(igraph)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(glmmTMB)
library(bipartite)
library(psych)

## CREATING MATING BIPARTITE NETWORKS
mating_matrices <- readRDS("mating_matrices.rds")
mating_matrices[[1]]

for (i in 1:length(mating_matrices)) {
     plotweb(web = mating_matrices[[i]], labsize = 2, col.high = "sandybrown", col.low = "skyblue3")
}


############# IGNORE FOR NOW #############
## CALCULATING SPERM COMPETITION INTENSITY CORREALTION (SCIC) FOR EACH MALE
attr_data <- read.csv("data/bbsna_attributes_full.csv") %>% 
             filter(network != "prelim")

attr_data$SIC <- as.numeric(attr_data$SIC)
attr_data$network <- as.factor(attr_data$network)
attr_data$block <- as.factor(attr_data$block)
attr_data$treatment <- as.factor(attr_data$treatment)
attr_data$treatment <- relevel(attr_data$treatment, "two")


ggplot(data = attr_data, aes(x = matings, y = SIC, color = network)) + geom_point() + geom_smooth(method = "lm") + 
       facet_grid(~treatment)


SCIC_model <- glmer(data = attr_data, SIC ~ mates + (1|treatment) + (1|block))
summary(SCIC_model)



## Visualizing # of mates ~ total # of matings 
ggplot(data = attr_data, aes(x = matings, y = unique_mates, color = sex)) + 
       geom_jitter(alpha = 0.5, size = 2, width = 0.2, height = 0.2) +
       facet_grid(~treatment) + 
       geom_smooth(method = "lm") + 
       labs(x = "Total number of matings", y = "Number of unique mates") + 
       scale_color_manual(values = c("sandybrown", "skyblue3")) + 
       theme(text = element_text(size = 16))
















