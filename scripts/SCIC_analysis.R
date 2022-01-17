library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(glmmTMB)
library(lme4)
library(assortnet)
library(DHARMa)
library(emmeans)
library(janitor)
library(car)
library(psych)
library(igraph)
source("scripts/functions.R")

## CREATING MATING MATRICES
mating_matrices <- readRDS("mating_matrices.rds") 

## Function that shuffles matrices
func_shuffle_matrix <- function(matrix) { 
                       names <- colnames(matrix)
                       m_names <- sample(subset(names, names %in% LETTERS[1:12]))
                       f_names <- sample(subset(names, names %in% LETTERS[13:24]))
                       new_names <- c(m_names, f_names)
                       colnames(matrix) <- new_names; rownames(matrix) <- new_names
return(matrix)
}

## Creating randomized mating matrices with same SNA structure
random_matrices <- lapply(mating_matrices, func_shuffle_matrix)

## Adding row and column sums to mating matrices
func_sums <- function(matrix) {
             new_matrix <- cbind(matrix, total_matings_m = rowSums(matrix))
             new_matrix <- (rbind(new_matrix, total_matings_f = colSums(new_matrix)))
             new_matrix <- as.data.frame(new_matrix)
             new_matrix <- new_matrix %>% 
                           filter(total_matings_m > 0)
              
return(new_matrix)
}

mating_matrices <- lapply(mating_matrices, func_sums)
random_matrices <- lapply(random_matrices, func_sums)

mating_matrices[[1]]
## USED THIS LOOP TO CALCULATE WEIGHTED SCI FOR EACH NETWORK
func_SCI_w <- function(matrix) {
              SCI <- vector() 
              num_mates <- vector()
              for (j in 1:(nrow(matrix) - 1)) {
                   mate_prop <- vector()
                       for (i in 1:(ncol(matrix) - 1)) { 
                          if(matrix[j,i] > 0) {
                          mate_prop <- c(mate_prop, matrix[j,i]/matrix[(nrow(matrix)),i])
                          SCI[j] <- (length(mate_prop))/(sum(mate_prop))
                          num_mates[j] <- length(mate_prop)
                                               }
                                                       }
                              }
SCI <- c(SCI, NA)
num_mates <- c(num_mates, NA)
new_matrix <- cbind(matrix, SCI, num_mates)
return(new_matrix)
}

## Need a loop that calculates SCIC
mating_matrices <- lapply(mating_matrices, func_SCI_w)

## CALCULATING OBSERVED R-SQUARED FOR NETWORK #1
observed_data_1 <- as.data.frame(mating_matrices[[6]]) %>% 
                   drop_na() %>% 
                   select(SCI, num_mates) %>% 
                   mutate(network = 1)

model_1 <- lm(data = observed_data_1, SCI ~ num_mates)
summary(model_1)

SCIC_1 <- summary(model_1)$adj.r.squared


## CALCULATING SCIC FOR RANDOMIZED NETWORK 1s
random_matrices <- lapply(random_matrices, func_sums)
random_matrices <- lapply(random_matrices, func_SCI_w)
random_matrices[[1]]

random_1 <- as.data.frame(random_matrices[[1]]) %>% 
            drop_na() %>% 
            select(SCI, num_mates) %>% 
            mutate(network = 1)

random_model_1 <- lm(data = random_1, SCI ~ num_mates)
summary(random_model_1)

SCIC_random_1 <- summary(random_model_1)$adj.r.squared




























func_combine_matrices <- function(matrices) {
                         new_matrices <- data.frame()
                         for (i in 1:length(matrices)) {
                         new_matrices_i <- as.data.frame(matrices[[i]])
                         print(new_matrices_i)
                         new_matrices_i <- new_matrices_i %>% 
                                           drop_na() %>% 
                                           select(SCI, num_mates) %>% 
                                           mutate(network = i)
                         new_matrices <- rbind(new_matrices, new_matrices_i)

                         }                         
return(new_matrices)  

                         }

func_combine_matrices(mating_matrices)







SCIC_data <- read.csv("data/bbsna_attributes_full.csv") %>% 
             filter(sex == "Male")

SCIC_data

SCIC_model <- lmer(data = SCIC_data, SCI_weighted ~ unique_mates*treatment + (1|network))
summary(SCIC_model)
Anova(SCIC_model)

plot(simulateResiduals(SCIC_model))



