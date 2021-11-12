## setwd("C:/Users/janya/Desktop/R/bedbugs/mating_dynamics")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(glmmTMB)
library(assortnet)
library(janitor)
library(igraph)

################### VISUALIZING MATING NETWORKS #####################

# Loading in mating matrices
mate_matrices <- readRDS("mating_matrices.rds")

# Function for turning matrix into igraph objects then into plots
func_matrix_to_igraph <- function(matrix){
  igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
  igraph <- set_vertex_attr(igraph, "sex", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male", "Female"))
  strength <- strength(igraph, mode = "in")
  igraph <- set_vertex_attr(igraph, "matings", value = strength)
  V(igraph)$color <- ifelse(V(igraph)$sex == "Female", "sandybrown", "skyblue3")
  V(igraph)$label.color <- "white"
  V(igraph)$size <- V(igraph)$matings*2.5
  E(igraph)$width <- E(igraph)$weight*1.5
  plot(igraph, edge.color = "dimgrey", layout = layout_nicely(igraph))
  return(igraph)
}

# Plotting each mating network
func_matrix_to_igraph(mate_matrices[[1]])
func_matrix_to_igraph(mate_matrices[[2]])
func_matrix_to_igraph(mate_matrices[[3]])
func_matrix_to_igraph(mate_matrices[[4]])
func_matrix_to_igraph(mate_matrices[[5]])
func_matrix_to_igraph(mate_matrices[[6]])

################### TOTAL NUMBER OF MATINGS ~ DAY #########################
mate_data <- read.csv("data/mating_data.csv")

mates_per_day <- read.csv("data/mating_data.csv") %>% 
                 mutate(mating_value = 1) %>% 
                 group_by(day) %>% 
                 summarize(num_of_mates = sum(mating_value))

ggplot(data = mates_per_day, aes(x = day, y = num_of_mates)) + geom_bar(stat = "identity", fill = "#86A484") + 
       labs(y = "Number of inseminations", x = "Day") + 
       scale_x_discrete(limits = c("1","2","3","4","5","6")) + 
       theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))


################# MATING RATE PER FEMALE ~ DAY ###########################
mates_per_fem <- read.csv("data/mates_per_fem.csv")
mates_per_fem$day <- as.integer(mates_per_fem$day)
mates_per_fem$treatment <- as.factor(mates_per_fem$treatment)
mates_per_fem$treatment <- relevel(mates_per_fem$treatment, "two")
mates_per_fem$block <- as.factor(mates_per_fem$block)

# Plot showing raw data
ggplot(data = mates_per_fem, aes(x = day, y = num_of_mates)) + geom_boxplot(outlier.shape = NA) + 
       geom_jitter(position = position_jitter(width = 0.25, height = 0), color = "sandybrown", alpha = 0.6, size = 1.5) +
       labs(y = "Insemination rate per female", x = "Day") + 
       scale_x_discrete(limits = c("1","2","3","4","5","6")) + 
       theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) + 
       facet_grid(~treatment)

# Barplot showing just the mean for each day
avg_mates_per_fem <- read.csv("data/mates_per_fem.csv") %>% 
                     group_by(day, treatment, block) %>% 
                     summarize(insemination_rate = mean(num_of_mates))

avg_mates_per_fem$treatment <- as.factor(avg_mates_per_fem$treatment)
avg_mates_per_fem$treatment <- relevel(avg_mates_per_fem$treatment, "two")
avg_mates_per_fem$block <- as.factor(avg_mates_per_fem$block)
                
ggplot(data = avg_mates_per_fem, aes(x = day, y = insemination_rate)) + geom_bar(stat = "identity", fill = "sandybrown") + 
       labs(y = "Mean insemination rate per female", x = "Day") + 
       scale_x_discrete(limits = c("1","2","3","4","5","6")) +
       theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) + 
       facet_grid(~treatment)
  
# Plot splitting data up by each replicate
ggplot(data = mates_per_fem, aes(x = day, y = num_of_mates)) + 
       geom_jitter(position = position_jitter(width = 0.15, height = 0), color = "sandybrown", alpha = 0.5, size = 1.5) + 
       facet_grid(~treatment*block) + 
       geom_smooth(method = "lm") +
       labs(y = "Inseminations per female", x = "Day") + 
       scale_x_discrete(limits = c("1","2","3","4","5","6")) + 
       theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))

