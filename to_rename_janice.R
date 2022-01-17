library(tidyverse)
library(ggplot2); theme_set(theme_classic())

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))


attr <- read.csv("data/bbsna_attributes_full.csv")

attr_f <- attr %>% 
          filter(sex == "Female")

attr_m <- attr %>% 
          filter(sex == "Male")


ggplot(data = attr_f, aes(unique_mates)) + geom_histogram(bins = 8, alpha = 0.5, fill = "sandybrown", color = "sandybrown") + 
                                            My_Theme +
                                            geom_vline(xintercept = mean(attr_f$unique_mates), linetype = "dashed")

ggplot(data = attr_m, aes(unique_mates)) + geom_histogram(bins = 10, alpha = 0.5, fill = "skyblue3", color = "skyblue3") + 
                                           My_Theme +
                                           geom_vline(xintercept = mean(attr_m$unique_mates), linetype = "dashed")
    

mean(attr_m$unique_mates)
mean(attr_f$unique_mates)


ggplot(data = attr_f, aes(unique_mates)) + geom_histogram(bins = 8, alpha = 0.5, fill = "sandybrown", 
                                                          color = "sandybrown") + 
  geom_vline(xintercept = mean(attr_f$unique_mates), linetype = "dashed")
