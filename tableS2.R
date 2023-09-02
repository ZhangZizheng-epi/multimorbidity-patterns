rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2)
#------
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male <- read.table('cos_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female <- read.table('cos_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
summary1 <- read.table('summary1')
frame <- data.frame(Metrics=c('No.of patients',
                              'Nodes(Diseases)',
                              'Edges(comorbidity pairs)',
                              'Density',
                              'Average Degree',
                              'Average Weighted degree',
                              'Average closeness centrality'),
                    Male=c(rep(NA,7)),
                    Female=c(rep(NA,7)))

#---Column 2: Male parameter assignment---
#---1(sample size)---
frame[1,2] <- summary1$Male[1]
#---2(nodes count)---
frame[2,2] <- vcount(male)
#---3(edges count)---
frame[3,2] <- ecount(male)
#---4(density)---
frame[4,2] <- graph.density(male)
#---5(Average Degree)---
frame[5,2] <- mean(degree(male))
#---6(Average Weighted degree)---
frame[6,2] <- mean(strength(male))
#---7(Average closeness centrality)---
male_length <- male
E(male_length)$weight <- 1/E(male_length)$weight
frame[7,2] <- mean(closeness(male_length,normalized = T))

#---Column 3: Female parameter assignment---
#---1(sample size)---
frame[1,3] <- summary1$Female[1]
#---2(nodes count)---
frame[2,3] <- vcount(female)
#---3(edges count)---
frame[3,3] <- ecount(female)
#---4(density)---
frame[4,3] <- graph.density(female)
#---5(Average Degree)---
frame[5,3] <- mean(degree(female))
#---6(Average Weighted degree)---
frame[6,3] <- mean(strength(female))
#---7(Average closeness centrality)---
female_length <- female
E(female_length)$weight <- 1/E(female_length)$weight
frame[7,3] <- mean(closeness(female_length,normalized = T))

#---Save---
#write.csv(frame,file = "TableS2(metrics).csv")



