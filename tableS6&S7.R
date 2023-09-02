rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2)
#------
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male <- read.table('cos_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female <- read.table('cos_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
ICD <- c("E04","E11.4","E78","H26","I10","I25","I49","I63","I70","J31","J42","K29.5","K76.0","M13","M81","N18" )

#---perturbation_nodes---
male_E04 <- delete.vertices(male,"E04");female_E04 <- delete.vertices(female,"E04")
male_E11.4 <- delete.vertices(male,"E11.4");female_E11.4 <- delete.vertices(female,"E11.4")
male_E78 <- delete.vertices(male,"E78");female_E78 <- delete.vertices(female,"E78")
male_H26 <- delete.vertices(male,"H26");female_H26 <- delete.vertices(female,"H26")
male_I10 <- delete.vertices(male,"I10");female_I10 <- delete.vertices(female,"I10")
male_I25 <- delete.vertices(male,"I25");female_I25 <- delete.vertices(female,"I25")
male_I49 <- delete.vertices(male,"I49");female_I49 <- delete.vertices(female,"I49")
male_I63 <- delete.vertices(male,"I63");female_I63 <- delete.vertices(female,"I63")
male_I70 <- delete.vertices(male,"I70");female_I70 <- delete.vertices(female,"I70")
male_J31 <- delete.vertices(male,"J31");female_J31 <- delete.vertices(female,"J31")
male_J42 <- delete.vertices(male,"J42");female_J42 <- delete.vertices(female,"J42")
male_K29.5 <- delete.vertices(male,"K29.5");female_K29.5 <- delete.vertices(female,"K29.5")
male_K76.0 <- delete.vertices(male,"K76.0");female_K76.0 <- delete.vertices(female,"K76.0")
male_M13 <- delete.vertices(male,"M13");female_M13 <- delete.vertices(female,"M13")
male_M81 <- delete.vertices(male,"M81");female_M81 <- delete.vertices(female,"M81")
male_N18 <- delete.vertices(male,"N18");female_N18 <- delete.vertices(female,"N18")

#----Take the reciprocal for harmonic centrality---
male_Har <- male;E(male_Har)$weight <- 1/E(male_Har)$weight
male_E04_Har <- male_E04;E(male_E04_Har)$weight <- 1/E(male_E04_Har)$weight
male_E11.4_Har <- male_E11.4;E(male_E11.4_Har)$weight <- 1/E(male_E11.4_Har)$weight
male_E78_Har <- male_E78;E(male_E78_Har)$weight <- 1/E(male_E78_Har)$weight
male_H26_Har <- male_H26;E(male_H26_Har)$weight <- 1/E(male_H26_Har)$weight
male_I10_Har <- male_I10;E(male_I10_Har)$weight <- 1/E(male_I10_Har)$weight
male_I25_Har <- male_I25;E(male_I25_Har)$weight <- 1/E(male_I25_Har)$weight
male_I49_Har <- male_I49;E(male_I49_Har)$weight <- 1/E(male_I49_Har)$weight
male_I63_Har <- male_I63;E(male_I63_Har)$weight <- 1/E(male_I63_Har)$weight
male_I70_Har <- male_I70;E(male_I70_Har)$weight <- 1/E(male_I70_Har)$weight
male_J31_Har <- male_J31;E(male_J31_Har)$weight <- 1/E(male_J31_Har)$weight
male_J42_Har <- male_J42;E(male_J42_Har)$weight <- 1/E(male_J42_Har)$weight
male_K29.5_Har <- male_K29.5;E(male_K29.5_Har)$weight <- 1/E(male_K29.5_Har)$weight
male_K76.0_Har <- male_K76.0;E(male_K76.0_Har)$weight <- 1/E(male_K76.0_Har)$weight
male_M13_Har <- male_M13;E(male_M13_Har)$weight <- 1/E(male_M13_Har)$weight
male_M81_Har <- male_M81;E(male_M81_Har)$weight <- 1/E(male_M81_Har)$weight
male_N18_Har <- male_N18;E(male_N18_Har)$weight <- 1/E(male_N18_Har)$weight
female_Har <- female;E(female_Har)$weight <- 1/E(female_Har)$weight
female_E04_Har <- female_E04;E(female_E04_Har)$weight <- 1/E(female_E04_Har)$weight
female_E11.4_Har <- female_E11.4;E(female_E11.4_Har)$weight <- 1/E(female_E11.4_Har)$weight
female_E78_Har <- female_E78;E(female_E78_Har)$weight <- 1/E(female_E78_Har)$weight
female_H26_Har <- female_H26;E(female_H26_Har)$weight <- 1/E(female_H26_Har)$weight
female_I10_Har <- female_I10;E(female_I10_Har)$weight <- 1/E(female_I10_Har)$weight
female_I25_Har <- female_I25;E(female_I25_Har)$weight <- 1/E(female_I25_Har)$weight
female_I49_Har <- female_I49;E(female_I49_Har)$weight <- 1/E(female_I49_Har)$weight
female_I63_Har <- female_I63;E(female_I63_Har)$weight <- 1/E(female_I63_Har)$weight
female_I70_Har <- female_I70;E(female_I70_Har)$weight <- 1/E(female_I70_Har)$weight
female_J31_Har <- female_J31;E(female_J31_Har)$weight <- 1/E(female_J31_Har)$weight
female_J42_Har <- female_J42;E(female_J42_Har)$weight <- 1/E(female_J42_Har)$weight
female_K29.5_Har <- female_K29.5;E(female_K29.5_Har)$weight <- 1/E(female_K29.5_Har)$weight
female_K76.0_Har <- female_K76.0;E(female_K76.0_Har)$weight <- 1/E(female_K76.0_Har)$weight
female_M13_Har <- female_M13;E(female_M13_Har)$weight <- 1/E(female_M13_Har)$weight
female_M81_Har <- female_M81;E(female_M81_Har)$weight <- 1/E(female_M81_Har)$weight
female_N18_Har <- female_N18;E(female_N18_Har)$weight <- 1/E(female_N18_Har)$weight

#---male frame---
frame_male <- data.frame(Networks=c('original network',
                              'delete E04','delete E11.4','delete E78','delete H26',
                              'delete I10','delete I25','delete I49','delete I63',
                              'delete I70','delete J31','delete J42','delete K29.5',
                              'delete K76.0','delete M13','delete M81','delete N18'),
                    Nodes=c(rep(NA,17)),
                    Edges=c(rep(NA,17)),
                    Density=c(rep(NA,17)),
                    Average_Degree = c(rep(NA,17)),
                    Average_Weighted_Degree = c(rep(NA,17)),
                    Average_Harmonic_Centrality = c(rep(NA,17)),
                    Connected = c(rep(NA,17)))

#---original network---
frame_male[1,2] <- vcount(male)
frame_male[1,3] <- ecount(male)
frame_male[1,4] <- graph.density(male)
frame_male[1,5] <- mean(degree(male))
frame_male[1,6] <- mean(strength(male))
frame_male[1,7] <- mean(harmonic_centrality(male_Har,normalized = T))
frame_male[1,8] <- is.connected(male)
#---delete 16 nodes---
frame_male[2,2] <- vcount(male_E04)
frame_male[2,3] <- ecount(male_E04)
frame_male[2,4] <- graph.density(male_E04)
frame_male[2,5] <- mean(degree(male_E04))
frame_male[2,6] <- mean(strength(male_E04))
frame_male[2,7] <- mean(harmonic_centrality(male_E04_Har,normalized = T))
frame_male[2,8] <- is.connected(male_E04)
#---
frame_male[3,2] <- vcount(male_E11.4)
frame_male[3,3] <- ecount(male_E11.4)
frame_male[3,4] <- graph.density(male_E11.4)
frame_male[3,5] <- mean(degree(male_E11.4))
frame_male[3,6] <- mean(strength(male_E11.4))
frame_male[3,7] <- mean(harmonic_centrality(male_E11.4_Har,normalized = T))
frame_male[3,8] <- is.connected(male_E11.4)
#---
frame_male[4,2] <- vcount(male_E78)
frame_male[4,3] <- ecount(male_E78)
frame_male[4,4] <- graph.density(male_E78)
frame_male[4,5] <- mean(degree(male_E78))
frame_male[4,6] <- mean(strength(male_E78))
frame_male[4,7] <- mean(harmonic_centrality(male_E78_Har,normalized = T))
frame_male[4,8] <- is.connected(male_E78)
#---
frame_male[5,2] <- vcount(male_H26)
frame_male[5,3] <- ecount(male_H26)
frame_male[5,4] <- graph.density(male_H26)
frame_male[5,5] <- mean(degree(male_H26))
frame_male[5,6] <- mean(strength(male_H26))
frame_male[5,7] <- mean(harmonic_centrality(male_H26_Har,normalized = T))
frame_male[5,8] <- is.connected(male_H26)
#---
frame_male[6,2] <- vcount(male_I10)
frame_male[6,3] <- ecount(male_I10)
frame_male[6,4] <- graph.density(male_I10)
frame_male[6,5] <- mean(degree(male_I10))
frame_male[6,6] <- mean(strength(male_I10))
frame_male[6,7] <- mean(harmonic_centrality(male_I10_Har,normalized = T))
frame_male[6,8] <- is.connected(male_I10)
#---
frame_male[7,2] <- vcount(male_I25)
frame_male[7,3] <- ecount(male_I25)
frame_male[7,4] <- graph.density(male_I25)
frame_male[7,5] <- mean(degree(male_I25))
frame_male[7,6] <- mean(strength(male_I25))
frame_male[7,7] <- mean(harmonic_centrality(male_I25_Har,normalized = T))
frame_male[7,8] <- is.connected(male_I25)
#---
frame_male[8,2] <- vcount(male_I49)
frame_male[8,3] <- ecount(male_I49)
frame_male[8,4] <- graph.density(male_I49)
frame_male[8,5] <- mean(degree(male_I49))
frame_male[8,6] <- mean(strength(male_I49))
frame_male[8,7] <- mean(harmonic_centrality(male_I49_Har,normalized = T))
frame_male[8,8] <- is.connected(male_I49)
#---
frame_male[9,2] <- vcount(male_I63)
frame_male[9,3] <- ecount(male_I63)
frame_male[9,4] <- graph.density(male_I63)
frame_male[9,5] <- mean(degree(male_I63))
frame_male[9,6] <- mean(strength(male_I63))
frame_male[9,7] <- mean(harmonic_centrality(male_I63_Har,normalized = T))
frame_male[9,8] <- is.connected(male_I63)
#---
frame_male[10,2] <- vcount(male_I70)
frame_male[10,3] <- ecount(male_I70)
frame_male[10,4] <- graph.density(male_I70)
frame_male[10,5] <- mean(degree(male_I70))
frame_male[10,6] <- mean(strength(male_I70))
frame_male[10,7] <- mean(harmonic_centrality(male_I70_Har,normalized = T))
frame_male[10,8] <- is.connected(male_I70)
#---
frame_male[11,2] <- vcount(male_J31)
frame_male[11,3] <- ecount(male_J31)
frame_male[11,4] <- graph.density(male_J31)
frame_male[11,5] <- mean(degree(male_J31))
frame_male[11,6] <- mean(strength(male_J31))
frame_male[11,7] <- mean(harmonic_centrality(male_J31_Har,normalized = T))
frame_male[11,8] <- is.connected(male_J31)
#---
frame_male[12,2] <- vcount(male_J42)
frame_male[12,3] <- ecount(male_J42)
frame_male[12,4] <- graph.density(male_J42)
frame_male[12,5] <- mean(degree(male_J42))
frame_male[12,6] <- mean(strength(male_J42))
frame_male[12,7] <- mean(harmonic_centrality(male_J42_Har,normalized = T))
frame_male[12,8] <- is.connected(male_J42)
#---
frame_male[13,2] <- vcount(male_K29.5)
frame_male[13,3] <- ecount(male_K29.5)
frame_male[13,4] <- graph.density(male_K29.5)
frame_male[13,5] <- mean(degree(male_K29.5))
frame_male[13,6] <- mean(strength(male_K29.5))
frame_male[13,7] <- mean(harmonic_centrality(male_K29.5_Har,normalized = T))
frame_male[13,8] <- is.connected(male_K29.5)
#---
frame_male[14,2] <- vcount(male_K76.0)
frame_male[14,3] <- ecount(male_K76.0)
frame_male[14,4] <- graph.density(male_K76.0)
frame_male[14,5] <- mean(degree(male_K76.0))
frame_male[14,6] <- mean(strength(male_K76.0))
frame_male[14,7] <- mean(harmonic_centrality(male_K76.0_Har,normalized = T))
frame_male[14,8] <- is.connected(male_K76.0)
#---
frame_male[15,2] <- vcount(male_M13)
frame_male[15,3] <- ecount(male_M13)
frame_male[15,4] <- graph.density(male_M13)
frame_male[15,5] <- mean(degree(male_M13))
frame_male[15,6] <- mean(strength(male_M13))
frame_male[15,7] <- mean(harmonic_centrality(male_M13_Har,normalized = T))
frame_male[15,8] <- is.connected(male_M13)
#---
frame_male[16,2] <- vcount(male_M81)
frame_male[16,3] <- ecount(male_M81)
frame_male[16,4] <- graph.density(male_M81)
frame_male[16,5] <- mean(degree(male_M81))
frame_male[16,6] <- mean(strength(male_M81))
frame_male[16,7] <- mean(harmonic_centrality(male_M81_Har,normalized = T))
frame_male[16,8] <- is.connected(male_M81)
#---
frame_male[17,2] <- vcount(male_N18)
frame_male[17,3] <- ecount(male_N18)
frame_male[17,4] <- graph.density(male_N18)
frame_male[17,5] <- mean(degree(male_N18))
frame_male[17,6] <- mean(strength(male_N18))
frame_male[17,7] <- mean(harmonic_centrality(male_N18_Har,normalized = T))
frame_male[17,8] <- is.connected(male_N18)

#---female frame---
frame_female <- data.frame(Networks=c('original network',
                                    'delete E04','delete E11.4','delete E78','delete H26',
                                    'delete I10','delete I25','delete I49','delete I63',
                                    'delete I70','delete J31','delete J42','delete K29.5',
                                    'delete K76.0','delete M13','delete M81','delete N18'),
                         Nodes=c(rep(NA,17)),
                         Edges=c(rep(NA,17)),
                         Density=c(rep(NA,17)),
                         Average_Degree = c(rep(NA,17)),
                         Average_Weighted_Degree = c(rep(NA,17)),
                         Average_Harmonic_Centrality = c(rep(NA,17)),
                         Connected = c(rep(NA,17)))

#---original network---
frame_female[1,2] <- vcount(female)
frame_female[1,3] <- ecount(female)
frame_female[1,4] <- graph.density(female)
frame_female[1,5] <- mean(degree(female))
frame_female[1,6] <- mean(strength(female))
frame_female[1,7] <- mean(harmonic_centrality(female_Har,normalized = T))
frame_female[1,8] <- is.connected(female)
#---delete 16 nodes---
frame_female[2,2] <- vcount(female_E04)
frame_female[2,3] <- ecount(female_E04)
frame_female[2,4] <- graph.density(female_E04)
frame_female[2,5] <- mean(degree(female_E04))
frame_female[2,6] <- mean(strength(female_E04))
frame_female[2,7] <- mean(harmonic_centrality(female_E04_Har,normalized = T))
frame_female[2,8] <- is.connected(female_E04)
#---
frame_female[3,2] <- vcount(female_E11.4)
frame_female[3,3] <- ecount(female_E11.4)
frame_female[3,4] <- graph.density(female_E11.4)
frame_female[3,5] <- mean(degree(female_E11.4))
frame_female[3,6] <- mean(strength(female_E11.4))
frame_female[3,7] <- mean(harmonic_centrality(female_E11.4_Har,normalized = T))
frame_female[3,8] <- is.connected(female_E11.4)
#---
frame_female[4,2] <- vcount(female_E78)
frame_female[4,3] <- ecount(female_E78)
frame_female[4,4] <- graph.density(female_E78)
frame_female[4,5] <- mean(degree(female_E78))
frame_female[4,6] <- mean(strength(female_E78))
frame_female[4,7] <- mean(harmonic_centrality(female_E78_Har,normalized = T))
frame_female[4,8] <- is.connected(female_E78)
#---
frame_female[5,2] <- vcount(female_H26)
frame_female[5,3] <- ecount(female_H26)
frame_female[5,4] <- graph.density(female_H26)
frame_female[5,5] <- mean(degree(female_H26))
frame_female[5,6] <- mean(strength(female_H26))
frame_female[5,7] <- mean(harmonic_centrality(female_H26_Har,normalized = T))
frame_female[5,8] <- is.connected(female_H26)
#---
frame_female[6,2] <- vcount(female_I10)
frame_female[6,3] <- ecount(female_I10)
frame_female[6,4] <- graph.density(female_I10)
frame_female[6,5] <- mean(degree(female_I10))
frame_female[6,6] <- mean(strength(female_I10))
frame_female[6,7] <- mean(harmonic_centrality(female_I10_Har,normalized = T))
frame_female[6,8] <- is.connected(female_I10)
#---
frame_female[7,2] <- vcount(female_I25)
frame_female[7,3] <- ecount(female_I25)
frame_female[7,4] <- graph.density(female_I25)
frame_female[7,5] <- mean(degree(female_I25))
frame_female[7,6] <- mean(strength(female_I25))
frame_female[7,7] <- mean(harmonic_centrality(female_I25_Har,normalized = T))
frame_female[7,8] <- is.connected(female_I25)
#---
frame_female[8,2] <- vcount(female_I49)
frame_female[8,3] <- ecount(female_I49)
frame_female[8,4] <- graph.density(female_I49)
frame_female[8,5] <- mean(degree(female_I49))
frame_female[8,6] <- mean(strength(female_I49))
frame_female[8,7] <- mean(harmonic_centrality(female_I49_Har,normalized = T))
frame_female[8,8] <- is.connected(female_I49)
#---
frame_female[9,2] <- vcount(female_I63)
frame_female[9,3] <- ecount(female_I63)
frame_female[9,4] <- graph.density(female_I63)
frame_female[9,5] <- mean(degree(female_I63))
frame_female[9,6] <- mean(strength(female_I63))
frame_female[9,7] <- mean(harmonic_centrality(female_I63_Har,normalized = T))
frame_female[9,8] <- is.connected(female_I63)
#---
frame_female[10,2] <- vcount(female_I70)
frame_female[10,3] <- ecount(female_I70)
frame_female[10,4] <- graph.density(female_I70)
frame_female[10,5] <- mean(degree(female_I70))
frame_female[10,6] <- mean(strength(female_I70))
frame_female[10,7] <- mean(harmonic_centrality(female_I70_Har,normalized = T))
frame_female[10,8] <- is.connected(female_I70)
#---
frame_female[11,2] <- vcount(female_J31)
frame_female[11,3] <- ecount(female_J31)
frame_female[11,4] <- graph.density(female_J31)
frame_female[11,5] <- mean(degree(female_J31))
frame_female[11,6] <- mean(strength(female_J31))
frame_female[11,7] <- mean(harmonic_centrality(female_J31_Har,normalized = T))
frame_female[11,8] <- is.connected(female_J31)
#---
frame_female[12,2] <- vcount(female_J42)
frame_female[12,3] <- ecount(female_J42)
frame_female[12,4] <- graph.density(female_J42)
frame_female[12,5] <- mean(degree(female_J42))
frame_female[12,6] <- mean(strength(female_J42))
frame_female[12,7] <- mean(harmonic_centrality(female_J42_Har,normalized = T))
frame_female[12,8] <- is.connected(female_J42)
#---
frame_female[13,2] <- vcount(female_K29.5)
frame_female[13,3] <- ecount(female_K29.5)
frame_female[13,4] <- graph.density(female_K29.5)
frame_female[13,5] <- mean(degree(female_K29.5))
frame_female[13,6] <- mean(strength(female_K29.5))
frame_female[13,7] <- mean(harmonic_centrality(female_K29.5_Har,normalized = T))
frame_female[13,8] <- is.connected(female_K29.5)
#---
frame_female[14,2] <- vcount(female_K76.0)
frame_female[14,3] <- ecount(female_K76.0)
frame_female[14,4] <- graph.density(female_K76.0)
frame_female[14,5] <- mean(degree(female_K76.0))
frame_female[14,6] <- mean(strength(female_K76.0))
frame_female[14,7] <- mean(harmonic_centrality(female_K76.0_Har,normalized = T))
frame_female[14,8] <- is.connected(female_K76.0)
#---
frame_female[15,2] <- vcount(female_M13)
frame_female[15,3] <- ecount(female_M13)
frame_female[15,4] <- graph.density(female_M13)
frame_female[15,5] <- mean(degree(female_M13))
frame_female[15,6] <- mean(strength(female_M13))
frame_female[15,7] <- mean(harmonic_centrality(female_M13_Har,normalized = T))
frame_female[15,8] <- is.connected(female_M13)
#---
frame_female[16,2] <- vcount(female_M81)
frame_female[16,3] <- ecount(female_M81)
frame_female[16,4] <- graph.density(female_M81)
frame_female[16,5] <- mean(degree(female_M81))
frame_female[16,6] <- mean(strength(female_M81))
frame_female[16,7] <- mean(harmonic_centrality(female_M81_Har,normalized = T))
frame_female[16,8] <- is.connected(female_M81)
#---
frame_female[17,2] <- vcount(female_N18)
frame_female[17,3] <- ecount(female_N18)
frame_female[17,4] <- graph.density(female_N18)
frame_female[17,5] <- mean(degree(female_N18))
frame_female[17,6] <- mean(strength(female_N18))
frame_female[17,7] <- mean(harmonic_centrality(female_N18_Har,normalized = T))
frame_female[17,8] <- is.connected(female_N18)

#---Percentage change---
male_Edges <- round(((frame_male$Edges[2:17]-frame_male$Edges[1])/frame_male$Edges[1])*100,3)
frame_male$Edges[2:17] <- paste(frame_male$Edges[2:17],'(',male_Edges,'%',')',sep = '')
#---
male_Density <- round(((frame_male$Density[2:17]-frame_male$Density[1])/frame_male$Density[1])*100,3)
frame_male$Density <- round(frame_male$Density,3)
frame_male$Density[2:17] <- paste(frame_male$Density[2:17],'(',male_Density,'%',')',sep = '')
#---
male_Average_Degree <- round(((frame_male$Average_Degree[2:17]-frame_male$Average_Degree[1])/frame_male$Average_Degree[1])*100,3)
frame_male$Average_Degree <- round(frame_male$Average_Degree,3)
frame_male$Average_Degree[2:17] <- paste(frame_male$Average_Degree[2:17],'(',male_Average_Degree,'%',')',sep = '')
#---
male_Average_Weighted_Degree <- round(((frame_male$Average_Weighted_Degree[2:17]-frame_male$Average_Weighted_Degree[1])/frame_male$Average_Weighted_Degree[1])*100,3)
frame_male$Average_Weighted_Degree <- round(frame_male$Average_Weighted_Degree,3)
frame_male$Average_Weighted_Degree[2:17] <- paste(frame_male$Average_Weighted_Degree[2:17],'(',male_Average_Weighted_Degree,'%',')',sep = '')
#---
male_Average_Harmonic_Centrality <- round(((frame_male$Average_Harmonic_Centrality[2:17]-frame_male$Average_Harmonic_Centrality[1])/frame_male$Average_Harmonic_Centrality[1])*100,3)
frame_male$Average_Harmonic_Centrality <- round(frame_male$Average_Harmonic_Centrality,3)
frame_male$Average_Harmonic_Centrality[2:17] <- paste(frame_male$Average_Harmonic_Centrality[2:17],'(',male_Average_Harmonic_Centrality,'%',')',sep = '')
#---female---
female_Edges <- round(((frame_female$Edges[2:17]-frame_female$Edges[1])/frame_female$Edges[1])*100,3)
frame_female$Edges[2:17] <- paste(frame_female$Edges[2:17],'(',female_Edges,'%',')',sep = '')
#---
female_Density <- round(((frame_female$Density[2:17]-frame_female$Density[1])/frame_female$Density[1])*100,3)
frame_female$Density <- round(frame_female$Density,3)
frame_female$Density[2:17] <- paste(frame_female$Density[2:17],'(',female_Density,'%',')',sep = '')
#---
female_Average_Degree <- round(((frame_female$Average_Degree[2:17]-frame_female$Average_Degree[1])/frame_female$Average_Degree[1])*100,3)
frame_female$Average_Degree <- round(frame_female$Average_Degree,3)
frame_female$Average_Degree[2:17] <- paste(frame_female$Average_Degree[2:17],'(',female_Average_Degree,'%',')',sep = '')
#---
female_Average_Weighted_Degree <- round(((frame_female$Average_Weighted_Degree[2:17]-frame_female$Average_Weighted_Degree[1])/frame_female$Average_Weighted_Degree[1])*100,3)
frame_female$Average_Weighted_Degree <- round(frame_female$Average_Weighted_Degree,3)
frame_female$Average_Weighted_Degree[2:17] <- paste(frame_female$Average_Weighted_Degree[2:17],'(',female_Average_Weighted_Degree,'%',')',sep = '')
#---
female_Average_Harmonic_Centrality <- round(((frame_female$Average_Harmonic_Centrality[2:17]-frame_female$Average_Harmonic_Centrality[1])/frame_female$Average_Harmonic_Centrality[1])*100,3)
frame_female$Average_Harmonic_Centrality <- round(frame_female$Average_Harmonic_Centrality,3)
frame_female$Average_Harmonic_Centrality[2:17] <- paste(frame_female$Average_Harmonic_Centrality[2:17],'(',female_Average_Harmonic_Centrality,'%',')',sep = '')
#---
frame_male <- frame_male[,-2]
frame_female <- frame_female[,-2]

#write.csv(frame_male,file = "TableS6(male_perturbation_nodes).csv")
#write.csv(frame_female,file = "TableS7(female_perturbation_nodes).csv")




