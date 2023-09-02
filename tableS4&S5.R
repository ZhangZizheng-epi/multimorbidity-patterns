rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2);library(cowplot);library(ggunchained)

#---Read age&sex subgroup data----
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male_A1 <- read.table('cos_male_A1') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A2 <- read.table('cos_male_A2') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A3 <- read.table('cos_male_A3') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A4 <- read.table('cos_male_A4') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A5 <- read.table('cos_male_A5') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A6 <- read.table('cos_male_A6') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A1 <- read.table('cos_female_A1') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A2 <- read.table('cos_female_A2') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A3 <- read.table('cos_female_A3') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A4 <- read.table('cos_female_A4') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A5 <- read.table('cos_female_A5') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A6 <- read.table('cos_female_A6') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)

#---Degree trajectories for nodes of male networks---
Frame_male <- data.frame(ICD=c(colnames(name_J),colnames(name_M)))
Frame_male <- merge(Frame_male,data.frame(ICD=names(degree(male_A1)),K_male_A1=degree(male_A1)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A2)),K_male_A2=degree(male_A2)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A3)),K_male_A3=degree(male_A3)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A4)),K_male_A4=degree(male_A4)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A5)),K_male_A5=degree(male_A5)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A6)),K_male_A6=degree(male_A6)),all.x = T) 
Frame_male <- Frame_male[rowSums(Frame_male[,c(2:7)],na.rm = T)!=0,]
#---
threshold <- 6
Frame_male <- Frame_male[which((Frame_male[,3]-Frame_male[,2])>=threshold|
                                 (Frame_male[,4]-Frame_male[,3])>=threshold|
                                 (Frame_male[,5]-Frame_male[,4])>=threshold|
                                 (Frame_male[,6]-Frame_male[,5])>=threshold|
                                 (Frame_male[,7]-Frame_male[,6])>=threshold),]
Frame_male[is.na(Frame_male)] <- 0
#---Constructing the trajectory data frame---
Frame_burst_male <- data.frame(Age=factor(rep(c('<40','40-49','50-59','60-69','70-79','≥80'),nrow(Frame_male)),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                               Degree=as.vector(t(as.matrix(Frame_male[,c(2:7)]))),
                               ICD=factor(rep(Frame_male$ICD,each=6)))

#---Degree trajectories for nodes of female networks---
Frame_female <- data.frame(ICD=c(colnames(name_J),colnames(name_M)))
Frame_female <- merge(Frame_female,data.frame(ICD=names(degree(female_A1)),K_female_A1=degree(female_A1)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A2)),K_female_A2=degree(female_A2)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A3)),K_female_A3=degree(female_A3)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A4)),K_female_A4=degree(female_A4)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A5)),K_female_A5=degree(female_A5)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A6)),K_female_A6=degree(female_A6)),all.x = T) 
Frame_female <- Frame_female[rowSums(Frame_female[,c(2:7)],na.rm = T)!=0,]
#---
threshold <- 6
Frame_female <- Frame_female[which((Frame_female[,3]-Frame_female[,2])>=threshold|
                                     (Frame_female[,4]-Frame_female[,3])>=threshold|
                                     (Frame_female[,5]-Frame_female[,4])>=threshold|
                                     (Frame_female[,6]-Frame_female[,5])>=threshold|
                                     (Frame_female[,7]-Frame_female[,6])>=threshold),]
Frame_female[is.na(Frame_female)] <- 0
#---Constructing the trajectory data frame---
Frame_burst_female <- data.frame(Age=factor(rep(c('<40','40-49','50-59','60-69','70-79','≥80'),nrow(Frame_female)),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                                 Degree=as.vector(t(as.matrix(Frame_female[,c(2:7)]))),
                                 ICD=factor(rep(Frame_female$ICD,each=6)))
rm(list = setdiff(ls(),c('Frame_male','Frame_female')))

#write.csv(Frame_male,file = "TableS4(MaleBurst).csv")
#write.csv(Frame_female,file = "TableS5(FemaleBurst).csv")

