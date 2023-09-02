rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2)
#---Top 10 comorbidity pairs with the highest SCI in different age groups---

#---Males----
#---less than 40 male---
male <- read.table('cos_male_A1') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_male <- as_data_frame(male,'edge')
E_male <- E_male[order(E_male$weight,decreasing = T),]
E_male <- E_male[substr(E_male$from,1,1)!=substr(E_male$to,1,1),c(1:3)]
E_male <- E_male[c(1:10),]
A1_male <- data.frame(A1=paste(E_male$from,'--',E_male$to,sep = ''),SCI=round(E_male$weight,2))
#---40~49 male---
male <- read.table('cos_male_A2') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_male <- as_data_frame(male,'edge')
E_male <- E_male[order(E_male$weight,decreasing = T),]
E_male <- E_male[substr(E_male$from,1,1)!=substr(E_male$to,1,1),c(1:3)]
E_male <- E_male[c(1:10),]
A2_male <- data.frame(A2=paste(E_male$from,'--',E_male$to,sep = ''),SCI=round(E_male$weight,2))
#---50~59 male---
male <- read.table('cos_male_A3') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_male <- as_data_frame(male,'edge')
E_male <- E_male[order(E_male$weight,decreasing = T),]
E_male <- E_male[substr(E_male$from,1,1)!=substr(E_male$to,1,1),c(1:3)]
E_male <- E_male[c(1:10),]
A3_male <- data.frame(A3=paste(E_male$from,'--',E_male$to,sep = ''),SCI=round(E_male$weight,2))
#---60~69 male---
male <- read.table('cos_male_A4') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_male <- as_data_frame(male,'edge')
E_male <- E_male[order(E_male$weight,decreasing = T),]
E_male <- E_male[substr(E_male$from,1,1)!=substr(E_male$to,1,1),c(1:3)]
E_male <- E_male[c(1:10),]
A4_male <- data.frame(A4=paste(E_male$from,'--',E_male$to,sep = ''),SCI=round(E_male$weight,2))
#---70~79 male---
male <- read.table('cos_male_A5') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_male <- as_data_frame(male,'edge')
E_male <- E_male[order(E_male$weight,decreasing = T),]
E_male <- E_male[substr(E_male$from,1,1)!=substr(E_male$to,1,1),c(1:3)]
E_male <- E_male[c(1:10),]
A5_male <- data.frame(A5=paste(E_male$from,'--',E_male$to,sep = ''),SCI=round(E_male$weight,2))
#---Greater than or equal to 80 male---
male <- read.table('cos_male_A6') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_male <- as_data_frame(male,'edge')
E_male <- E_male[order(E_male$weight,decreasing = T),]
E_male <- E_male[substr(E_male$from,1,1)!=substr(E_male$to,1,1),c(1:3)]
E_male <- E_male[c(1:10),]
A6_male <- data.frame(A6=paste(E_male$from,'--',E_male$to,sep = ''),SCI=round(E_male$weight,2))

#---Females----
#---less than 40 female---
female <- read.table('cos_female_A1') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_female <- as_data_frame(female,'edge')
E_female <- E_female[order(E_female$weight,decreasing = T),]
E_female <- E_female[substr(E_female$from,1,1)!=substr(E_female$to,1,1),c(1:3)]
E_female <- E_female[c(1:10),]
A1_female <- data.frame(A1=paste(E_female$from,'--',E_female$to,sep = ''),SCI=round(E_female$weight,2))
#---40~49 female---
female <- read.table('cos_female_A2') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_female <- as_data_frame(female,'edge')
E_female <- E_female[order(E_female$weight,decreasing = T),]
E_female <- E_female[substr(E_female$from,1,1)!=substr(E_female$to,1,1),c(1:3)]
E_female <- E_female[c(1:10),]
A2_female <- data.frame(A2=paste(E_female$from,'--',E_female$to,sep = ''),SCI=round(E_female$weight,2))
#---50~59 female---
female <- read.table('cos_female_A3') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_female <- as_data_frame(female,'edge')
E_female <- E_female[order(E_female$weight,decreasing = T),]
E_female <- E_female[substr(E_female$from,1,1)!=substr(E_female$to,1,1),c(1:3)]
E_female <- E_female[c(1:10),]
A3_female <- data.frame(A3=paste(E_female$from,'--',E_female$to,sep = ''),SCI=round(E_female$weight,2))
#---60~69 female---
female <- read.table('cos_female_A4') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_female <- as_data_frame(female,'edge')
E_female <- E_female[order(E_female$weight,decreasing = T),]
E_female <- E_female[substr(E_female$from,1,1)!=substr(E_female$to,1,1),c(1:3)]
E_female <- E_female[c(1:10),]
A4_female <- data.frame(A4=paste(E_female$from,'--',E_female$to,sep = ''),SCI=round(E_female$weight,2))
#---70~79 female---
female <- read.table('cos_female_A5') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_female <- as_data_frame(female,'edge')
E_female <- E_female[order(E_female$weight,decreasing = T),]
E_female <- E_female[substr(E_female$from,1,1)!=substr(E_female$to,1,1),c(1:3)]
E_female <- E_female[c(1:10),]
A5_female <- data.frame(A5=paste(E_female$from,'--',E_female$to,sep = ''),SCI=round(E_female$weight,2))
#---Greater than or equal to 80 female---
female <- read.table('cos_female_A6') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
E_female <- as_data_frame(female,'edge')
E_female <- E_female[order(E_female$weight,decreasing = T),]
E_female <- E_female[substr(E_female$from,1,1)!=substr(E_female$to,1,1),c(1:3)]
E_female <- E_female[c(1:10),]
A6_female <- data.frame(A6=paste(E_female$from,'--',E_female$to,sep = ''),SCI=round(E_female$weight,2))

#---Merge---
M <- cbind(A1_male,A2_male,A3_male,A4_male,A5_male,A6_male)
FF <- cbind(A1_female,A2_female,A3_female,A4_female,A5_female,A6_female)
Table <- rbind(M,FF)
Sex <- as.data.frame(c(rep('Male',10),rep('Female',10)))
colnames(Sex) <- 'Sex'
Table <- cbind(Sex,Table)
#write.csv(Table,file = "TableS3(Top10Pairs).csv")


