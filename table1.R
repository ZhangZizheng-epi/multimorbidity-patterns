rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2);library(corrplot);library(gridExtra);library(cowplot)
#---
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male <- read.table('cos_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female <- read.table('cos_female')%>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)

#---Community Detection and Root Disease----
#---【Take the average of multiple times community detection counts;male-6;female-7】
set.seed(123456)
com_male <- cluster_louvain(male)
com_female <- cluster_louvain(female)

#---Male---
sizes(com_male)
communities(com_male)[[1]]
com_male_C1 <- delete.vertices(male,setdiff(V(male)$name,communities(com_male)[[1]]))
com_male_C2 <- delete.vertices(male,setdiff(V(male)$name,communities(com_male)[[2]]))
com_male_C3 <- delete.vertices(male,setdiff(V(male)$name,communities(com_male)[[3]]))
com_male_C4 <- delete.vertices(male,setdiff(V(male)$name,communities(com_male)[[4]]))
com_male_C5 <- delete.vertices(male,setdiff(V(male)$name,communities(com_male)[[5]]))
com_male_C6 <- delete.vertices(male,setdiff(V(male)$name,communities(com_male)[[6]]))
root_m1 <- eigen_centrality(com_male_C1,scale = F)$vector[order(as.numeric(eigen_centrality(com_male_C1,scale = F)$vector),decreasing = T)][1] %>% names()
root_m2 <- eigen_centrality(com_male_C2,scale = F)$vector[order(as.numeric(eigen_centrality(com_male_C2,scale = F)$vector),decreasing = T)][1] %>% names()
root_m3 <- eigen_centrality(com_male_C3,scale = F)$vector[order(as.numeric(eigen_centrality(com_male_C3,scale = F)$vector),decreasing = T)][1] %>% names()
root_m4 <- eigen_centrality(com_male_C4,scale = F)$vector[order(as.numeric(eigen_centrality(com_male_C4,scale = F)$vector),decreasing = T)][1] %>% names()
root_m5 <- eigen_centrality(com_male_C5,scale = F)$vector[order(as.numeric(eigen_centrality(com_male_C5,scale = F)$vector),decreasing = T)][1] %>% names()
root_m6 <- eigen_centrality(com_male_C6,scale = F)$vector[order(as.numeric(eigen_centrality(com_male_C6,scale = F)$vector),decreasing = T)][1] %>% names()
root_male <- c(root_m1,root_m2,root_m3,root_m4,root_m5,root_m6)

#---Female---
sizes(com_female)
communities(com_female)[[1]]
com_female_C1 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[1]]))
com_female_C2 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[2]]))
com_female_C3 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[3]]))
com_female_C4 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[4]]))
com_female_C5 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[5]]))
com_female_C6 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[6]]))
com_female_C7 <- delete.vertices(female,setdiff(V(female)$name,communities(com_female)[[7]]))
root_f1 <- eigen_centrality(com_female_C1,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C1,scale = F)$vector),decreasing = T)][1] %>% names()
root_f2 <- eigen_centrality(com_female_C2,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C2,scale = F)$vector),decreasing = T)][1] %>% names()
root_f3 <- eigen_centrality(com_female_C3,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C3,scale = F)$vector),decreasing = T)][1] %>% names()
root_f4 <- eigen_centrality(com_female_C4,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C4,scale = F)$vector),decreasing = T)][1] %>% names()
root_f5 <- eigen_centrality(com_female_C5,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C5,scale = F)$vector),decreasing = T)][1] %>% names()
root_f6 <- eigen_centrality(com_female_C6,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C6,scale = F)$vector),decreasing = T)][1] %>% names()
root_f7 <- eigen_centrality(com_female_C7,scale = F)$vector[order(as.numeric(eigen_centrality(com_female_C7,scale = F)$vector),decreasing = T)][1] %>% names()
root_female <- c(root_f1,root_f2,root_f3,root_f4,root_f5,root_f6,root_f7)
intersect(root_female,root_male)
plot(com_male_C5)
#---
sizes(com_male);sizes(com_female)
#---
Table <- data.frame(Sex=c(rep('Male',6),rep('Female',7)),
           Community=c('M1','M2','M3','M4','M5','M6','F1','F2','F3','F4','F5','F6','F7'),
           Nodes=c(vcount(com_male_C1),vcount(com_male_C2),vcount(com_male_C3),
                   vcount(com_male_C4),vcount(com_male_C5),vcount(com_male_C6),
                   vcount(com_female_C1),vcount(com_female_C2),vcount(com_female_C3),
                   vcount(com_female_C4),vcount(com_female_C5),vcount(com_female_C6),
                   vcount(com_female_C7)),
           Edges=c(ecount(com_male_C1),ecount(com_male_C2),ecount(com_male_C3),
                   ecount(com_male_C4),ecount(com_male_C5),ecount(com_male_C6),
                   ecount(com_female_C1),ecount(com_female_C2),ecount(com_female_C3),
                   ecount(com_female_C4),ecount(com_female_C5),ecount(com_female_C6),
                   ecount(com_female_C7)),
           Root=c(root_m1,root_m2,root_m3,root_m4,root_m5,root_m6,
                  root_f1,root_f2,root_f3,root_f4,root_f5,root_f6,root_f7),
           Member=c(paste(V(com_male_C1)$name,collapse=', '),
                    paste(V(com_male_C2)$name,collapse=', '),
                    paste(V(com_male_C3)$name,collapse=', '),
                    paste(V(com_male_C4)$name,collapse=', '),
                    paste(V(com_male_C5)$name,collapse=', '),
                    paste(V(com_male_C6)$name,collapse=', '),
                    paste(V(com_female_C1)$name,collapse=', '),
                    paste(V(com_female_C2)$name,collapse=', '),
                    paste(V(com_female_C3)$name,collapse=', '),
                    paste(V(com_female_C4)$name,collapse=', '),
                    paste(V(com_female_C5)$name,collapse=', '),
                    paste(V(com_female_C6)$name,collapse=', '),
                    paste(V(com_female_C7)$name,collapse=', ')))
#write.csv(Table,file = "Table1(community).csv")





