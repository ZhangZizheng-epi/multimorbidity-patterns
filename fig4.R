rm(list=ls(all=T))
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2);library(gridExtra);library(cowplot);library(ggVennDiagram)
#---
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male <- read.table('cos_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female <- read.table('cos_female')%>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)

#---1.Hub diseases----
Pagerank_male <- page_rank(male,damping = 0.5)$vector 
Pagerank_female <- page_rank(female,damping = 0.5)$vector 
Page_T10_male <- Pagerank_male[order(as.numeric(Pagerank_male),decreasing = T)][1:10]
Page_T10_female <- Pagerank_female[order(as.numeric(Pagerank_female),decreasing = T)][1:10]
Page_T10_male <- names(Page_T10_male)
Page_T10_female <- names(Page_T10_female)

#---2.Community detection----
set.seed(123456)
com_male <- cluster_louvain(male)
com_female <- cluster_louvain(female)

#---Males---
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

#---Females---
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

#---3.Burst disease----
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')

#---Constructing Data frames for trajectories of degrees of male networks---
male_A1 <- read.table('cos_male_A1') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A2 <- read.table('cos_male_A2') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A3 <- read.table('cos_male_A3') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A4 <- read.table('cos_male_A4') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A5 <- read.table('cos_male_A5') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_A6 <- read.table('cos_male_A6') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
Frame_male <- data.frame(ICD=c(colnames(name_J),colnames(name_M)))
Frame_male <- merge(Frame_male,data.frame(ICD=names(degree(male_A1)),K_male_A1=degree(male_A1)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A2)),K_male_A2=degree(male_A2)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A3)),K_male_A3=degree(male_A3)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A4)),K_male_A4=degree(male_A4)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A5)),K_male_A5=degree(male_A5)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(male_A6)),K_male_A6=degree(male_A6)),all.x = T) 
Frame_male <- Frame_male[rowSums(Frame_male[,c(2:7)],na.rm = T)!=0,]

#----Constructing Data frames for trajectories of degrees of female networks---
female_A1 <- read.table('cos_female_A1') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A2 <- read.table('cos_female_A2') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A3 <- read.table('cos_female_A3') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A4 <- read.table('cos_female_A4') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A5 <- read.table('cos_female_A5') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_A6 <- read.table('cos_female_A6') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
Frame_female <- data.frame(ICD=c(colnames(name_J),colnames(name_M)))
Frame_female <- merge(Frame_female,data.frame(ICD=names(degree(female_A1)),K_female_A1=degree(female_A1)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A2)),K_female_A2=degree(female_A2)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A3)),K_female_A3=degree(female_A3)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A4)),K_female_A4=degree(female_A4)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A5)),K_female_A5=degree(female_A5)),all.x = T) %>% 
  merge(data.frame(ICD=names(degree(female_A6)),K_female_A6=degree(female_A6)),all.x = T) 
Frame_female <- Frame_female[rowSums(Frame_female[,c(2:7)],na.rm = T)!=0,]

#---Filtering nodes with a degree increase more than or equle to 6 in neighboring age groups---
threshold <- 6
Frame_male <- Frame_male[which((Frame_male[,3]-Frame_male[,2])>=threshold|
                                 (Frame_male[,4]-Frame_male[,3])>=threshold|
                                 (Frame_male[,5]-Frame_male[,4])>=threshold|
                                 (Frame_male[,6]-Frame_male[,5])>=threshold|
                                 (Frame_male[,7]-Frame_male[,6])>=threshold),]
Frame_female <- Frame_female[which((Frame_female[,3]-Frame_female[,2])>=threshold|
                                     (Frame_female[,4]-Frame_female[,3])>=threshold|
                                     (Frame_female[,5]-Frame_female[,4])>=threshold|
                                     (Frame_female[,6]-Frame_female[,5])>=threshold|
                                     (Frame_female[,7]-Frame_female[,6])>=threshold),]
burst_male <- Frame_male$ICD
burst_female <- Frame_female$ICD

#---Plotting venn diagrams---
g1 <- ggVennDiagram(x=list(Page_T10_male,root_male,burst_male),
              label_alpha = 0,edge_colour=c('black','black','black'),
              category.names = c('Hub','Root','Burst'),
              edge_size=0.1,label = 'count')+
  scale_color_manual(values = c('#898989','#898989','#898989'))+
  scale_fill_gradient(low = 'white',high='#99D1D3')

g2 <- ggVennDiagram(x=list(Page_T10_female,root_female,burst_female),
              label_alpha = 0,edge_colour=c('black','black','black'),
              category.names = c('Hub','Root','Burst'),
              edge_size=0.1,label = 'count')+
  scale_color_manual(values = c('#898989','#898989','#898989'))+
  scale_fill_gradient(low = 'white',high='#D2A6C7')

#----Save---
#pdf("fig4(Venn Diagram).pdf", width = 12, height = 8)
plot_grid(g1,g2,scale = 0.9)
#dev.off()

