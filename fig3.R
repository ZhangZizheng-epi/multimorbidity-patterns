rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2);library(cowplot);library(ggunchained)

#---Read 12 networks for age-sex subgroups----
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

#---Figure 3A (Nodes trends)----
Nodes_male <- c(vcount(male_A1),vcount(male_A2),vcount(male_A3),vcount(male_A4),vcount(male_A5),vcount(male_A6))
Nodes_female <- c(vcount(female_A1),vcount(female_A2),vcount(female_A3),vcount(female_A4),vcount(female_A5),vcount(female_A6))
Frame_Nodes <- data.frame(Sex=factor(c(rep('Male',6),rep('Female',6)),levels = c('Male','Female'),ordered=T),
                          Age=factor(c('<40','40-49','50-59','60-69','70-79','≥80','<40','40-49','50-59','60-69','70-79','≥80'),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                          Number_Nodes=c(Nodes_male,Nodes_female))

g1 <- ggplot(Frame_Nodes, aes(x = Age, y = Number_Nodes, fill= Sex)) +
  geom_bar(stat = 'identity',position = 'dodge',width = 0.3) +
  scale_fill_manual(values = c("#6EC3C9","#C57CAC")) +
  scale_y_continuous(limits = c(0,120),breaks = c(0,30,60,90,120))+
  xlab('Age group (Years)') +
  ylab('Number of Nodes') +
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.3),
        panel.grid.minor= element_line(linetype = 2,size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.22,0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(fill=guide_legend(nrow=1))

#---Figure 3B (Edges trends)---
Edges_male <- c(ecount(male_A1),ecount(male_A2),ecount(male_A3),ecount(male_A4),ecount(male_A5),ecount(male_A6))
Edges_female <- c(ecount(female_A1),ecount(female_A2),ecount(female_A3),ecount(female_A4),ecount(female_A5),ecount(female_A6))
Frame_Edges <- data.frame(Sex=factor(c(rep('Male',6),rep('Female',6)),levels = c('Male','Female'),ordered=T),
                          Age=factor(c('<40','40-49','50-59','60-69','70-79','≥80','<40','40-49','50-59','60-69','70-79','≥80'),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                          Number_Edges=c(Edges_male,Edges_female))

g2 <- ggplot(Frame_Edges, aes(Age,Number_Edges,fill=Sex)) +
  geom_bar(stat = 'identity',position = 'dodge',width = 0.3) +
  scale_fill_manual(values = c("#6EC3C9","#C57CAC")) +
  scale_y_continuous(limits = c(0,550),breaks = c(0,100,200,300,400,500))+
  xlab('Age group (Years)') +
  ylab('Number of Edges') +
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.3),
        panel.grid.minor= element_line(linetype = 2,size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.22,0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(fill=guide_legend(nrow=1))

#---Figure 3C (density trends)---
Density_male <- c(graph.density(male_A1),graph.density(male_A2),graph.density(male_A3),graph.density(male_A4),graph.density(male_A5),graph.density(male_A6))
Density_female <- c(graph.density(female_A1),graph.density(female_A2),graph.density(female_A3),graph.density(female_A4),graph.density(female_A5),graph.density(female_A6))
Frame_Density <- data.frame(Sex=factor(rep(c('Male','Female'),each=6),levels = c('Male','Female'),ordered = T),
                            Age=factor(c('<40','40-49','50-59','60-69','70-79','≥80','<40','40-49','50-59','60-69','70-79','≥80'),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                            Density=c(Density_male,Density_female))

g3 <- ggplot(Frame_Density, aes(Age,Density,group=Sex,color=Sex,shape=Sex)) +
  geom_line(size=0.2,linetype=2) +
  geom_point(size=2.5) +
  scale_color_manual(values = c('#426EB4', '#AF4A92')) +
  scale_y_continuous(limits = c(0,0.1),breaks = c(0,0.02,0.04,0.06,0.08,0.10))+
  xlab('Age group (Years)') +
  ylab('Density') +
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.3),
        panel.grid.minor= element_line(linetype = 2,size = 0.3),
        legend.position = c(0.6,0.2),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(color=guide_legend(nrow=1))

#---Figure 3D1 (Trends for Average Degree)---
Degree_male <- c(mean(degree(male_A1)),mean(degree(male_A2)),mean(degree(male_A3)),mean(degree(male_A4)),mean(degree(male_A5)),mean(degree(male_A6)))
Degree_female <- c(mean(degree(female_A1)),mean(degree(female_A2)),mean(degree(female_A3)),mean(degree(female_A4)),mean(degree(female_A5)),mean(degree(female_A6)))
Frame_Degree <- data.frame(Sex=factor(rep(c('Male','Female'),each=6),levels = c('Male','Female'),ordered = T),
                           Age=factor(c('<40','40-49','50-59','60-69','70-79','≥80','<40','40-49','50-59','60-69','70-79','≥80'),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                           Degree=c(Degree_male,Degree_female))

g4a <- ggplot(Frame_Degree, aes(Age,Degree,group=Sex,color=Sex,shape=Sex)) +
  geom_line(size=0.2,linetype=2) +
  geom_point(size=2.5) +
  scale_color_manual(values = c('#426EB4', '#AF4A92')) +
  scale_y_continuous(limits = c(0,10),breaks = c(0,2,4,6,8,10))+
  xlab('Age group (Years)') +
  ylab('Average Degree') +
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.3),
        panel.grid.minor= element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.6,0.2),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(color=guide_legend(nrow=1))

#---Figure 3D2 (Trends for Average Weighted Degree)---
Strength_male <- c(mean(strength(male_A1)),mean(strength(male_A2)),mean(strength(male_A3)),mean(strength(male_A4)),mean(strength(male_A5)),mean(strength(male_A6)))
Strength_female <- c(mean(strength(female_A1)),mean(strength(female_A2)),mean(strength(female_A3)),mean(strength(female_A4)),mean(strength(female_A5)),mean(strength(female_A6)))
Frame_Strength <- data.frame(Sex=factor(rep(c('Male','Female'),each=6),levels = c('Male','Female'),ordered = T),
                             Age=factor(c('<40','40-49','50-59','60-69','70-79','≥80','<40','40-49','50-59','60-69','70-79','≥80'),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                             Strength=c(Strength_male,Strength_female))

g4b <- ggplot(Frame_Strength, aes(Age,Strength,group=Sex,color=Sex,shape=Sex)) +
  geom_line(size=0.2,linetype=2) +
  geom_point(size=2.5) +
  scale_color_manual(values = c('#426EB4', '#AF4A92')) +
  scale_y_continuous(limits = c(0,2.5),breaks = c(0,0.5,1,1.5,2,2.5))+
  xlab('Age group (Years)') +
  ylab('Average Weighted Degree') +
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.3),
        panel.grid.minor= element_blank(),
        legend.position = c(0.6,0.2),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(color=guide_legend(nrow=1))
g4 <- plot_grid(g4a,g4b,nrow = 2,align = 'hv')

#---Figure 3E (Trends for Average Closeness Centrality)------
#===Constructing shortest path networks===
male_length_A1 <- male_A1;E(male_length_A1)$weight <- 1/E(male_length_A1)$weight;male_length_A2 <- male_A2;E(male_length_A2)$weight <- 1/E(male_length_A2)$weight
male_length_A3 <- male_A3;E(male_length_A3)$weight <- 1/E(male_length_A3)$weight;male_length_A4 <- male_A4;E(male_length_A4)$weight <- 1/E(male_length_A4)$weight
male_length_A5 <- male_A5;E(male_length_A5)$weight <- 1/E(male_length_A5)$weight;male_length_A6 <- male_A6;E(male_length_A6)$weight <- 1/E(male_length_A6)$weight
female_length_A1 <- female_A1;E(female_length_A1)$weight <- 1/E(female_length_A1)$weight;female_length_A2 <- female_A2;E(female_length_A2)$weight <- 1/E(female_length_A2)$weight
female_length_A3 <- female_A3;E(female_length_A3)$weight <- 1/E(female_length_A3)$weight;female_length_A4 <- female_A4;E(female_length_A4)$weight <- 1/E(female_length_A4)$weight
female_length_A5 <- female_A5;E(female_length_A5)$weight <- 1/E(female_length_A5)$weight;female_length_A6 <- female_A6;E(female_length_A6)$weight <- 1/E(female_length_A6)$weight
#---
Frame_harmonic <- data.frame(Sex=factor(rep(c('Male','Female'),each=6),levels = c('Male','Female'),ordered = T),
                             Age=factor(c('<40','40-49','50-59','60-69','70-79','≥80','<40','40-49','50-59','60-69','70-79','≥80'),levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                             Harmonic=c(mean(harmonic_centrality(male_length_A1,normalized = T)),mean(harmonic_centrality(male_length_A2,normalized = T)),mean(harmonic_centrality(male_length_A3,normalized = T)),mean(harmonic_centrality(male_length_A4,normalized = T)),mean(harmonic_centrality(male_length_A5,normalized = T)),mean(harmonic_centrality(male_length_A6,normalized = T)),mean(harmonic_centrality(female_length_A1,normalized = T)),mean(harmonic_centrality(female_length_A2,normalized = T)),mean(harmonic_centrality(female_length_A3,normalized = T)),mean(harmonic_centrality(female_length_A4,normalized = T)),mean(harmonic_centrality(female_length_A5,normalized = T)),mean(harmonic_centrality(female_length_A6,normalized = T))))

g5 <- ggplot(Frame_harmonic, aes(Age,Harmonic,group=Sex,color=Sex,shape=Sex)) +
  geom_line(size=0.2,linetype=2) +
  geom_point(size=2.5) +
  scale_color_manual(values = c('#4169E1', '#CD2626')) +
  scale_y_continuous(limits = c(0,0.14),breaks = c(0,0.04,0.08,0.12))+
  xlab('Age group (Years)') +
  ylab('Average Harmonic Centrality') +
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.3),
        panel.grid.minor= element_line(linetype = 2,size = 0.3),
        legend.position = c(0.6,0.2),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(color=guide_legend(nrow=1))

#---Figure 3F (Trends for Weight/SCI)------
Cos_male <- c(E(male_A1)$weight,E(male_A2)$weight,E(male_A3)$weight,E(male_A4)$weight,E(male_A5)$weight,E(male_A6)$weight)
Cos_female <- c(E(female_A1)$weight,E(female_A2)$weight,E(female_A3)$weight,E(female_A4)$weight,E(female_A5)$weight,E(female_A6)$weight)
Frame_Weights <- data.frame(Sex= factor(c(rep('Male',length(Cos_male)),rep('Female',length(Cos_female))),levels = c('Male','Female'),ordered = T),
                            Age= factor(c(rep('<40',ecount(male_A1)),rep('40-49',ecount(male_A2)),rep('50-59',ecount(male_A3)),
                                          rep('60-69',ecount(male_A4)),rep('70-79',ecount(male_A5)),rep('≥80',ecount(male_A6)),
                                          rep('<40',ecount(female_A1)),rep('40-49',ecount(female_A2)),rep('50-59',ecount(female_A3)),
                                          rep('60-69',ecount(female_A4)),rep('70-79',ecount(female_A5)),rep('≥80',ecount(female_A6))),
                                          levels=c('<40','40-49','50-59','60-69','70-79','≥80'),ordered=T),
                            Weights=c(Cos_male,Cos_female))

g6 <- ggplot(Frame_Weights,aes(Age,Weights,fill=Sex)) +
  geom_split_violin(scale = 'count',trim = F,adjust=0.8,lwd=0.1)+
  scale_fill_manual(values = c("#1E90FF","#F1AF00"))+
  geom_boxplot(aes(color=Sex), fill='white', lwd=0.05,width=0.15, outlier.colour = NA, position = position_dodge(0.5)) +
  stat_summary(aes(color=Sex),fun=median, geom='point',shape=19, size=0.2, position = position_dodge(0.5)) +
  scale_color_manual(values = c('black', 'black'))+
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  xlab('Age group (Years)') +
  ylab('Salton Cosine Index')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = c(0.22,0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))+
  guides(fill=guide_legend(nrow=1))

#---Figure 3G1 (Degree trajectories for nodes of male networks)---
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
g7 <- ggplot(Frame_burst_male,aes(Age,Degree,group=ICD,color=ICD))+
  geom_line(size=0.2)+
  geom_point(size=0.5)+
  xlab('Age group (Years)') +
  ylab('Degree of Nodes')+
  scale_x_discrete(expand = c(0.05, 0.05))+
  scale_y_continuous(limits = c(0,90),breaks = c(0,15,30,45,60,75,90))+
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.2),
        panel.grid.minor= element_line(linetype = 2,size = 0.2),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.key.size = unit(4,'mm'),
        legend.text = element_text(size=5))+
  guides(color=guide_legend(ncol=1))+
  scale_color_manual(values = c("#6495ED", "#7B68EE", "#1E90FF",'#4682B4','#00CED1',
                                '#2E8B57','#FFD700','#B22222','#FFA07A','#FF4500',
                                '#C71585','#8B8378','#9AC0CD','#B4EEB4','#64004B',
                                '#77FFCC','#CD853F'))

#---Figure 3G2 (Degree trajectories for nodes of female networks)---
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
g8 <- ggplot(Frame_burst_female,aes(Age,Degree,group=ICD,color=ICD))+
  geom_line(size=0.2)+
  geom_point(size=0.5)+
  xlab('Age group (Years)') +
  ylab('Degree of Nodes')+
  scale_x_discrete(expand = c(0.05, 0.05))+
  scale_y_continuous(limits = c(0,90),breaks = c(0,15,30,45,60,75,90))+
  theme_bw()+
  theme(panel.grid.major= element_line(linetype = 2,size=0.2),
        panel.grid.minor= element_line(linetype = 2,size = 0.2),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.key.size = unit(4,'mm'),
        legend.text = element_text(size=5))+
  guides(color=guide_legend(ncol=1))+
  scale_color_manual(values = c('#6495ED','#1E90FF','#2E8B57','#BDB76B','#BC8F8F',
                                '#FFD700','#B22222','#FFA07A','#FF4500','#FFB6C1',
                                '#C71585','#8B8378','#9AC0CD','#B4EEB4','#008B8B',
                                '#7AC5CD','#8C63A4','#64004B','#FFA488','#77FFCC',
                                '#9F88FF','#5500FF','#E8CCFF','#FF00FF','#CD853F',
                                '#FFDAB9','#00FF7F'))

#---Merge 8 figures ---
#pdf("fig3(Compount Trend).pdf", width = 14, height = 10)
plot_grid(g1,g2,g3,g4,g5,g6,g7,g8,nrow = 2,rel_heights = c(1,1),align = 'hv')
#dev.off()
