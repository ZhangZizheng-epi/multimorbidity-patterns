rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2);library(cowplot);library(fmsb)

#---Read age&sex subgroup data----
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male_15 <- read.table('cos_total_15_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_16 <- read.table('cos_total_16_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_17 <- read.table('cos_total_17_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_18 <- read.table('cos_total_18_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_19 <- read.table('cos_total_19_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_20 <- read.table('cos_total_20_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
male_21 <- read.table('cos_total_21_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_15 <- read.table('cos_total_15_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_16 <- read.table('cos_total_16_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_17 <- read.table('cos_total_17_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_18 <- read.table('cos_total_18_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_19 <- read.table('cos_total_19_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_20 <- read.table('cos_total_20_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female_21 <- read.table('cos_total_21_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)

#---Extraction of nodes common to both male and female patients---
male_15 <- delete.vertices(male_15,setdiff(V(male_15)$name,intersect(V(male_15)$name,V(female_15)$name)))
female_15 <- delete.vertices(female_15,setdiff(V(female_15)$name,intersect(V(male_15)$name,V(female_15)$name)))
male_16 <- delete.vertices(male_16,setdiff(V(male_16)$name,intersect(V(male_16)$name,V(female_16)$name)))
female_16 <- delete.vertices(female_16,setdiff(V(female_16)$name,intersect(V(male_16)$name,V(female_16)$name)))
male_17 <- delete.vertices(male_17,setdiff(V(male_17)$name,intersect(V(male_17)$name,V(female_17)$name)))
female_17 <- delete.vertices(female_17,setdiff(V(female_17)$name,intersect(V(male_17)$name,V(female_17)$name)))
male_18 <- delete.vertices(male_18,setdiff(V(male_18)$name,intersect(V(male_18)$name,V(female_18)$name)))
female_18 <- delete.vertices(female_18,setdiff(V(female_18)$name,intersect(V(male_18)$name,V(female_18)$name)))
male_19 <- delete.vertices(male_19,setdiff(V(male_19)$name,intersect(V(male_19)$name,V(female_19)$name)))
female_19 <- delete.vertices(female_19,setdiff(V(female_19)$name,intersect(V(male_19)$name,V(female_19)$name)))
male_20 <- delete.vertices(male_20,setdiff(V(male_20)$name,intersect(V(male_20)$name,V(female_20)$name)))
female_20 <- delete.vertices(female_20,setdiff(V(female_20)$name,intersect(V(male_20)$name,V(female_20)$name)))
male_21 <- delete.vertices(male_21,setdiff(V(male_21)$name,intersect(V(male_21)$name,V(female_21)$name)))
female_21 <- delete.vertices(female_21,setdiff(V(female_21)$name,intersect(V(male_21)$name,V(female_21)$name)))

#---Year-by-year common node extraction 
#(incomplete for years 15 and 16, complete for years 17-21, total of 16 diseases)
#ICD refers to 16 "three categories of crucial diseases" that are common to both male and female patients
ICD <- c("E04","E11.4","E78","H26","I10","I25","I49","I63","I70","J31","J42","K29.5","K76.0","M13","M81","N18" )
ICD_15 <- intersect(V(male_15)$name,ICD)
ICD_16 <- intersect(V(male_16)$name,ICD)
ICD_17 <- intersect(V(male_17)$name,ICD)
ICD_18 <- intersect(V(male_18)$name,ICD)
ICD_19 <- intersect(V(male_19)$name,ICD)
ICD_20 <- intersect(V(male_20)$name,ICD)
ICD_21 <- intersect(V(male_21)$name,ICD)

#---Radar chart for 2015----
M_15 <- degree(male_15,v = ICD_15) %>% matrix(nrow = 1) %>% as.data.frame()
F_15 <- degree(female_15,v = ICD_15) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_15 <- rbind(M_15,F_15)
colnames(Frame_15) <- ICD_15
rownames(Frame_15) <- c('Male','Female')
max_min <- data.frame(E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0), J31=c(70, 0), 
                      K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_15 <- rbind(max_min, Frame_15)

#---Save--
#pdf("fig5_15.pdf", width = 12, height = 8)
radarchart(Frame_15, axistype = 1,seg=7,
  pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
  plwd = 1.5, plty = 1,
  cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
  vlcex = 1, vlabels = colnames(Frame_15),
  caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

#---Radar chart for 2016---
M_16 <- degree(male_16,v = ICD_16) %>% matrix(nrow = 1) %>% as.data.frame()
F_16 <- degree(female_16,v = ICD_16) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_16 <- rbind(M_16,F_16)
colnames(Frame_16) <- ICD_16
rownames(Frame_16) <- c('Male','Female')
max_min <- data.frame(E04=c(70, 0),E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0),I70=c(70, 0), J31=c(70, 0), 
                      J42=c(70, 0),K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_16 <- rbind(max_min, Frame_16)

#---Save--
#pdf("fig5_16.pdf", width = 12, height = 8)
radarchart(Frame_16, axistype = 1,seg = 7,
           pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
           plwd = 1.5, plty = 1,
           cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
           vlcex = 1, vlabels = colnames(Frame_16),
           caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

#---Radar chart for 2017---
M_17 <- degree(male_17,v = ICD_17) %>% matrix(nrow = 1) %>% as.data.frame()
F_17 <- degree(female_17,v = ICD_17) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_17 <- rbind(M_17,F_17)
colnames(Frame_17) <- ICD_17
rownames(Frame_17) <- c('Male','Female')
max_min <- data.frame(E04=c(70, 0),E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0),I70=c(70, 0), J31=c(70, 0), 
                      J42=c(70, 0),K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),M81=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_17 <- rbind(max_min, Frame_17)

#---Save--
#pdf("fig5_17.pdf", width = 12, height = 8)
radarchart(Frame_17, axistype = 1,seg = 7,
           pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
           plwd = 1.5, plty = 1,
           cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
           vlcex = 1, vlabels = colnames(Frame_17),
           caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

#---Radar chart for 2018---
M_18 <- degree(male_18,v = ICD_18) %>% matrix(nrow = 1) %>% as.data.frame()
F_18 <- degree(female_18,v = ICD_18) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_18 <- rbind(M_18,F_18)
colnames(Frame_18) <- ICD_18
rownames(Frame_18) <- c('Male','Female')
max_min <- data.frame(E04=c(70, 0),E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0),I70=c(70, 0), J31=c(70, 0), 
                      J42=c(70, 0),K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),M81=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_18 <- rbind(max_min, Frame_18)

#---Save--
#pdf("fig5_18.pdf", width = 12, height = 8)
radarchart(Frame_18, axistype = 1,seg = 7,
           pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
           plwd = 1.5, plty = 1,
           cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
           vlcex = 1, vlabels = colnames(Frame_18),
           caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

#---Radar chart for 2019---
M_19 <- degree(male_19,v = ICD_19) %>% matrix(nrow = 1) %>% as.data.frame()
F_19 <- degree(female_19,v = ICD_19) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_19 <- rbind(M_19,F_19)
colnames(Frame_19) <- ICD_19
rownames(Frame_19) <- c('Male','Female')
max_min <- data.frame(E04=c(70, 0),E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0),I70=c(70, 0), J31=c(70, 0), 
                      J42=c(70, 0),K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),M81=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_19 <- rbind(max_min, Frame_19)

#---Save--
#pdf("fig5_19.pdf", width = 12, height = 8)
radarchart(Frame_19, axistype = 1,seg = 7,
           pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
           plwd = 1.5, plty = 1,
           cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
           vlcex = 1, vlabels = colnames(Frame_19),
           caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

#---Radar chart for 2020---
M_20 <- degree(male_20,v = ICD_20) %>% matrix(nrow = 1) %>% as.data.frame()
F_20 <- degree(female_20,v = ICD_20) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_20 <- rbind(M_20,F_20)
colnames(Frame_20) <- ICD_20
rownames(Frame_20) <- c('Male','Female')
max_min <- data.frame(E04=c(70, 0),E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0),I70=c(70, 0), J31=c(70, 0), 
                      J42=c(70, 0),K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),M81=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_20 <- rbind(max_min, Frame_20)

#---Save--
#pdf("fig5_20.pdf", width = 12, height = 8)
radarchart(Frame_20, axistype = 1,seg = 7,
           pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
           plwd = 1.5, plty = 1,
           cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
           vlcex = 1, vlabels = colnames(Frame_20),
           caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

#---Radar chart for 2021---
M_21 <- degree(male_21,v = ICD_21) %>% matrix(nrow = 1) %>% as.data.frame()
F_21 <- degree(female_21,v = ICD_21) %>% matrix(nrow = 1) %>% as.data.frame()
Frame_21 <- rbind(M_21,F_21)
colnames(Frame_21) <- ICD_21
rownames(Frame_21) <- c('Male','Female')
max_min <- data.frame(E04=c(70, 0),E11.4=c(70, 0), E78=c(70, 0), H26=c(70, 0),I10=c(70, 0), 
                      I25=c(70, 0), I49=c(70, 0),I63=c(70, 0),I70=c(70, 0), J31=c(70, 0), 
                      J42=c(70, 0),K29.5=c(70, 0),K76.0=c(70, 0),M13=c(70, 0),M81=c(70, 0),N18=c(70, 0))
rownames(max_min) <- c("Max", "Min")
Frame_21 <- rbind(max_min, Frame_21)

#---Save--
#pdf("fig5_21.pdf", width = 12, height = 8)
radarchart(Frame_21, axistype = 1,seg = 7,
           pcol = c("#00AFBB", "#E7B800"), pfcol = scales::alpha(c("#00AFBB", "#E7B800"),0.2), 
           plwd = 1.5, plty = 1,
           cglcol = "#B5B5B5", cglty = 2, cglwd = 0.5,axislabcol = "#4F4F4F", 
           vlcex = 1, vlabels = colnames(Frame_21),
           caxislabels = c(0,10,20,30,40,50,60,70))
par("usr")
rect(xleft = -1.35, xright = 1.35,ybottom = -1.29,ytop = 1.29, border="black", lwd=0.5)
legend(
  x = "right", legend = rownames(Frame_15[-c(1,2),]), horiz = F,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),
  text.col = "black", cex = 1, pt.cex = 2
)
#dev.off()

