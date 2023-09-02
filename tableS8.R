rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2)
#---
male <- read.table('cos_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
female <- read.table('cos_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
set.seed(135)

#---male---
round(ecount(male)*0.01)
p1_1_male <- sample(1:ecount(male), round(ecount(male)*0.01), replace = FALSE)
p1_2_male <- sample(1:ecount(male), round(ecount(male)*0.01), replace = FALSE)
p1_3_male <- sample(1:ecount(male), round(ecount(male)*0.01), replace = FALSE)
male_P1_1 <- delete.edges(male,edges = p1_1_male)
male_P1_2 <- delete.edges(male,edges = p1_2_male)
male_P1_3 <- delete.edges(male,edges = p1_3_male)
#---
round(ecount(male)*0.03)
p3_1_male <- sample(1:ecount(male), round(ecount(male)*0.03), replace = FALSE)
p3_2_male <- sample(1:ecount(male), round(ecount(male)*0.03), replace = FALSE)
p3_3_male <- sample(1:ecount(male), round(ecount(male)*0.03), replace = FALSE)
male_P3_1 <- delete.edges(male,edges = p3_1_male)
male_P3_2 <- delete.edges(male,edges = p3_2_male)
male_P3_3 <- delete.edges(male,edges = p3_3_male)
#---
round(ecount(male)*0.05)
p5_1_male <- sample(1:ecount(male), round(ecount(male)*0.05), replace = FALSE)
p5_2_male <- sample(1:ecount(male), round(ecount(male)*0.05), replace = FALSE)
p5_3_male <- sample(1:ecount(male), round(ecount(male)*0.05), replace = FALSE)
male_P5_1 <- delete.edges(male,edges = p5_1_male)
male_P5_2 <- delete.edges(male,edges = p5_2_male)
male_P5_3 <- delete.edges(male,edges = p5_3_male)

#---female---
round(ecount(female)*0.01)
p1_1_female <- sample(1:ecount(female), round(ecount(female)*0.01), replace = FALSE)
p1_2_female <- sample(1:ecount(female), round(ecount(female)*0.01), replace = FALSE)
p1_3_female <- sample(1:ecount(female), round(ecount(female)*0.01), replace = FALSE)
female_P1_1 <- delete.edges(female,edges = p1_1_female)
female_P1_2 <- delete.edges(female,edges = p1_2_female)
female_P1_3 <- delete.edges(female,edges = p1_3_female)
#---
round(ecount(female)*0.03)
p3_1_female <- sample(1:ecount(female), round(ecount(female)*0.03), replace = FALSE)
p3_2_female <- sample(1:ecount(female), round(ecount(female)*0.03), replace = FALSE)
p3_3_female <- sample(1:ecount(female), round(ecount(female)*0.03), replace = FALSE)
female_P3_1 <- delete.edges(female,edges = p3_1_female)
female_P3_2 <- delete.edges(female,edges = p3_2_female)
female_P3_3 <- delete.edges(female,edges = p3_3_female)
#---
round(ecount(female)*0.05)
p5_1_female <- sample(1:ecount(female), round(ecount(female)*0.05), replace = FALSE)
p5_2_female <- sample(1:ecount(female), round(ecount(female)*0.05), replace = FALSE)
p5_3_female <- sample(1:ecount(female), round(ecount(female)*0.05), replace = FALSE)
female_P5_1 <- delete.edges(female,edges = p5_1_female)
female_P5_2 <- delete.edges(female,edges = p5_2_female)
female_P5_3 <- delete.edges(female,edges = p5_3_female)

#---PageRank_top10---Male--
Pagerank_male <- page_rank(male,damping = 0.5)$vector 
Page_T10_male <- Pagerank_male[order(as.numeric(Pagerank_male),decreasing = T)][1:10] %>% names()
#---random delete 1% edges three times---
Pagerank_male_P1_1 <- page_rank(male_P1_1,damping = 0.5)$vector 
Page_T10_male_P1_1 <- Pagerank_male_P1_1[order(as.numeric(Pagerank_male_P1_1),decreasing = T)][1:10] %>% names()
Pagerank_male_P1_2 <- page_rank(male_P1_2,damping = 0.5)$vector 
Page_T10_male_P1_2 <- Pagerank_male_P1_2[order(as.numeric(Pagerank_male_P1_2),decreasing = T)][1:10] %>% names()
Pagerank_male_P1_3 <- page_rank(male_P1_3,damping = 0.5)$vector 
Page_T10_male_P1_3 <- Pagerank_male_P1_3[order(as.numeric(Pagerank_male_P1_3),decreasing = T)][1:10] %>% names()
#---random delete 3% edges three times---
Pagerank_male_P3_1 <- page_rank(male_P3_1,damping = 0.5)$vector 
Page_T10_male_P3_1 <- Pagerank_male_P3_1[order(as.numeric(Pagerank_male_P3_1),decreasing = T)][1:10] %>% names()
Pagerank_male_P3_2 <- page_rank(male_P3_2,damping = 0.5)$vector 
Page_T10_male_P3_2 <- Pagerank_male_P3_2[order(as.numeric(Pagerank_male_P3_2),decreasing = T)][1:10] %>% names()
Pagerank_male_P3_3 <- page_rank(male_P3_3,damping = 0.5)$vector 
Page_T10_male_P3_3 <- Pagerank_male_P3_3[order(as.numeric(Pagerank_male_P3_3),decreasing = T)][1:10] %>% names()
#---random delete 5% edges three times---
Pagerank_male_P5_1 <- page_rank(male_P5_1,damping = 0.5)$vector 
Page_T10_male_P5_1 <- Pagerank_male_P5_1[order(as.numeric(Pagerank_male_P5_1),decreasing = T)][1:10] %>% names()
Pagerank_male_P5_2 <- page_rank(male_P5_2,damping = 0.5)$vector 
Page_T10_male_P5_2 <- Pagerank_male_P5_2[order(as.numeric(Pagerank_male_P5_2),decreasing = T)][1:10] %>% names()
Pagerank_male_P5_3 <- page_rank(male_P5_3,damping = 0.5)$vector 
Page_T10_male_P5_3 <- Pagerank_male_P5_3[order(as.numeric(Pagerank_male_P5_3),decreasing = T)][1:10] %>% names()

#---PageRank_top10---Female--
Pagerank_female <- page_rank(female,damping = 0.5)$vector 
Page_T10_female <- Pagerank_female[order(as.numeric(Pagerank_female),decreasing = T)][1:10] %>% names()
#---random delete 1% edges three times---
Pagerank_female_P1_1 <- page_rank(female_P1_1,damping = 0.5)$vector 
Page_T10_female_P1_1 <- Pagerank_female_P1_1[order(as.numeric(Pagerank_female_P1_1),decreasing = T)][1:10] %>% names()
Pagerank_female_P1_2 <- page_rank(female_P1_2,damping = 0.5)$vector 
Page_T10_female_P1_2 <- Pagerank_female_P1_2[order(as.numeric(Pagerank_female_P1_2),decreasing = T)][1:10] %>% names()
Pagerank_female_P1_3 <- page_rank(female_P1_3,damping = 0.5)$vector 
Page_T10_female_P1_3 <- Pagerank_female_P1_3[order(as.numeric(Pagerank_female_P1_3),decreasing = T)][1:10] %>% names()
#---random delete 3% edges three times---
Pagerank_female_P3_1 <- page_rank(female_P3_1,damping = 0.5)$vector 
Page_T10_female_P3_1 <- Pagerank_female_P3_1[order(as.numeric(Pagerank_female_P3_1),decreasing = T)][1:10] %>% names()
Pagerank_female_P3_2 <- page_rank(female_P3_2,damping = 0.5)$vector 
Page_T10_female_P3_2 <- Pagerank_female_P3_2[order(as.numeric(Pagerank_female_P3_2),decreasing = T)][1:10] %>% names()
Pagerank_female_P3_3 <- page_rank(female_P3_3,damping = 0.5)$vector 
Page_T10_female_P3_3 <- Pagerank_female_P3_3[order(as.numeric(Pagerank_female_P3_3),decreasing = T)][1:10] %>% names()
#---random delete 5% edges three times---
Pagerank_female_P5_1 <- page_rank(female_P5_1,damping = 0.5)$vector 
Page_T10_female_P5_1 <- Pagerank_female_P5_1[order(as.numeric(Pagerank_female_P5_1),decreasing = T)][1:10] %>% names()
Pagerank_female_P5_2 <- page_rank(female_P5_2,damping = 0.5)$vector 
Page_T10_female_P5_2 <- Pagerank_female_P5_2[order(as.numeric(Pagerank_female_P5_2),decreasing = T)][1:10] %>% names()
Pagerank_female_P5_3 <- page_rank(female_P5_3,damping = 0.5)$vector 
Page_T10_female_P5_3 <- Pagerank_female_P5_3[order(as.numeric(Pagerank_female_P5_3),decreasing = T)][1:10] %>% names()

#---as.char---
Page_T10_male <- paste(Page_T10_male, collapse = ", ")
Page_T10_male_P1_1 <- paste(Page_T10_male_P1_1, collapse = ", ")
Page_T10_male_P1_2 <- paste(Page_T10_male_P1_2, collapse = ", ")
Page_T10_male_P1_3 <- paste(Page_T10_male_P1_3, collapse = ", ")
Page_T10_male_P3_1 <- paste(Page_T10_male_P3_1, collapse = ", ")
Page_T10_male_P3_2 <- paste(Page_T10_male_P3_2, collapse = ", ")
Page_T10_male_P3_3 <- paste(Page_T10_male_P3_3, collapse = ", ")
Page_T10_male_P5_1 <- paste(Page_T10_male_P5_1, collapse = ", ")
Page_T10_male_P5_2 <- paste(Page_T10_male_P5_2, collapse = ", ")
Page_T10_male_P5_3 <- paste(Page_T10_male_P5_3, collapse = ", ")
#---
Page_T10_female <- paste(Page_T10_female, collapse = ", ")
Page_T10_female_P1_1 <- paste(Page_T10_female_P1_1, collapse = ", ")
Page_T10_female_P1_2 <- paste(Page_T10_female_P1_2, collapse = ", ")
Page_T10_female_P1_3 <- paste(Page_T10_female_P1_3, collapse = ", ")
Page_T10_female_P3_1 <- paste(Page_T10_female_P3_1, collapse = ", ")
Page_T10_female_P3_2 <- paste(Page_T10_female_P3_2, collapse = ", ")
Page_T10_female_P3_3 <- paste(Page_T10_female_P3_3, collapse = ", ")
Page_T10_female_P5_1 <- paste(Page_T10_female_P5_1, collapse = ", ")
Page_T10_female_P5_2 <- paste(Page_T10_female_P5_2, collapse = ", ")
Page_T10_female_P5_3 <- paste(Page_T10_female_P5_3, collapse = ", ")
#---
frame <- data.frame(Network=c(rep('Male network',10),rep('Female network',10)),
                    Edges=c(c(ecount(male),rep(ecount(male_P1_1),3),rep(ecount(male_P3_1),3),rep(ecount(male_P5_1),3)),c(ecount(female),rep(ecount(female_P1_1),3),rep(ecount(female_P3_1),3),rep(ecount(female_P5_1),3))),
                    Deleted_edges=c(NA,rep('1%',3),rep('3%',3),rep('5%',3),NA,rep('1%',3),rep('3%',3),rep('5%',3)),
                    Times=c(NA,rep(c(1,2,3),times=3),NA,rep(c(1,2,3),times=3)),
                    Nodes_with_the_top_10_PageRank_values=c(Page_T10_male,
                                                            Page_T10_male_P1_1,Page_T10_male_P1_2,Page_T10_male_P1_3,
                                                            Page_T10_male_P3_1,Page_T10_male_P3_2,Page_T10_male_P3_3,
                                                            Page_T10_male_P5_1,Page_T10_male_P5_2,Page_T10_male_P5_3,
                                                            Page_T10_female,
                                                            Page_T10_female_P1_1,Page_T10_female_P1_2,Page_T10_female_P1_3,
                                                            Page_T10_female_P3_1,Page_T10_female_P3_2,Page_T10_female_P3_3,
                                                            Page_T10_female_P5_1,Page_T10_female_P5_2,Page_T10_female_P5_3))

#write.csv(frame,file = "TableS8(perturbation_edges).csv")

