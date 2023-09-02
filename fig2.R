rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2)

#---(Part I) Global networks for males and females---
#---1.Male---
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male <- read.table('cos_male') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
Pre_male <- read.table('Pre_male') %>% as.vector()
female <- read.table('cos_female') %>% as.matrix() %>% graph.adjacency(mode='undirected',diag=F,weighted = T)
Pre_female <- read.table('Pre_female') %>% as.vector()

#---Deletion of nodes less than 10 degrees and prevalence rate---
remove_male <- as.numeric(which(degree(male)<10))#---91 nodes deleted for males
male <- delete.vertices(male,V(male)$name[remove_male])
Pre_male <- Pre_male[-remove_male,]
remove_female <- as.numeric(which(degree(female)<10))#---Females also delete 91 nodes
female <- delete.vertices(female,V(female)$name[remove_female])
Pre_female <- Pre_female[-remove_female,]

#---Node.size---
PRE <- data.frame(ICD=V(male)$name,Pre_male) %>% arrange(Pre_male)

#---Add label---
V(male)$name
V_male <- data.frame(ICD=V(male)$name,
          names=c('Hyperuricemia','Sleep disorder','URTI','Other respiratory disorders','Dermatitis',
              'Goitre','T2DM with renal complications','T2DM with ophthalmic complications',
              'T2DM with neurological complications','T2DM with peripheral circulatory complications',
              'T2DM with multiple complications','Hyperlipidaemia','Senile cataract','Other cataract',
              'Hypertension','CIHD','Other cardiac arrhythmias','Heart failure',
              'Complications and ill-defined descriptions of heart disease','Cerebral infarction',
              'Other cerebrovascular diseases','Sequelae of cerebrovascular disease','Atherosclerosis',
              'Vasomotor and allergic rhinitis','Chronic rhinitis, nasopharyngitis and pharyngitis',
              'Chronic bronchitis','COPD','Asthma','Chronic periodontitis','GORD','Chronic gastritis',
              'Fatty change of liver','Liver disease','Gout','Other arthritis','Spondylopathy',
              'Other specified intervertebral disc displacement','Osteoporosis','CNS',
              'CKD','Kidney failure'))
V(female)$name
V_female <- data.frame(ICD=V(female)$name,
                       names=c('Sleep disorder','Conjunctivitis','URTI','Bronchitis','Other respiratory disorders',
                               'Dermatitis','Pain in joint','Dorsalgia','Other soft tissue disorders',
                               'Urinary tract infection','Hypothyroidism','Goitre','T2DM with renal complications',
                               'T2DM with ophthalmic complications','T2DM with neurological complications',
                               'T2DM with peripheral circulatory complications','T2DM with multiple complications',
                               'Hyperlipidaemia','Other anxiety disorders','Chronic conjunctivitis',
                               'Keratoconjunctivitis sicca','Senile cataract','Other cataract','Disorders of vitreous body',
                               'Other disorders of optic [2nd] nerve and visual pathways','Hypertension','CIHD',
                               'Other cardiac arrhythmias','Heart failure','Complications and ill-defined descriptions of heart disease',
                               'Cerebral infarction','Other cerebrovascular diseases','Atherosclerosis',
                               'Vasomotor and allergic rhinitis','Chronic rhinitis, nasopharyngitis and pharyngitis',
                               'Chronic bronchitis','COPD','Asthma','Chronic periodontitis','GORD','Chronic gastritis',
                               'Fatty change of liver','Liver disease','Other arthritis','Gonarthrosis',
                               'Spondylosis','Spondylopathy','Other specified intervertebral disc displacement',
                               'Osteoporosis','CNS','CKD','Kidney failure','Fibroadenosis of breast','Menopausal and other perimenopausal disorders'))

#---mapping of male network---
#---V1.Node size (by prevalence)
V(male)$size <- sqrt(Pre_male)*10
#---V2.Edge color (gray)
V(male)$frame.color <- rep('#4F4F4F',vcount(male))
#---V3.Edge width (0.1)
V(male)$frame.width <- rep(0.1,vcount(male))
#---V4.Node color (by chapter)
V(male)$color <- case_when(str_detect(V(male)$name,'E')~'#205AA7',
                             str_detect(V(male)$name,'F')~'#C4FCEF',
                             str_detect(V(male)$name,'G')~'#FF6347',
                             str_detect(V(male)$name,'H')~'#B15B00',
                             str_detect(V(male)$name,'I')~'#D13A28',
                             str_detect(V(male)$name,'J')~'#FFC75F',
                             str_detect(V(male)$name,'K')~'#9F79EE',
                             str_detect(V(male)$name,'L')~'#F9F871',
                             str_detect(V(male)$name,'M')~'#4B4453',
                             TRUE ~ '#00CD00')
#---V5.Node label
V(male)$diseases <- V_male$names
#---V6.Label size
V(male)$label.size <- 0.5

#---E1.Edge width mapping (by weight/SCI)
E(male)$width <- 0
E(male)$width[E(male)$weight<=0.2] <- 0.1
E(male)$width[E(male)$weight>0.2&E(male)$weight<=0.4] <- 0.3
E(male)$width[E(male)$weight>0.4&E(male)$weight<=0.6] <- 0.7
E(male)$width[E(male)$weight>0.6] <- 1.2

#---E2.Edge color mapping
E(male)$color <- NA
E(male)$color[E(male)$weight<=0.2] <- '#99D1D3'
E(male)$color[E(male)$weight>0.2&E(male)$weight<=0.4] <- '#6EC3C9'
E(male)$color[E(male)$weight>0.4&E(male)$weight<=0.6] <- '#00B2BF'
E(male)$color[E(male)$weight>0.6] <- '#00A6AD'

#---mapping of male network---
#---V1.Node size (by prevalence)
V(female)$size <- sqrt(Pre_female)*10
#---V2.Edge color (gray)
V(female)$frame.color <- rep('#4F4F4F',vcount(female))
#---V3.Edge width (0.1)
V(female)$frame.width <- rep(0.1,vcount(female))
#---V4.Node color (by chapter)
V(female)$color <- case_when(str_detect(V(female)$name,'E')~'#205AA7',
                             str_detect(V(female)$name,'F')~'#C4FCEF',
                             str_detect(V(female)$name,'G')~'#FF6347',
                             str_detect(V(female)$name,'H')~'#B15B00',
                             str_detect(V(female)$name,'I')~'#D13A28',
                             str_detect(V(female)$name,'J')~'#FFC75F',
                             str_detect(V(female)$name,'K')~'#9F79EE',
                             str_detect(V(female)$name,'L')~'#F9F871',
                             str_detect(V(female)$name,'M')~'#4B4453',
                             TRUE ~ '#00CD00')
#---V5.Node label
V(female)$diseases <- V_female$names
#---V6.Label size
V(female)$label.size <- 0.5

#---E1.Edge width mapping (by weight/SCI)
E(female)$width <- 0
E(female)$width[E(female)$weight<=0.2] <- 0.1
E(female)$width[E(female)$weight>0.2&E(female)$weight<=0.4] <- 0.3
E(female)$width[E(female)$weight>0.4&E(female)$weight<=0.6] <- 0.7
E(female)$width[E(female)$weight>0.6] <- 1.2

#---E2.Edge color mapping
E(female)$color <- NA
E(female)$color[E(female)$weight<=0.2] <- '#D2A6C7'
E(female)$color[E(female)$weight>0.2&E(female)$weight<=0.4] <- '#C57CAC'
E(female)$color[E(female)$weight>0.4&E(female)$weight<=0.6] <- '#AF4A92'
E(female)$color[E(female)$weight>0.6] <- '#A2007C'

#---Plotting fig2a---
#pdf("fig2a.pdf", width = 25, height = 15)
par(mfrow = c(1, 2))
#---
plot(male,layout=layout.kamada.kawai,
     vertex.size = V(male)$size,
     vertex.frame.color = V(male)$frame.color,
     vertex.frame.width = V(male)$frame.width,
     vertex.color = V(male)$color,
     vertex.label=V(male)$diseases,
     vertex.label.cex=V(male)$label.size,
     vertex.label.dist=1,
     vertex.label.color='#363636',
     edge.width = E(male)$width,
     edge.color=E(male)$color)
#---
plot(female,layout=layout.kamada.kawai,
     vertex.size = V(female)$size,
     vertex.frame.color = V(female)$frame.color,
     vertex.frame.width = V(female)$frame.width,
     vertex.color = V(female)$color,
     vertex.label=V(female)$diseases,
     vertex.label.cex=V(female)$label.size,
     vertex.label.dist=1,
     vertex.label.color='#363636',
     edge.width = E(female)$width,
     edge.color=E(female)$color)
#dev.off()

#---Drawing labels fig2b---
rm(list=ls(all=T))
#pdf("fig2b.pdf", width = 10, height = 10)
par(mfrow = c(1, 1))
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),axes = FALSE)
legend(x = 0,
       y = 1,
       pch=21,
       col='#363636',
       legend = c('E00-E90\nEndocrine, nutritional and metabolic diseases',
                  'F00-F99\nMental, Behavioral and Neurodevelopmental disorders',
                  'G00-G99\nDiseases of the nervous system',
                  'H00-H95\nDiseases of the eye, adnexa, ear, and mastoid process',
                  'I00-I99\nDiseases of the circulatory system',
                  'J00-J99\nDiseases of the respiratory system',
                  'K00-K93\nDiseases of the digestive system',
                  'L00-L99\nDiseases of the skin and subcutaneous tissue',
                  'M00-M99\nDiseases of the musculoskeletal system and connective tissue',
                  'N00-N99\nDiseases of the genitourinary system'),
       pt.bg = c('#205AA7','#C4FCEF','#FF6347','#B15B00','#D13A28','#FFC75F','#9F79EE','#F9F871','#4B4453','#00CD00'),
       cex = 1,
       pt.cex = 1.5)
#dev.off()

#---Plotting the weighting legend fig2c---
rm(list=ls(all=T))
#pdf("fig2c.pdf", width = 10, height = 10)
par(mfrow = c(1, 1))
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),axes = FALSE)
legend('top',legend = c('~0.2','0.2~0.4','0.4~0.6','0.6~','~0.2','0.2~0.4','0.4~0.6','0.6~'),
       lty = 1,lwd=3,
       col=c('#99D1D3','#6EC3C9','#00B2BF','#00A6AD','#D2A6C7','#C57CAC','#AF4A92','#A2007C'))
#dev.off()

#---Plotting prevalence legend fig2d---
rm(list=ls(all=T))
#pdf("fig2d.pdf", width = 10, height = 10)
par(mfrow = c(1, 1))
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),axes = FALSE)
legend('top',legend = c('~0.2','0.2~0.4','0.4~0.6','0.6~'),
       pch=21,
       col='black',pt.cex=2)
#dev.off()

