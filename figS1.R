rm(list=ls(all=T))
setwd('d://network//')
library(data.table);library(tidyverse);library(lubridate);library(igraph);library(ggplot2);library(corrplot);library(gridExtra);library(cowplot)
#---
name_J <- read.table('name_acute') 
name_M <- read.table('name_chronic')
male <- read.table('cos_male') %>% as.matrix()
female <- read.table('cos_female')%>% as.matrix() 
male_new <- c('A49',"B02","B18.1","B35","B36","C18","C22","C34","C61","C73","C78",
              'D64.9',"E03.9","E04","E05","E06","E11.2","E11.3","E11.4","E11.5","E11.7",
              "E66.9","E78","E79.0","E87","F03","F32","F41","F51.9","G20",
              "G31","G45","G47.9","G62","H10.4","H10.9","H11","H16.22","H25",
              "H26","H35","H40.9","H43","H47","H60.9","H93.1","I10","I20","I21","I24",
              "I25","I44","I48","I49","I50.9","I51","I63","I65","I67","I69","I70","I83",
              "J02","J06","J15","J18.9","J20","J30","J31","J32","J37","J40","J42",
              "J43","J44.9","J45.9","J47","J94","J96.9","J98","K02","K04","K05.3","K21",
              "K25.9","K29.3","K29.4","K29.5","K29.6","K29.7","K30","K52.9","K59.9",
              "K63","K72.9","K73","K74","K76.0","K76.8","K76.9","K80.2","K81.9",
              "K92","L08","L21","L23","L28","L29","L30.9","L40","L50","M10","M13","M17",
              "M19","M25.5","M47","M48.0","M48.9","M50.2","M51.2","M54","M79","M81",
              "N03","N13","N18","N19","N20","N28.1","N39.0","N39.9")
female_new <- c("A09","A49","B02","B07","B18.1","B35","C18","C22","C34","C50","C73","C78",
                "D64.9","E03.9","E04","E05","E06","E11.2","E11.3","E11.4","E11.5","E11.7",
                "E66.9","E78","E79.0","E87","F03","F32","F41","F48","F51.9","G20",
                "G31","G45","G47.9","G62","H10.4","H10.9","H11","H16.22","H25","H26",
                "H35","H40.9","H43","H47","H60.9","H81.9","H93.1","I10","I20","I21","I24","I25",
                "I44","I48","I49","I50.9","I51","I63","I65","I67","I69","I70",'I83',"J02","J06",
                "J15","J18.9","J20","J30","J31","J32","J37","J40","J42","J44.9","J45.9","J47",
                "J94","J96.9","J98","K02","K04","K05.3","K12","K21","K25.9","K29.3","K29.4",
                "K29.5","K29.6","K29.7","K30","K52.9","K59.9","K63","K72.9","K73","K74","K76.0",
                "K76.8","K76.9","K80.2","K81.9",'K92',"L08","L21","L23","L28","L29",
                "L30.9","L50","M06","M10","M13","M15","M17","M19","M25.5","M47","M48.0",
                "M48.9","M50.2","M51.2","M54","M65","M75","M79",'M81',"N03","N18","N19",
                "N20","N28.1","N39.0","N39.9","N60.2","N63","N64","N76.0","N76.1","N92","N95")

#---Arrange the rows and columns of the matrix in alphabetical order---
male <- male[male_new,male_new]
female <- female[female_new,female_new]

#---Male heat map---
male_melt <- melt(male, varnames = c("row", "col"), value.name = "SCI")
heat_m <- ggplot(male_melt, aes(col,row, fill = SCI)) +
          geom_tile(color = "white", size = 0.1) +
          scale_fill_gradient(low = "#CAE5E8", high = "#00A6AD")+
          coord_equal()+
  annotate(geom = "rect",xmin=0.5,xmax=1.5,ymin=0.5,ymax=1.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=1.5,xmax=5.5,ymin=1.5,ymax=5.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=5.5,xmax=11.5,ymin=5.5,ymax=11.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=11.5,xmax=12.5,ymin=11.5,ymax=12.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=12.5,xmax=25.5,ymin=12.5,ymax=25.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=25.5,xmax=29.5,ymin=25.5,ymax=29.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=29.5,xmax=34.5,ymin=29.5,ymax=34.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=34.5,xmax=46.5,ymin=34.5,ymax=46.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=46.5,xmax=62.5,ymin=46.5,ymax=62.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=62.5,xmax=80.5,ymin=62.5,ymax=80.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=80.5,xmax=103.5,ymin=80.5,ymax=103.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=103.5,xmax=111.5,ymin=103.5,ymax=111.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=111.5,xmax=124.5,ymin=111.5,ymax=124.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=124.5,xmax=132.5,ymin=124.5,ymax=132.5,color='#9C9C9C',fill=NA,size=0.3)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5,size = 2.5),
                axis.text.y = element_text(hjust = 0.5,size = 2.5),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                legend.position = "right") 

#---Female heat map---
female_melt <- melt(female, varnames = c("row", "col"), value.name = "SCI")
heat_f <- ggplot(female_melt, aes(col,row, fill = SCI)) +
          geom_tile(color = "white", size = 0.1) +
          scale_fill_gradient(low = "#E8D3E3", high = "#A2007C")+
          coord_equal()+
  annotate(geom = "rect",xmin=0.5,xmax=2.5,ymin=0.5,ymax=2.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=2.5,xmax=6.5,ymin=2.5,ymax=6.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=6.5,xmax=12.5,ymin=6.5,ymax=12.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=12.5,xmax=13.5,ymin=12.5,ymax=13.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=13.5,xmax=26.5,ymin=13.5,ymax=26.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=26.5,xmax=31.5,ymin=26.5,ymax=31.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=31.5,xmax=36.5,ymin=31.5,ymax=36.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=36.5,xmax=49.5,ymin=36.5,ymax=49.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=49.5,xmax=65.5,ymin=49.5,ymax=65.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=65.5,xmax=82.5,ymin=65.5,ymax=82.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=82.5,xmax=106.5,ymin=82.5,ymax=106.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=106.5,xmax=113.5,ymin=106.5,ymax=113.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=113.5,xmax=130.5,ymin=113.5,ymax=130.5,color='#9C9C9C',fill=NA,size=0.3)+
  annotate(geom = "rect",xmin=130.5,xmax=144.5,ymin=130.5,ymax=144.5,color='#9C9C9C',fill=NA,size=0.3)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5,size = 2.5),
                axis.text.y = element_text(hjust = 0.5,size = 2.5),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                legend.position = "right") 

#---Merge Heat Map---
#pdf("figS1(heatMap).pdf", width = 12, height = 12)
plot_grid(heat_m,heat_f,nrow = 1,rel_heights = c(1,1))
#dev.off()

