#---Male patient Multimorbidity network as an example
#---Constructing a matrix of SCIs for male patients
rm(list=ls(all=T))
library(data.table);library(tidyverse);library(lubridate);library(igraph)
Disease <- fread('\\\\DESKTOP-APL06RP\\download\\Zhangzz\\net_backup\\Disease.csv',sep = '\001',quote = '',header = T,stringsAsFactors = T,encoding = 'UTF-8')
Logic_J <- fread('Logic_JJ')
Logic_M <- fread('Logic_MM')

#=== Part1:Constructing three category of matrix for determining significant SCI ===
#---Take the male patient and the corresponding logical value of the disease---
Loc_male <- which(Disease$sex==1)
Disease <- Disease[Loc_male,]
Logic_J <- Logic_J[Loc_male,]
Logic_M <- Logic_M[Loc_male,]

#---1 SCI Matrix of coexistence of acute diseases---
#---1.1 Construct three identical matrices for holding SCI,t-value and Nab(Nij)--- 
#---Note: Acute diseases coexistence was not included in the analysis
cha_j <- names(Logic_J)
M_jj1 <- matrix(0,nrow=length(cha_j),ncol=length(cha_j),dimnames = list(cha_j,cha_j)) %>% as.data.frame()
M_jj2 <- matrix(0,nrow=length(cha_j),ncol=length(cha_j),dimnames = list(cha_j,cha_j)) %>% as.data.frame()
M_jj3 <- matrix(0,nrow=length(cha_j),ncol=length(cha_j),dimnames = list(cha_j,cha_j)) %>% as.data.frame()
list_j <- as.list(Logic_J)

#---1.2 Function construction: 
#---For determining the coexistence of any pair of acute disease---
#---Rule: any two acute disease with a onset time interval of less than or equal to 10 days---
FUN_JJ <- function(x,y){
  a = x[!is.na(x)]
  b = y[!is.na(y)]
  SUM = 0
  for (i in 1:length(a)) {
    N = sum(abs(time_length(interval(a[i],b),'day'))<=10)
    SUM = SUM + N
    SUM = sum(SUM!=0)
  }
  SUM
}

#---1.3 Assign values to the three matrices---
Time <- Disease$zdsj
N_total = unique(Disease$kh_zh) %>% length() %>% as.numeric()
for (i in 1:length(list_j)) {
  for (j in 1:length(list_j)) {
    Disease_ = Disease
    Ni = unique(Disease_[list_j[[i]],'kh_zh']) %>% nrow()%>% as.numeric()
    Nj = unique(Disease_[list_j[[j]],'kh_zh']) %>% nrow()%>% as.numeric()
    NiNj = Ni * Nj
    Na = Ni
    Nb = Nj
    TimeA <- Time
    TimeB <- Time
    TimeA[!list_j[[i]]] = NA
    TimeB[!list_j[[j]]] = NA
    Disease_$A = TimeA
    Disease_$B = TimeB
    inter = intersect(unique(Disease_[list_j[[i]],'kh_zh']),unique(Disease_[list_j[[j]],'kh_zh']))
    Disease_ = Disease_[Disease_$kh_zh%in%inter$kh_zh]
    Group <- group_by(Disease_,kh_zh) %>% summarise(Nij=FUN_JJ(A,B))
    Nij = sum(Group$Nij,na.rm = T)%>% as.numeric()
    Nab = Nij
    M_jj1[i,j] = Nij/sqrt(NiNj)
    phi = (Nab*N_total-Na*Nb)/sqrt((Na*Nb*(N_total-Na)*(N_total-Nb)))
    M_jj2[i,j] = (phi*sqrt(Nab-2))/sqrt(1-phi^2)  
    M_jj3[i,j] = Nab   
  }
}
#---
write.table(M_jj1,file = 'M_jj_cos_male',row.names = T)
write.table(M_jj2,file = 'M_jj_tvalue_male',row.names = T)
write.table(M_jj3,file = 'M_jj_Nab_male',row.names = T)
rm(list = setdiff(ls(),c('Disease','Logic_J','Logic_M')))


#---2 SCI Matrix of coexistence of acute and chronic diseases---
#---2.1 Construct three identical matrices for holding SCI,t-value and Nab(Nij)--- 
cha_j <- names(Logic_J)
cha_m <- names(Logic_M)
M_jm1 <- matrix(0,nrow=length(cha_j),ncol=length(cha_m),dimnames = list(cha_j,cha_m)) %>% as.data.frame()
M_jm2 <- matrix(0,nrow=length(cha_j),ncol=length(cha_m),dimnames = list(cha_j,cha_m)) %>% as.data.frame()
M_jm3 <- matrix(0,nrow=length(cha_j),ncol=length(cha_m),dimnames = list(cha_j,cha_m)) %>% as.data.frame()
list_j <- as.list(Logic_J)
list_m <- as.list(Logic_M)

#---2.2 Function construction: 
#---for determining the coexistence of any one acute disease and any one chronic disease---
#---Rule: any one acute disease appears later than any one chronic disease---
FUN_JM <- function(x,y){
  a = x[!is.na(x)]
  b = y[1]
  SUM = sum(a >= b)
  SUM = sum(SUM!=0)
  SUM
}

#---2.3 Assign values to the three matrices---
N_total = unique(Disease$kh_zh) %>% length() %>% as.numeric()
for (i in 1:length(list_j)) {
  for (j in 1:length(list_m)) {
    Disease_ = Disease
    inter = intersect(unique(Disease_[list_j[[i]],'kh_zh']),unique(Disease_[list_m[[j]],'kh_zh']))
    Ni = unique(Disease_[list_j[[i]],'kh_zh']) %>% nrow()%>% as.numeric() 
    Nj = unique(Disease_[list_m[[j]],'kh_zh']) %>% nrow()%>% as.numeric()
    Na = Ni
    Nb = Nj
    NiNj = Ni * Nj
    DF_X = Disease_[list_m[[j]],] %>% arrange(zdsj) %>% distinct(kh_zh,.keep_all = T) %>% rename(X=zdsj) %>% select(kh_zh,X)
    TimeA = Disease_$zdsj
    TimeA[!list_j[[i]]] = NA
    Disease_$A = TimeA
    Disease_ = Disease_ %>% select(kh_zh,A)
    Disease_ = Disease_[Disease_$kh_zh%in%inter$kh_zh]
    Disease_ = merge(Disease_,DF_X,by = 'kh_zh',all.x = T)  
    Group = group_by(Disease_,kh_zh) %>% summarise(Nij=FUN_JM(A,X))
    Nij = sum(Group$Nij,na.rm = T)%>% as.numeric()
    Nab = Nij
    phi = (Nab*N_total-Na*Nb)/sqrt((Na*Nb*(N_total-Na)*(N_total-Nb)))
    M_jm1[i,j] = Nij/sqrt(NiNj)
    M_jm2[i,j] = (phi*sqrt(Nab-2))/sqrt(1-phi^2)  
    M_jm3[i,j] = Nab   
  }
}
#---
write.table(M_jm1,file = 'M_jm_cos_male',row.names = T)
write.table(M_jm2,file = 'M_jm_tvalue_male',row.names = T)
write.table(M_jm3,file = 'M_jm_Nab_male',row.names = T)
rm(list = setdiff(ls(),c('Disease','Logic_J','Logic_M')))

#---3 SCI Matrix of coexistence of chronic diseases---
#---3.1 Construct three identical matrices for holding SCI,t-value and Nab(Nij)--- 
cha_m <- names(Logic_M)
M_mm1 <- matrix(0,nrow=length(cha_m),ncol=length(cha_m),dimnames = list(cha_m,cha_m)) %>% as.data.frame()
M_mm2 <- matrix(0,nrow=length(cha_m),ncol=length(cha_m),dimnames = list(cha_m,cha_m)) %>% as.data.frame()
M_mm3 <- matrix(0,nrow=length(cha_m),ncol=length(cha_m),dimnames = list(cha_m,cha_m)) %>% as.data.frame()
list_m <- as.list(Logic_M)

#---3.2 Assign values to the three matrices---
N_total = unique(Disease$kh_zh) %>% length() %>% as.numeric()
for (i in 1:length(list_m)) {
  for (j in 1:length(list_m)) {
    Ni = unique(Disease[list_m[[i]],]$kh_zh)
    Nj = unique(Disease[list_m[[j]],]$kh_zh)
    Na = as.numeric(length(Ni))
    Nb = as.numeric(length(Nj))
    Nab = intersect(Ni,Nj) %>% length() %>% as.numeric()
    Nij = intersect(Ni,Nj) %>% length() %>% as.numeric()
    NiNj = as.numeric(length(Ni))*as.numeric(length(Nj))
    phi = (Nab*N_total-Na*Nb)/sqrt((Na*Nb*(N_total-Na)*(N_total-Nb)))
    M_mm1[i,j] = Nij/sqrt(NiNj)
    M_mm2[i,j] = (phi*sqrt(Nab-2))/sqrt(1-phi^2)
    M_mm3[i,j] = Nab
  }
}
#---
write.table(M_mm1,file = 'M_mm_cos_male',row.names = T)
write.table(M_mm2,file = 'M_mm_tvalue_male',row.names = T)
write.table(M_mm3,file = 'M_mm_Nab_male',row.names = T)
rm(list = setdiff(ls(),c('Disease','Logic_J','Logic_M')))

#---Combining the matrices for the three cases: obtaining the total SCI matrix---
M_jj_cos_male <- read.table('M_jj_cos_male') %>% as.matrix()
diag(M_jj_cos_male) <- 0
M_jj_cos_male <- as.data.frame(M_jj_cos_male)
M_jm_cos_male <- read.table('M_jm_cos_male') 
M_mm_cos_male <- read.table('M_mm_cos_male') %>% as.matrix()
diag(M_mm_cos_male) <- 0
M_mm_cos_male <- as.data.frame(M_mm_cos_male)
M_all_cos_male <- rbind(cbind(M_jj_cos_male,M_jm_cos_male),cbind(t(M_jm_cos_male),M_mm_cos_male))
write.table(M_all_cos_male,file = 'M_all_cos_male',row.names = T)
rm(list = setdiff(ls(),c('Disease','Logic_J','Logic_M')))

#---Combining the matrices for the three cases: obtaining the total t-value matrix---
M_jj_tvalue_male <- read.table('M_jj_tvalue_male') %>% as.matrix()
diag(M_jj_tvalue_male) <- 0
M_jj_tvalue_male <- as.data.frame(M_jj_tvalue_male)
M_jm_tvalue_male <- read.table('M_jm_tvalue_male') 
M_mm_tvalue_male <- read.table('M_mm_tvalue_male') %>% as.matrix()
diag(M_mm_tvalue_male) <- 0
M_mm_tvalue_male <- as.data.frame(M_mm_tvalue_male)
M_all_tvalue_male <- rbind(cbind(M_jj_tvalue_male,M_jm_tvalue_male),cbind(t(M_jm_tvalue_male),M_mm_tvalue_male))
write.table(M_all_tvalue_male,file = 'M_all_tvalue_male',row.names = T)
rm(list = setdiff(ls(),c('Disease','Logic_J','Logic_M')))

#---Combining the matrices for the three cases: obtaining the total Nab(Nij) matrix---
M_jj_Nab_male <- read.table('M_jj_Nab_male') %>% as.matrix()
diag(M_jj_Nab_male) <- 0
M_jj_Nab_male <- as.data.frame(M_jj_Nab_male)
M_jm_Nab_male <- read.table('M_jm_Nab_male') 
M_mm_Nab_male <- read.table('M_mm_Nab_male') %>% as.matrix()
diag(M_mm_Nab_male) <- 0
M_mm_Nab_male <- as.data.frame(M_mm_Nab_male)
M_all_Nab_male <- rbind(cbind(M_jj_Nab_male,M_jm_Nab_male),cbind(t(M_jm_Nab_male),M_mm_Nab_male))
write.table(M_all_Nab_male,file = 'M_all_Nab_male',row.names = T)
rm(list = setdiff(ls(),c('Disease','Logic_J','Logic_M')))

#=== Part2:Determining significant SCI ===
M_all_cos_male <- read.table('M_all_cos_male') %>% as.matrix()
M_all_tvalue_male <- read.table('M_all_tvalue_male') %>% as.matrix()
M_all_Nab_male <- read.table('M_all_Nab_male') %>% as.matrix()

#---step1:Delete 0-value rows or columns---
remove <- which(is.na(M_all_cos_male[1,]))%>% as.numeric() %>% c()
M_all_cos_male <- M_all_cos_male[-remove,-remove]
M_all_tvalue_male <- M_all_tvalue_male[-remove,-remove]
M_all_Nab_male <- M_all_Nab_male[-remove,-remove]
rm(remove)

#---step2:Calculation of the number of minimal disease pairs(Cutoff_Nab)--- 644.15
Nab <- M_all_Nab_male
diag(Nab) <- NA
Nab[lower.tri(M_all_Nab_male)] <- NA
Nab <- as.vector(Nab[!is.na(Nab)])
Nab_with_0 <- which(Nab==0) %>% length() %>% as.numeric()
N <- (nrow(M_all_Nab_male)*(nrow(M_all_Nab_male)-1))/2 %>% as.numeric()
P <- N - Nab_with_0
Cutoff_Nab <- sum(Nab/P)
rm(N,Nab,Nab_with_0,P)

#---step3:t-value and Nab converted to equal-length vectors for determining Cutoff_SCI---
tvalue <- M_all_tvalue_male
tvalue[is.na(tvalue)] <- 0
diag(tvalue) <- NA
tvalue[lower.tri(tvalue)] <- NA
tvalue <- tvalue[!is.na(tvalue)] %>% as.vector()
#---
Nab <- M_all_Nab_male
diag(Nab) <- NA
Nab[lower.tri(Nab)] <- NA
Nab <- Nab[!is.na(Nab)] %>% as.vector()

#---Results obtained: 
#---the first 703 SCI values in descending order are statistically significant---
loc_t <- which(tvalue>2.58)
loc_N <- which(Nab>Cutoff_Nab)
loc <- intersect(loc_t,loc_N) %>% length() %>% as.numeric()

#---step4:Take the SCI cut-off by the place obtained(703)---
#---Result obtained: cut-off for SCI is 0.1276---
Cos <- M_all_cos_male
diag(Cos) <- NA
Cos[lower.tri(Cos)] <- NA
Cos <- Cos[!is.na(Cos)] %>% as.vector()
Cos <- Cos[order(Cos,decreasing = T)]
Cutoff_Cos <- Cos[loc]

#---step5:SCI values for cut-offs less than SCI are assigned 0---
M_all_cos_male[M_all_cos_male<Cutoff_Cos] <- 0

#---step6:Delete meaningless nodes (nodes that are not connected to any node)---
#---The final network consisting of 132 nodes is obtained (39 nodes eliminated)---
remove <- which(colSums(M_all_cos_male)==0) %>% as.numeric() %>% c()
M_all_cos_male <- M_all_cos_male[-remove,-remove]

