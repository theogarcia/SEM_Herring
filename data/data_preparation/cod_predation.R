library(Hmisc) #Lag function
library(dplyr) 
###################################### Load data #######################################################

COD<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/COD.RData"))
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGIDE.RData")
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/AFWG.RData")

#################################### data manip #####################################################
COD$Age0<-as.numeric(as.character(COD$Age0))
COD$Age1<-as.numeric(as.character(COD$Age1))
sub2<-WGIDE_good[,c(1,2)]
sub3<-AFWG_good[,c(1,24)]
dat<-Reduce(function(x,y) merge(x = x, y = y, by = "years",all=T), list(sub2,sub3,COD))
dat$H_0<-Lag(dat$H_0,+2)#Lag

abb.pos<-which((dat$H_R2/dat$H_0)>1)
dat[which((dat$H_R2/dat$H_0)>1),3]<-NA #Enlève aberration H0 (3 years)

dat$Tot<-Lag(dat$Age0,+2)+Lag(dat$Age1,+1)#Pertes pour chaque cohorte(H0+H1) en millions

dat$Ztot<--log(dat$H_R2/dat$H_0) #Mortality rate

abb.pos2<-c(which((dat$Tot/dat$H_0)>1),abb.pos)
dat[abb.pos2,6]<-NA #Enlève aberrations

dat$Zcod<-(dat$Tot/dat$H_0)*(dat$Ztot/(1-exp(-dat$Ztot))) #Mortality due to cod precodion


cod<-dat[,c(1,6,7,8)]

save(cod,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/cod.RData")

############################### Plot

plot(dat$Zcod,exp(-(dat$Ztot)))
abline(rq(exp(-(dat$Ztot))~dat$Zcod, tau=0.9))


plot(dat$Zcod,dat$Ztot)


plot(dat$H_R2~dat$Ztot)
plot(dat$H_R2~dat$Zcod)
abline(rq(dat$H_R2~dat$Zcod, tau=0.9))


plot(dat$H_R2~exp(-(dat$Ztot)))
abline(rq(dat$H_R2~exp(-(dat$Ztot)), tau=0.9))
