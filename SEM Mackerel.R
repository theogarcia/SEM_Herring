#### SEM mackerel predation ####
#### Packages ####
library(readr)
library(ggplot2)
library(mgcv)
library(qgam)
library(visreg) #package gam
library(quantreg)
library(gridExtra)
library(piecewiseSEM)
library(Hmisc)
library(dplyr)
library(Qtools)#Goodness of fit quantile regression
##### data ####
data <- read.csv("data/data_1.csv", header=T, sep=";",dec=".")
##load("stress_mean.RData")
###load("stress_sd.RData")
#data[which(data$years>1947),dim(data)[2]+1]<-stress_mean
#colnames(data)[dim(data)[2]]<-"stress_mean"
#data[which(data$years>1947),dim(data)[2]+1]<-stress_sd
#colnames(data)[dim(data)[2]]<-"stress_sd"
attach(data)

#####Quantile regression plots ##########
####Stress wind on VPA####
data_wind<-data.frame(na.omit(cbind(data$years,Lag(data$stress_mean,-2),Lag(data$stress_sd,-2),data$H_R2_VPA)))
windows()
par(mfrow=c(3,2))
plot(data_wind$X4~data_wind$X2,data=data_wind, xlab="ACW stress mean lagged",ylab="Recruitment VPA")
abline(rq(data_wind$X4~data_wind$X2, tau=0.95),col="red")
abline(rq(data_wind$X4~data_wind$X2, tau=0.9),col="orange")
abline(rq(data_wind$X4~data_wind$X2, tau=0.85),col="purple")
#plot(rq(data_wind$X4~data_wind$X2, tau=seq(0.2,0.9,by=0.1)))
#rq(data_wind$X4~data_wind$X2, tau=0.8)->blabla
#GOFTest(blabla, type = "cusum", alpha = 0.05, B = 100, seed = NULL)

plot(data_wind$X4~data_wind$X3,data=data_wind, xlab="ACW stress sd lagged",ylab="Recruitment VPA")
abline(rq(data_wind$X4~data_wind$X3, tau=0.95),col="red")
abline(rq(data_wind$X4~data_wind$X3, tau=0.9),col="orange")
abline(rq(data_wind$X4~data_wind$X3, tau=0.85),col="purple")

####Stress wind on stock assesment####
data_wind<-data.frame(na.omit(cbind(data$years,Lag(data$stress_mean,-2),Lag(data$stress_sd,-2),data$H_R2)))

#par(mfrow=c(1,2))
plot(data_wind$X4~data_wind$X2,data=data_wind, xlab="ACW stress mean lagged",ylab="Recruitment Model")
abline(rq(data_wind$X4~data_wind$X2, tau=0.95),col="red")
abline(rq(data_wind$X4~data_wind$X2, tau=0.9),col="orange")
abline(rq(data_wind$X4~data_wind$X2, tau=0.85),col="purple")

plot(data_wind$X4~data_wind$X3,data=data_wind, xlab="ACW stress sd lagged",ylab="Recruitment Model")
abline(rq(data_wind$X4~data_wind$X3, tau=0.95),col="red")
abline(rq(data_wind$X4~data_wind$X3, tau=0.9),col="orange")
abline(rq(data_wind$X4~data_wind$X3, tau=0.85),col="purple")

####Stress wind on Age 0####
data_wind<-data.frame(na.omit(cbind(data$years,data$stress_mean,data$stress_sd,data$H_0)))

#par(mfrow=c(1,2))
plot(data_wind$X4~data_wind$X2,data=data_wind, xlab="ACW stress mean",ylab="Age 0")
abline(rq(data_wind$X4~data_wind$X2, tau=0.95),col="red")
abline(rq(data_wind$X4~data_wind$X2, tau=0.9),col="orange")
abline(rq(data_wind$X4~data_wind$X2, tau=0.85),col="purple")
lm(data_wind$X4~data_wind$X2)%>%summary() 

plot(data_wind$X4~data_wind$X3,data=data_wind, xlab="ACW stress sd",ylab="Age 0")
abline(rq(data_wind$X4~data_wind$X3, tau=0.95),col="red")
abline(rq(data_wind$X4~data_wind$X3, tau=0.9),col="orange")
abline(rq(data_wind$X4~data_wind$X3, tau=0.85),col="purple")
lm(data_wind$X4~data_wind$X3)%>%summary() 

#### Limitation by mackerel (itself limited by water temperature) ####

plot(data$M_RT~data$T_Ssum,data=data)
abline(rq(data$M_RT~data$T_Ssum, tau=0.95),col="red")
abline(rq(data$M_RT~data$T_Ssum, tau=0.9),col="orange")
abline(rq(data$M_RT~data$T_Ssum, tau=0.85),col="purple")
abline(lm(data$M_RT~data$T_Ssum))
lm(data$M_RT~data$T_Ssum)%>%summary 

plot(data$H_R2~Lag(data$M_RT,-2),data=data)
abline(rq(data$H_R2~Lag(data$M_RT,-2), tau=0.95),col="red")
abline(rq(data$H_R2~Lag(data$M_RT,-2), tau=0.9),col="orange")
abline(rq(data$H_R2~Lag(data$M_RT,-2), tau=0.85),col="purple")
abline(lm(data$H_R2~Lag(data$M_RT,-2)))
lm(data$H_R2~Lag(data$M_RT,-2))%>%summary 

plot(data$H_0~data$M_RT,data=data)
abline(rq(data$H_0~data$M_RT, tau=0.95),col="red")
abline(rq(data$H_0~data$M_RT, tau=0.9),col="orange")
abline(rq(data$H_0~data$M_RT, tau=0.85),col="purple")
abline(lm(data$H_0~data$M_RT))
lm(data$H_0~data$M_RT)%>%summary 

#### SEM with predation, larvae drift on juvenile (and Age 2) ####
dataSem1<-data.frame(na.omit(cbind(data$years,data$H_R2,Lag(data$H_0,-2),
                                   Lag(data$stress_mean,-2),Lag(data$stress_sd,-2),
                                   Lag(data$M_RT,-2),Lag(data$T_Ssum,-2))))
colnames(dataSem1)<-c("years","H_R2","H_0lag","stress_meanlag","stress_sdlag","Macklag","Templag")

SEM_juv<-psem(lm(Macklag~Templag,dataSem1),lm(H_0lag~Macklag+stress_meanlag*stress_sdlag,dataSem1),
              lm(H_R2~H_0lag,dataSem1))
summary(SEM_juv)


gam<-gam(dataSem1$H_R2~s(dataSem1$H_0lag,k=3))%>%summary()
windows()
plot(gam)
abline(h=0)

SEM_ad<-psem(lm(Macklag~Templag,dataSem1),lm(H_R2~Macklag+stress_meanlag*stress_sdlag,dataSem1))
summary(SEM_ad)


AIC(SEM_juv,SEM_ad)
