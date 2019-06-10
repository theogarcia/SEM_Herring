library(ggplot2)
library(mgcv)
library(qgam) #Quant reg
library(visreg) #package gam
library(quantreg) #Quant reg
library(Hmisc) #Lag function
library(dplyr)

###################################### Load data #######################################################

load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data.RData")

#################################### Lag variables #####################################################
##### Lag -1 #####

data$Cc_H<-Lag(data$Cc_H,-1)
data$Cod<-Lag(data$Cod,-1)
data$Cap_cod_rat<-Lag(data$Cap_cod_rat,-1)

##### Lag -2 ####

data$Puff<-Lag(data$Puff,-2)
data$Mack<-Lag(data$Mack,-2)
data$H_0<-Lag(data$H_0,-2)
data$T_Ssum<-Lag(data$T_Ssum,-2)
data$Cal_fin<-Lag(data$Cal_fin,-2)
data$ACW_stress<-Lag(data$ACW_stress,-2)
data$ACW_stab<-Lag(data$ACW_stab,-2)
data$ACW_stab<-Lag(data$ACW_stab,-2)
data$mean_hatch<-Lag(data$mean_hatch,-2)
data$Sal_I2<-Lag(data$Sal_I2,-2)
data$Sal_I1<-Lag(data$Sal_I1,-2)

##### Lag -3 ####

data$SSB_H<-Lag(data$SSB_H,-3)
data$Age_index1<-Lag(data$Age_index1,-3)
data$Age_index2<-Lag(data$Age_index2,-3)

#################################### Quantile regressions ###############################################
######## Cod predation

plot(qgam(H_R2~s(Cc_H,k=3),data=data,qu=0.9),se=T)
abline(h=0)

plot(H_R2~Cc_H,data=data)

plot(Cc_H~Cap_cod_rat,data=data)
abline(rq(Cc_H~Cap_cod_rat,data=data, tau=0.9))

plot(qgam(Cc_H~s(Cod,k=4),data=data,qu=0.9),se=T)
abline(h=0)


####### Puffin pred
plot(qgam(H_R2~s(Puff,k=3),data=data,qu=0.9),se=T)
abline(h=0)

plot(H_R2~Puff, data=data)

###### H_0

plot(H_R2~H_0, data=data)
plot(qgam(H_R2~s(H_0,k=3),data=data,qu=0.9),se=T)
abline(h=0)


###### Current
plot(qgam(H_0~s(Sal_I2,k=3),data=data,qu=0.9),se=T)
abline(h=0)
plot(qgam(H_0~s(Sal_I1,k=3),data=data,qu=0.9),se=T)
abline(h=0)

plot(H_0~Sal_I2, data=data)
abline(rq(H_0~Sal_I2,data=data,tau=0.9),col="red")

plot(qgam(Sal_I2~s(ACW_stress,k=3),data=data,qu=0.9),se=T)
abline(h=0)
plot(qgam(Sal_I2~s(ACW_stress,k=3),data=data,qu=0.9),se=T)
abline(h=0)

plot(H_0~data$ACW_stress, data=data)
abline(rq(H_0~Sal_I2,data=data,tau=0.9),col="red")

plot(qgam(H_0~s(ACW_stress,k=3),data=data,qu=0.9),se=T)
abline(h=0)


####### Mack
plot(H_0~Mack, data=data)
abline(rq(H_0~Mack,data=data,tau=0.9),col="red")

plot(qgam(H_0~s(Mack,k=3),data=data,qu=0.9),se=T)
abline(h=0)

plot(Mack~T_Ssum, data=data)
abline(rq(Mack~T_Ssum,data=data,tau=0.9),col="red")

####### ZOOPK

plot(H_0~Cal_fin, data=data)
abline(rq(H_0~Cal_fin,data=data,tau=0.9),col="red")

plot(Cal_fin~T_Ssum, data=data)
abline(rq(Cal_fin~T_Ssum,data=data,tau=0.9),col="red")


####### Hatch

plot(H_0~mean_hatch, data=data)
abline(rq(H_0~mean_hatch,data=data,tau=0.9),col="red")

plot(qgam(H_0~s(mean_hatch,k=3),data=data,qu=0.9),se=T)
abline(h=0)

plot(mean_hatch~Age_index1, data=data)
plot(mean_hatch~Age_index2, data=data)

plot(H_0~SSB_H, data=data)
abline(rq(H_0~SSB_H,data=data,tau=0.9),col="red")

plot(qgam(H_0~s(SSB_H,k=3),data=data,qu=0.9),se=T)
abline(h=0)

