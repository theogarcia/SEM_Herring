library(ggplot2) #plot
library(mgcv) #Quantgam
library(qgam) #Quantgam
library(visreg) #package gam
library(quantreg) #Quant reg
library(Hmisc) #Lag function
library(dplyr) 
library(mgcViz) #QAGM check
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
data$SSB_H<-Lag(data$SSB_H,-2)
data$Age_index1<-Lag(data$Age_index1,-2)
data$Age_index2<-Lag(data$Age_index2,-2)

#################################### Quantile regressions ###############################################
######## Cod predation
dat1<-data.frame(na.omit(cbind(H_R2,Cc_H)))
model1<-qgam(H_R2~s(Cc_H,k=3),data=dat1,qu=0.9)
pred <- predict(model1, newdata = dat1, se=TRUE)

fit1 <- qgamV(H_R2 ~ s(Cc_H,k=3), data=data, qu = 0.9) #
check1D(fit1, "Cc_H") + l_gridQCheck1D(qu = 0.9)       #Check residuals......
check.qgam(model1)

plot(H_R2~Cc_H,data=dat1,ylim=c(0,max(pred$fit + 2*pred$se.fit, na.rm=T)))
points(dat1$Cc_H,pred$fit,col="red",pch=16,cex=1.1)
points(dat1$Cc_H, pred$fit + 2*pred$se.fit,pch=16,cex=1.1,col = "blue")
points(dat1$Cc_H, pred$fit - 2*pred$se.fit,pch=16,cex=1.1, col = "blue")

datcap<-data.frame(na.omit(cbind(Cc_H,Cap_cod_rat)))
model2<-qgam(Cc_H~s(Cap_cod_rat,k=3),data=datcap,qu=0.9)
pred2 <- predict(model2, newdata = datcap, se=TRUE)

plot(Cc_H~Cap_cod_rat,data=datcap,ylim=c(0,max(pred2$fit + 2*pred2$se.fit, na.rm=T)))
points(datcap$Cap_cod_rat,pred2$fit,col="red",pch=16,cex=1.1)
points(datcap$Cap_cod_rat, pred2$fit + 2*pred2$se.fit,pch=16,cex=1.1,col = "blue")
points(datcap$Cap_cod_rat, pred2$fit - 2*pred2$se.fit,pch=16,cex=1.1, col = "blue")


dat3<-data.frame(na.omit(cbind(Cc_H,Cod)))
model3<-qgam(Cc_H~s(Cod,k=3),data=dat3,qu=0.9)
pred3<- predict(model3, newdata = dat3, se=TRUE)

plot(Cc_H~Cod,data=dat3,ylim=c(0,max(pred3$fit + 2*pred3$se.fit, na.rm=T)))
points(dat3$Cod,pred3$fit,col="red",pch=16,cex=1.1)
points(dat3$Cod, pred3$fit + 2*pred3$se.fit,pch=16,cex=1.1,col = "blue")
points(dat3$Cod, pred3$fit - 2*pred3$se.fit,pch=16,cex=1.1, col = "blue")


####### Puffin pred
datpuf<-data.frame(na.omit(cbind(H_R2,Puff)))
model4<-qgam(H_R2~s(Puff,k=3),data=datpuf,qu=0.9)
pred4<- predict(model4, newdata = datpuf, se=TRUE)

plot(H_R2~Puff,data=datpuf,ylim=c(0,max(pred4$fit + 2*pred4$se.fit, na.rm=T)))
points(datpuf$Puff,pred4$fit,col="red",pch=16,cex=1.1)
points(datpuf$Puff, pred4$fit + 2*pred4$se.fit,pch=16,cex=1.1,col = "blue")
points(datpuf$Puff, pred4$fit - 2*pred4$se.fit,pch=16,cex=1.1, col = "blue")

###### H_0
dat5<-data.frame(na.omit(cbind(H_R2,H_0)))
model5<-qgam(H_R2~s(H_0,k=3),data=dat5,qu=0.9)
pred5<- predict(model5, newdata = dat5, se=TRUE)

plot(H_R2~H_0, data=dat5,ylim=c(0,max(pred5$fit + 2*pred5$se.fit, na.rm=T)))
points(dat5$H_0, pred5$fit,col="red",pch=16,cex=1.1)
points(dat5$H_0, pred5$fit + 2*pred5$se.fit,pch=16,cex=1.1,col = "blue")
points(dat5$H_0, pred5$fit - 2*pred5$se.fit,pch=16,cex=1.1, col = "blue")

###### Current
###Sal on H_0
dat6<-data.frame(na.omit(cbind(H_0,Sal_I2)))
model6<-qgam(H_0~s(Sal_I2,k=3),data=dat6,qu=0.9)
pred6<- predict(model6, newdata = dat6, se=TRUE)

plot(H_0~Sal_I2, data=dat6,ylim=c(0,max(pred6$fit + 2*pred6$se.fit, na.rm=T)))
points(dat6$Sal_I2, pred6$fit,col="red",pch=16,cex=1.1)
points(dat6$Sal_I2, pred6$fit + 2*pred6$se.fit,pch=16,cex=1.1,col = "blue")
points(dat6$Sal_I2, pred6$fit - 2*pred6$se.fit,pch=16,cex=1.1, col = "blue")

dat7<-data.frame(na.omit(cbind(H_0,Sal_I1)))
model7<-qgam(H_0~s(Sal_I1,k=3),data=dat7,qu=0.9)
pred7<- predict(model7, newdata = dat7, se=TRUE)

plot(H_0~Sal_I1, data=dat7,ylim=c(0,max(pred7$fit + 2*pred7$se.fit, na.rm=T)))
points(dat7$Sal_I1, pred7$fit,col="red",pch=16,cex=1.1)
points(dat7$Sal_I1, pred7$fit + 2*pred7$se.fit,pch=16,cex=1.1,col = "blue")
points(dat7$Sal_I1, pred7$fit - 2*pred7$se.fit,pch=16,cex=1.1, col = "blue")

###ACW on Sal
dat8<-data.frame(na.omit(cbind(Sal_I2,ACW_stress)))
model8<-qgam(Sal_I2~s(ACW_stress,k=3),data=dat8,qu=0.9)
pred8<- predict(model8, newdata = dat8, se=TRUE)

plot(Sal_I2~ACW_stress, data=dat8,ylim=c(min(dat8$Sal_I2, na.rm=T),max(pred8$fit + 2*pred8$se.fit, na.rm=T)))
points(dat8$ACW_stress, pred8$fit,col="red",pch=16,cex=1.1)
points(dat8$ACW_stress, pred8$fit + 2*pred8$se.fit,pch=16,cex=1.1,col = "blue")
points(dat8$ACW_stress, pred8$fit - 2*pred8$se.fit,pch=16,cex=1.1, col = "blue")


dat9<-data.frame(na.omit(cbind(Sal_I2,ACW_stab)))
model9<-qgam(Sal_I2~s(ACW_stab,k=3),data=dat9,qu=0.9)
pred9<- predict(model9, newdata = dat9, se=TRUE)

plot(Sal_I2~ACW_stab, data=dat9,ylim=c(min(dat9$Sal_I2, na.rm=T),max(pred9$fit + 2*pred9$se.fit, na.rm=T)))
points(dat9$ACW_stab, pred9$fit,col="red",pch=16,cex=1.1)
points(dat9$ACW_stab, pred9$fit + 2*pred9$se.fit,pch=16,cex=1.1,col = "blue")
points(dat9$ACW_stab, pred9$fit - 2*pred9$se.fit,pch=16,cex=1.1, col = "blue")

dat10<-data.frame(na.omit(cbind(H_0,ACW_stress)))
model10<-qgam(H_0~s(ACW_stress,k=3),data=dat10,qu=0.9)
pred10<- predict(model10, newdata = dat10, se=TRUE)

plot(H_0~ACW_stress, data=dat10,ylim=c(0,max(H_0, na.rm=T)))
points(dat10$ACW_stress, pred10$fit,col="red",pch=16,cex=1.1)
points(dat10$ACW_stress, pred10$fit + 2*pred10$se.fit,pch=16,cex=1.1,col = "blue")
points(dat10$ACW_stress, pred10$fit - 2*pred10$se.fit,pch=16,cex=1.1, col = "blue")


####### Mack
dat11<-data.frame(na.omit(cbind(H_0,Mack)))
model11<-qgam(H_0~s(Mack,k=3),data=dat11,qu=0.9)
pred11<- predict(model11, newdata = dat11, se=TRUE)

plot(H_0~Mack, data=dat11,ylim=c(0,max(H_0, na.rm=T)))
points(dat11$Mack, pred11$fit,col="red",pch=16,cex=1.1)
points(dat11$Mack, pred11$fit + 2*pred11$se.fit,pch=16,cex=1.1,col = "blue")
points(dat11$Mack, pred11$fit - 2*pred11$se.fit,pch=16,cex=1.1, col = "blue")

dat12<-data.frame(na.omit(cbind(Mack,T_Ssum)))
model12<-qgam(Mack~s(T_Ssum,k=3),data=dat12,qu=0.9)
pred12<- predict(model12, newdata = dat12, se=TRUE)

plot(Mack~T_Ssum, data=dat12,ylim=c(min(dat12$Mack),max(pred12$fit + 2*pred12$se.fit, na.rm=T)))
points(dat12$T_Ssum, pred12$fit,col="red",pch=16,cex=1.1)
points(dat12$T_Ssum, pred12$fit + 2*pred12$se.fit,pch=16,cex=1.1,col = "blue")
points(dat12$T_Ssum, pred12$fit - 2*pred12$se.fit,pch=16,cex=1.1, col = "blue")

####### ZOOPK
dat13<-data.frame(na.omit(cbind(H_0,Cal_fin)))
model13<-qgam(H_0~s(Cal_fin,k=3),data=dat13,qu=0.9)
pred13<- predict(model13, newdata = dat13, se=TRUE)

plot(H_0~Cal_fin, data=dat13,ylim=c(0,max(pred13$fit + 2*pred13$se.fit, na.rm=T)))
points(dat13$Cal_fin, pred13$fit,col="red",pch=16,cex=1.1)
points(dat13$Cal_fin, pred13$fit + 2*pred13$se.fit,pch=16,cex=1.1,col = "blue")
points(dat13$Cal_fin, pred13$fit - 2*pred13$se.fit,pch=16,cex=1.1, col = "blue")


dat14<-data.frame(na.omit(cbind(Cal_fin,T_Ssum)))
model14<-qgam(Cal_fin~s(T_Ssum,k=3),data=dat14,qu=0.9)
pred14<- predict(model14, newdata = dat14, se=TRUE)

plot(Cal_fin~T_Ssum, data=dat14)
points(dat14$T_Ssum, pred14$fit,col="red",pch=16,cex=1.1)
points(dat14$T_Ssum, pred14$fit + 2*pred14$se.fit,pch=16,cex=1.1,col = "blue")
points(dat14$T_Ssum, pred14$fit - 2*pred14$se.fit,pch=16,cex=1.1, col = "blue")


####### Hatch
dat15<-data.frame(na.omit(cbind(H_0,mean_hatch)))
model15<-qgam(H_0~s(mean_hatch,k=3),data=dat15,qu=0.9)
pred15<- predict(model15, newdata = dat15, se=TRUE)

plot(H_0~mean_hatch, data=dat15,ylim=c(0,max(pred15$fit + 2*pred15$se.fit, na.rm=T)))
points(dat15$mean_hatch, pred15$fit,col="red",pch=16,cex=1.1)
points(dat15$mean_hatch, pred15$fit + 2*pred15$se.fit,pch=16,cex=1.1,col = "blue")
points(dat15$mean_hatch, pred15$fit - 2*pred15$se.fit,pch=16,cex=1.1, col = "blue")

par(mfrow=c(1,2))
dat16<-data.frame(na.omit(cbind(mean_hatch,Age_index1)))
model16<-qgam(mean_hatch~s(Age_index1,k=3),data=dat16,qu=0.9)
pred16<- predict(model16, newdata = dat16, se=TRUE)
dat17<-data.frame(na.omit(cbind(mean_hatch,Age_index2)))
model17<-qgam(mean_hatch~s(Age_index2,k=3),data=dat17,qu=0.9)
pred17<- predict(model17, newdata = dat17, se=TRUE)

plot(mean_hatch~Age_index1, data=dat16,ylim=c(min(dat16$mean_hatch),max(pred16$fit + 2*pred16$se.fit, na.rm=T)))
points(dat16$Age_index1, pred16$fit,col="red",pch=16,cex=1.1)
points(dat16$Age_index1, pred16$fit + 2*pred16$se.fit,pch=16,cex=1.1,col = "blue")
points(dat16$Age_index1, pred16$fit - 2*pred16$se.fit,pch=16,cex=1.1, col = "blue")

plot(mean_hatch~Age_index2, data=dat17,ylim=c(min(dat17$mean_hatch),max(pred17$fit + 2*pred17$se.fit, na.rm=T)))
points(dat17$Age_index2, pred17$fit,col="red",pch=16,cex=1.1)
points(dat17$Age_index2, pred17$fit + 2*pred17$se.fit,pch=16,cex=1.1,col = "blue")
points(dat17$Age_index2, pred17$fit - 2*pred17$se.fit,pch=16,cex=1.1, col = "blue")


dev.off() 

dat18<-data.frame(na.omit(cbind(H_0,SSB_H)))
model18<-qgam(H_0~s(SSB_H,k=3),data=dat18,qu=0.9)
pred18<- predict(model18, newdata = dat18, se=TRUE)

plot(H_0~SSB_H, data=dat18)
points(dat18$SSB_H, pred18$fit,col="red",pch=16,cex=1.1)
points(dat18$SSB_H, pred18$fit + 2*pred18$se.fit,pch=16,cex=1.1,col = "blue")
points(dat18$SSB_H, pred18$fit - 2*pred18$se.fit,pch=16,cex=1.1, col = "blue")

#######################################################################################################
############################################# PLOT ####################################################
#######################################################################################################
########## HR2 #########

windows()
par(mfrow=c(2,2))

plot(H_R2~Cc_H,data=dat1, xlab="Consumption by Cod", ylab="Herring recruits at age 2", ylim=c(0,max(pred$fit + 2*pred$se.fit, na.rm=T)))
points(dat1$Cc_H,pred$fit,col="red",pch=16,cex=1.1)
points(dat1$Cc_H, pred$fit + 2*pred$se.fit,pch=16,cex=1.1,col = "blue")
points(dat1$Cc_H, pred$fit - 2*pred$se.fit,pch=16,cex=1.1, col = "blue")

plot(H_R2~Puff,data=datpuf, xlab="Puffin abundance", ylab="Herring recruits at age 2", ylim=c(0,max(pred4$fit + 2*pred4$se.fit, na.rm=T)))
points(datpuf$Puff,pred4$fit,col="red",pch=16,cex=1.1)
points(datpuf$Puff, pred4$fit + 2*pred4$se.fit,pch=16,cex=1.1,col = "blue")
points(datpuf$Puff, pred4$fit - 2*pred4$se.fit,pch=16,cex=1.1, col = "blue")

plot(H_R2~H_0, data=dat5, xlab="Juveniles abundance (age 0)", ylab="Herring recruits at age 2", ylim=c(0,max(pred5$fit + 2*pred5$se.fit, na.rm=T)))
points(dat5$H_0, pred5$fit,col="red",pch=16,cex=1.1)
points(dat5$H_0, pred5$fit + 2*pred5$se.fit,pch=16,cex=1.1,col = "blue")
points(dat5$H_0, pred5$fit - 2*pred5$se.fit,pch=16,cex=1.1, col = "blue")

####### H_0 ########
windows()
par(mfrow=c(3,2))
plot(H_0~Sal_I2, data=dat6, xlab="Salinity anomalies", ylab="Juveniles abundance (age 0)", ylim=c(0,max(pred6$fit + 2*pred6$se.fit, na.rm=T)))
points(dat6$Sal_I2, pred6$fit,col="red",pch=16,cex=1.1)
points(dat6$Sal_I2, pred6$fit + 2*pred6$se.fit,pch=16,cex=1.1,col = "blue")
points(dat6$Sal_I2, pred6$fit - 2*pred6$se.fit,pch=16,cex=1.1, col = "blue")

plot(H_0~Mack, data=dat11, xlab="Mackerel abundance", ylab="Juveniles abundance (age 0)", ylim=c(0,max(pred11$fit + 2*pred11$se.fit, na.rm=T)))
points(dat11$Mack, pred11$fit,col="red",pch=16,cex=1.1)
points(dat11$Mack, pred11$fit + 2*pred11$se.fit,pch=16,cex=1.1,col = "blue")
points(dat11$Mack, pred11$fit - 2*pred11$se.fit,pch=16,cex=1.1, col = "blue")

plot(H_0~Cal_fin, data=dat13, xlab="Calanus finmarchichus abundance", ylab="Juveniles abundance (age 0)", ylim=c(0,max(pred13$fit + 2*pred13$se.fit, na.rm=T)))
points(dat13$Cal_fin, pred13$fit,col="red",pch=16,cex=1.1)
points(dat13$Cal_fin, pred13$fit + 2*pred13$se.fit,pch=16,cex=1.1,col = "blue")
points(dat13$Cal_fin, pred13$fit - 2*pred13$se.fit,pch=16,cex=1.1, col = "blue")

plot(H_0~mean_hatch, data=dat15, xlab="Mean hatching date", ylab="Juveniles abundance (age 0)", ylim=c(0,max(pred15$fit + 2*pred15$se.fit, na.rm=T)))
points(dat15$mean_hatch, pred15$fit,col="red",pch=16,cex=1.1)
points(dat15$mean_hatch, pred15$fit + 2*pred15$se.fit,pch=16,cex=1.1,col = "blue")
points(dat15$mean_hatch, pred15$fit - 2*pred15$se.fit,pch=16,cex=1.1, col = "blue")

plot(H_0~SSB_H, data=dat18, xlab="SSB", ylab="Juveniles abundance (age 0)", ylim=c(0,max(pred18$fit + 2*pred18$se.fit, na.rm=T)))
points(dat18$SSB_H, pred18$fit,col="red",pch=16,cex=1.1)
points(dat18$SSB_H, pred18$fit + 2*pred18$se.fit,pch=16,cex=1.1,col = "blue")
points(dat18$SSB_H, pred18$fit - 2*pred18$se.fit,pch=16,cex=1.1, col = "blue")
