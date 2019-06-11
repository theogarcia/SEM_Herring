library(ggplot2)
library(mgcv)
library(qgam) #Quant reg
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
model1<-qgam(H_R2~s(Cc_H,k=3),data=data,qu=0.9)
pred <- predict(model1, newdata = data, se=TRUE)

fit1 <- qgamV(H_R2 ~ s(Cc_H,k=3), data=data, qu = 0.9) #
check1D(fit1, "Cc_H") + l_gridQCheck1D(qu = 0.9)       #Check residuals......

plot(H_R2~Cc_H,data=data)
points(data$Cc_H,pred$fit,col="red",pch=16,cex=1.1)
points(data$Cc_H, pred$fit + 2*pred$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Cc_H, pred$fit - 2*pred$se.fit,pch=16,cex=1.1, col = "blue")

model2<-qgam(Cc_H~s(Cap_cod_rat,k=3),data=data,qu=0.9)
pred2 <- predict(model2, newdata = data, se=TRUE)

plot(Cc_H~Cap_cod_rat,data=data)
points(data$Cap_cod_rat,pred2$fit,col="red",pch=16,cex=1.1)
points(data$Cap_cod_rat, pred2$fit + 2*pred2$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Cap_cod_rat, pred2$fit - 2*pred2$se.fit,pch=16,cex=1.1, col = "blue")



model3<-qgam(Cc_H~s(Cod,k=3),data=data,qu=0.9)
pred3<- predict(model3, newdata = data, se=TRUE)

plot(Cc_H~Cod,data=data)
points(data$Cod,pred3$fit,col="red",pch=16,cex=1.1)
points(data$Cod, pred3$fit + 2*pred3$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Cod, pred3$fit - 2*pred3$se.fit,pch=16,cex=1.1, col = "blue")


####### Puffin pred
model4<-qgam(H_R2~s(Puff,k=3),data=data,qu=0.9)
pred4<- predict(model4, newdata = data, se=TRUE)

plot(H_R2~Puff,data=data)
points(data$Puff,pred4$fit,col="red",pch=16,cex=1.1)
points(data$Puff, pred4$fit + 2*pred4$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Puff, pred4$fit - 2*pred4$se.fit,pch=16,cex=1.1, col = "blue")

###### H_0
model5<-qgam(H_R2~s(H_0,k=3),data=data,qu=0.9)
pred5<- predict(model5, newdata = data, se=TRUE)

plot(H_R2~H_0, data=data)
points(data$H_0, pred5$fit,col="red",pch=16,cex=1.1)
points(data$H_0, pred5$fit + 2*pred5$se.fit,pch=16,cex=1.1,col = "blue")
points(data$H_0, pred5$fit - 2*pred5$se.fit,pch=16,cex=1.1, col = "blue")

###### Current
###Sal on H_0
model6<-qgam(H_0~s(Sal_I2,k=3),data=data,qu=0.9)
pred6<- predict(model6, newdata = data, se=TRUE)

plot(H_0~Sal_I2, data=data)
points(data$Sal_I2, pred6$fit,col="red",pch=16,cex=1.1)
points(data$Sal_I2, pred6$fit + 2*pred6$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Sal_I2, pred6$fit - 2*pred6$se.fit,pch=16,cex=1.1, col = "blue")

model7<-qgam(H_0~s(Sal_I1,k=3),data=data,qu=0.9)
pred7<- predict(model7, newdata = data, se=TRUE)

plot(H_0~Sal_I1, data=data)
points(data$Sal_I1, pred7$fit,col="red",pch=16,cex=1.1)
points(data$Sal_I1, pred7$fit + 2*pred7$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Sal_I1, pred7$fit - 2*pred7$se.fit,pch=16,cex=1.1, col = "blue")

###ACW on Sal

model8<-qgam(Sal_I2~s(ACW_stress,k=3),data=data,qu=0.9)
pred8<- predict(model8, newdata = data, se=TRUE)

plot(Sal_I2~ACW_stress, data=data)
points(data$ACW_stress, pred8$fit,col="red",pch=16,cex=1.1)
points(data$ACW_stress, pred8$fit + 2*pred8$se.fit,pch=16,cex=1.1,col = "blue")
points(data$ACW_stress, pred8$fit - 2*pred8$se.fit,pch=16,cex=1.1, col = "blue")



model9<-qgam(Sal_I2~s(ACW_stab,k=3),data=data,qu=0.9)
pred9<- predict(model9, newdata = data, se=TRUE)

plot(Sal_I2~ACW_stab, data=data)
points(data$ACW_stab, pred9$fit,col="red",pch=16,cex=1.1)
points(data$ACW_stab, pred9$fit + 2*pred9$se.fit,pch=16,cex=1.1,col = "blue")
points(data$ACW_stab, pred9$fit - 2*pred9$se.fit,pch=16,cex=1.1, col = "blue")


model10<-qgam(H_0~s(ACW_stress,k=3),data=data,qu=0.9)
pred10<- predict(model10, newdata = data, se=TRUE)

plot(H_0~ACW_stress, data=data)
points(data$ACW_stress, pred10$fit,col="red",pch=16,cex=1.1)
points(data$ACW_stress, pred10$fit + 2*pred10$se.fit,pch=16,cex=1.1,col = "blue")
points(data$ACW_stress, pred10$fit - 2*pred10$se.fit,pch=16,cex=1.1, col = "blue")


####### Mack
model11<-qgam(H_0~s(Mack,k=3),data=data,qu=0.9)
pred11<- predict(model11, newdata = data, se=TRUE)

plot(H_0~Mack, data=data)
points(data$Mack, pred11$fit,col="red",pch=16,cex=1.1)
points(data$Mack, pred11$fit + 2*pred11$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Mack, pred11$fit - 2*pred11$se.fit,pch=16,cex=1.1, col = "blue")


model12<-qgam(Mack~s(T_Ssum,k=3),data=data,qu=0.9)
pred12<- predict(model12, newdata = data, se=TRUE)

plot(Mack~T_Ssum, data=data)
points(data$T_Ssum, pred12$fit,col="red",pch=16,cex=1.1)
points(data$T_Ssum, pred12$fit + 2*pred12$se.fit,pch=16,cex=1.1,col = "blue")
points(data$T_Ssum, pred12$fit - 2*pred12$se.fit,pch=16,cex=1.1, col = "blue")

####### ZOOPK
model13<-qgam(H_0~s(Cal_fin,k=3),data=data,qu=0.9)
pred13<- predict(model13, newdata = data, se=TRUE)

plot(H_0~Cal_fin, data=data)
points(data$Cal_fin, pred13$fit,col="red",pch=16,cex=1.1)
points(data$Cal_fin, pred13$fit + 2*pred13$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Cal_fin, pred13$fit - 2*pred13$se.fit,pch=16,cex=1.1, col = "blue")


model14<-qgam(Cal_fin~s(T_Ssum,k=3),data=data,qu=0.9)
pred14<- predict(model14, newdata = data, se=TRUE)

plot(Cal_fin~T_Ssum, data=data)
points(data$T_Ssum, pred14$fit,col="red",pch=16,cex=1.1)
points(data$T_Ssum, pred14$fit + 2*pred14$se.fit,pch=16,cex=1.1,col = "blue")
points(data$T_Ssum, pred14$fit - 2*pred14$se.fit,pch=16,cex=1.1, col = "blue")


####### Hatch
model15<-qgam(H_0~s(mean_hatch,k=3),data=data,qu=0.9)
pred15<- predict(model15, newdata = data, se=TRUE)

plot(H_0~mean_hatch, data=data)
points(data$mean_hatch, pred15$fit,col="red",pch=16,cex=1.1)
points(data$mean_hatch, pred15$fit + 2*pred15$se.fit,pch=16,cex=1.1,col = "blue")
points(data$mean_hatch, pred15$fit - 2*pred15$se.fit,pch=16,cex=1.1, col = "blue")

par(mfrow=c(1,2))
model16<-qgam(mean_hatch~s(Age_index1,k=3),data=data,qu=0.9)
pred16<- predict(model16, newdata = data, se=TRUE)
model17<-qgam(mean_hatch~s(Age_index2,k=3),data=data,qu=0.9)
pred17<- predict(model17, newdata = data, se=TRUE)

plot(mean_hatch~Age_index1, data=data)
points(data$Age_index1, pred16$fit,col="red",pch=16,cex=1.1)
points(data$Age_index1, pred16$fit + 2*pred16$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Age_index1, pred16$fit - 2*pred16$se.fit,pch=16,cex=1.1, col = "blue")

plot(mean_hatch~Age_index2, data=data)
points(data$Age_index2, pred17$fit,col="red",pch=16,cex=1.1)
points(data$Age_index2, pred17$fit + 2*pred17$se.fit,pch=16,cex=1.1,col = "blue")
points(data$Age_index2, pred17$fit - 2*pred17$se.fit,pch=16,cex=1.1, col = "blue")


dev.off() 



model18<-qgam(H_0~s(SSB_H,k=3),data=data,qu=0.9)
pred18<- predict(model18, newdata = data, se=TRUE)

plot(H_0~SSB_H, data=data)
points(data$SSB_H, pred18$fit,col="red",pch=16,cex=1.1)
points(data$SSB_H, pred18$fit + 2*pred18$se.fit,pch=16,cex=1.1,col = "blue")
points(data$SSB_H, pred18$fit - 2*pred18$se.fit,pch=16,cex=1.1, col = "blue")