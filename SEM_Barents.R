library(Hmisc) #Lag function
library(dplyr) 
library(piecewiseSEM)

###################################### Load data #######################################################

load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data.RData")

#################################### Lag variables #####################################################

##### Lag -1 #####

data$Cod<-Lag(data$Cod,1)
data$Cap_cod_rat<-Lag(data$Cap_cod_rat,1)

##### Lag -2 ####

data$Puff<-Lag(data$Puff,2)
data$Mack<-Lag(data$Mack,2)
data$H_0<-Lag(data$H_0,2)
data$T_Ssum<-Lag(data$T_Ssum,2)
data$Cal_fin<-Lag(data$Cal_fin,2)
data$ACW_stress<-Lag(data$ACW_stress,2)
data$ACW_stab<-Lag(data$ACW_stab,2)
data$mean_hatch<-Lag(data$mean_hatch,2)
data$Sal_I2<-Lag(data$Sal_I2,2)
data$Sal_I1<-Lag(data$Sal_I1,2)
data$SSB_H<-Lag(data$SSB_H,2)
data$Age_index1<-Lag(data$Age_index1,2)
data$Age_index2<-Lag(data$Age_index2,2)
data$Ztot<-data$Ztot/2
data$Zcod<-data$Zcod/2

aber<-data[which((data$H_R2/data$H_0)>1),]
data[which((data$H_R2/data$H_0)>1),6]<-NA
attach(data)
############################### Fit SEM ##################################################

small.data<-data.frame(na.omit(cbind(log(Cc_H),log(H_0),log(H_R2),log(Cap_cod_rat),log(Cod))))
colnames(small.data)<-c("Cod_pred","H0","H2","CapCod","Cod")
attach(small.data)

#### SEM fit #####
reg_H2<-lm(H2~H0+Cod_pred, data=small.data)
reg_pred<-lm(Cod_pred~H0+Cod+CapCod, data=small.data)


model_1<-psem(reg_H2,reg_pred)
sum.mod1<-summary(model_1, .progressBar = F)
sum.mod1$IC


reg_pred_2<-lm(Cod_pred~Cod+CapCod, data=small.data)


model_2<-psem(reg_H2,
              reg_pred_2)
sum.mod2<-summary(model_2, .progressBar = F)
sum.mod2$IC

reg_H2_2<-lm(H2~Cod_pred, data=small.data)

model_3<-psem(reg_H2_2,
              reg_pred)
sum.mod3<-summary(model_3, .progressBar = F)
sum.mod3$IC


##Export

Tab2<-rbind(cbind(sum.mod1$Cstat,sum.mod1$IC[1:2]),
      cbind(sum.mod2$Cstat,sum.mod2$IC[1:2]),
      cbind(sum.mod3$Cstat,sum.mod3$IC[1:2]))
rownames(Tab2)<-c("Model 1","Model 2","Model 3")      
save(Tab2, file = "Tab2.RData")


library("xlsx")
#D-sep test
Dsep<-rbind(sum.mod1$dTable,
sum.mod2$dTable,
sum.mod3$dTable)
write.xlsx(Dsep,"Dsep.xlsx")
save(Dsep, file = "Dsep.RData")

#Coefficients
Coef_SEM<-rbind(sum.mod1$coefficients,
            sum.mod2$coefficients,
            sum.mod3$coefficients)
write.xlsx(Coef_SEM,"Coef_SEM.xlsx")
save(Coef_SEM, file = "Coef_SEM.RData")
#Table R2
R2<-cbind(sum.mod1$R2[c(1,5)],
      sum.mod2$R2[5],
      sum.mod3$R2[5])
colnames(R2)[c(2:4)]<-c("Model 1","Model 2","Model 3")
write.xlsx(R2,"R2.xlsx")
save(R2, file = "R2.RData")
