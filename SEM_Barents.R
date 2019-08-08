library(Hmisc) #Lag function
library(dplyr) 
library(piecewiseSEM)
library(lavaan)
library(brms)
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
hist(H_0)
H_0_fact<-cut(H_0, breaks=3)

#### Plot ####

ggplot(small.data, aes(x = Cod_pred, y = H2)) + 
  geom_point(aes(size=as.factor( H0)), alpha=0.5) + 
  scale_size_discrete(range = c(3,9))+ theme(legend.position = "none") 

plot(log(Cc_H)~log(H_0))
abline(lm(log(Cc_H)~log(H_0)-1))

plot(log(H_R2)~log(Cc_H), col=H_0_fact,pch=16)
legend(4,9, legend=levels(H_0_fact),
       col=unique(H_0_fact),pch=16, cex=0.7)
abline(lm(log(H_R2)~log(Cc_H)-1))

plot(log(H_R2)~log(H_0))
abline(lm(log(H_R2)~log(H_0)-1))

#### SEM fit #####
reg_H2<-lm(H2~H0+Cod_pred, data=small.data)
reg_pred<-lm(Cod_pred~H0+Cod+CapCod, data=small.data)


model_1<-psem(reg_H2,reg_pred)
summary(model_1, .progressBar = F)

model_2<-psem(reg_H2,
              reg_pred,
              CapCod%~~%H2,
              Cod%~~%H2)
summary(model_2, .progressBar = F)

model_3<-psem(reg_H2,
              reg_pred,
              CapCod%~~%H2)
summary(model_3, .progressBar = F)

model_4<-psem(reg_H2,
              reg_pred,
              Cod%~~%H2)
summary(model_4, .progressBar = F)

AIC(model_1)
AIC(model_2)
AIC(model_3)
AIC(model_4)

### Now test H_0 -> H2
reg_H2_bis<-lm(H2~Cod_pred, data=small.data)

model_2_bis<-psem(reg_H2_bis,
                  reg_pred,
                  CapCod%~~%H2)
summary(model_2_bis, .progressBar = F)

AIC(model_2_bis)



library(DiagrammeR)

grViz("
	digraph causal {
	
	  # Nodes
	  node [shape = plaintext]
	  A [label = 'Recruits \n 0-group']
	  E [label = 'Recruits \n Age 2']
	  B [label = 'Cod \n predation']
	  C [label = 'Cod \n abundance']
	  D [label = 'Cap : Cod \n ratio']
	  
	  # Edges
	  edge [color = black,
	        arrowhead = vee]
	  rankdir = LR
	  A->{E B}
	  B->E
	  C->B
	  D->B
	  
	  # Graph
	  graph [overlap = true, fontsize = 10]
	}")
