library(ggplot2) #plot
library(mgcv) #Quantgam
library(qgam) #Quantgam
library(visreg) #package gam
library(quantreg) #Quant reg
library(Hmisc) #Lag function
library(dplyr) 
library(mgcViz) #QAGM check
library(gridExtra)
library(grid)
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
data$mean_hatch<-Lag(data$mean_hatch,-2)
data$Sal_I2<-Lag(data$Sal_I2,-2)
data$Sal_I1<-Lag(data$Sal_I1,-2)
data$SSB_H<-Lag(data$SSB_H,-2)
data$Age_index1<-Lag(data$Age_index1,-2)
data$Age_index2<-Lag(data$Age_index2,-2)
#attach(data)

#################################### Relationships data #################################
x<-c("T_Ssum","Cal_fin","SSB_H","Age_index1","mean_hatch","Mack",
     "ACW_stress","Sal_I2","H_0","Cap_cod_rat","Cod","Cc_H")
y<-c("Cal_fin","H_0","H_0","mean_hatch","H_0","H_0","Sal_I2","H_0",
     "H_R2","Cc_H","Cc_H","H_R2")
index<-c(1:12)
RD<-data.frame(x,y,index)

################################### Loop Quantile Regression #################################
dat<-NULL
model<-NULL
pred<-NULL
output_dat<-list()
output_model<-list()
output_pred<-list()

#Calculations
for (i in 1:length(index)){
  X<-as.character(RD[i,1])
  Y<-as.character(RD[i,2])
  
  dat<-data.frame(na.omit(cbind(get(X),get(Y))))
  mod<-qgam(X2~s(X1,k=3),data=dat,qu=0.9)
  pred<- predict(mod, newdata = dat, se=TRUE)
  colnames(dat)<-c(X,Y)
  
  output_dat[[i]] <- dat
  output_model[[i]] <- mod
  output_pred[[i]] <- pred
  
  }

#Function to plot output of loop
plot_quantile<-function(dat.list,pred.list){

  df<-as.data.frame(dat.list[[1]])   #Unlist because list are not supported
  df_pred<-as.data.frame(pred.list[[1]]) # in ggplot
  
  p<-ggplot()+
        xlab(colnames(df[1]))+
        ylab(colnames(df[2]))+
        geom_ribbon(aes(ymax=df_pred$fit + 2*df_pred$se.fit,ymin=df_pred$fit - 2*df_pred$se.fit,x=df[,1]),
                    fill = "slategray3")+
        geom_line(aes(y=df_pred$fit,x=df[,1]),data=df,colour="red",size=1.3)+
        geom_point(aes(y=df[,2],x=df[,1]),data=df,size=1.7)
  
  print(p)

}
  
#Create all the plots, and save into a list
plist<-list()
for (i in 1:length(index)){
  pl<-plot_quantile(output_dat[i],output_pred[i])
  plist[[i]]<-pl
}

############################################ Plot #################################################


#### Abundance at age 0

grid.arrange(plist[[2]],plist[[3]],plist[[5]],
             plist[[6]],plist[[8]],ncol=2)

#### Recruitment at age 2

grid.arrange(plist[[9]],plist[[12]],ncol=2)

#### cal fin
plist[[1]]

#### Hatch timing
plist[[4]]

#### Salinity
plist[[7]]

#### Cod consumption
grid.arrange(plist[[10]],plist[[11]],ncol=2)