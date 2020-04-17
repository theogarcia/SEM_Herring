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

load("data/data.RData")

#################################### Lag variables #####################################################
#logN2<-log(data$H_R2)
#logN0<-log(data$H_0)
##### Lag -1 #####
data$Cod2<-Lag(data$Cod2,1)
data$Cod<-Lag(data$Cod,1)
data$Cap_cod_rat<-Lag(data$Cap_cod_rat,1)
data$Temp_kola<-Lag(data$Temp_kola,1)
data$smooth_temp_kola<-Lag(data$smooth_temp_kola,1)
##### Lag -2 ####
data$H_VPA_R0<-Lag(data$H_VPA_R0,2)
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

#################################### Relationships data #################################
x<-c("Mack","H_0","Cc_H","smooth_temp_kola","Mack","smooth_temp_kola","Age_index1","Age_index2","mean_hatch","ACW_stress","Sal_I2","Cap_cod_rat","Cod2","mean_hatch","Sal_I2","ACW_stress")
y<-c("H_0","H_R2","H_R2","H_0","H_R2","H_R2","mean_hatch","mean_hatch","H_0","H_0","H_0","Cc_H","Cc_H","H_R2","H_R2","H_R2")

#x<-c(,"Mack","H_0","Cc_H","Temp_kola","smooth_temp_kola","Temp_kola","smooth_temp_kola","Temp_kola","smooth_temp_kola")
#y<-c(,"H_0","H_R2","H_R2","H_0","H_0","H_VPA_R2","H_VPA_R2","H_R2","H_R2")
index<-c(1:length(x))
RD<-data.frame(x,y,index)

############################## Quantile Regression (control) #############################
dat<-NULL
model<-NULL
pred<-NULL
add<-NULL
sum<-NULL

output_dat<-list()
output_model<-list()
output_pred<-list()
add_output<-list()
output_summary<-list()


#Calculations
for (i in 1:length(index)){
  X<-as.character(RD[i,1])
  Y<-as.character(RD[i,2])
  
  xpo<-which(colnames(aber)==X)
  ypo<-which(colnames(aber)==Y)
  add<-data.frame(X1=aber[,xpo],X2=aber[,ypo])
  
  dat<-data.frame(na.omit(cbind(get(X),get(Y))))
  mod<-qgam(X2~s(X1, k=3),data=dat,qu=0.5)
  pred<- predict(mod, newdata = dat, se=TRUE)
  names(mod$coefficients)[2]<-X
  colnames(dat)<-c(X,Y)
  sum<-summary(mod)
  sum$formula<-paste(Y,"~",X)
  
  par(mfrow=c(2,3))
  gam.check(mod)
  acf(mod$residuals)
  plot.new()
  mtext(paste0(Y,"~",X), outer = TRUE,  cex=1, line=-1.5)
  
  output_dat[[i]] <- dat
  output_model[[i]] <- mod
  output_pred[[i]] <- pred
  output_summary[[i]] <- sum
  names(output_summary)[[i]]<-sum$formula
  add_output[[i]]<-add

}

#Function to plot output of loop
plot_quantile<-function(dat.list,pred.list,add.list){
  
  df<-as.data.frame(dat.list[[1]])   #Unlist because list are not supported
  df_pred<-as.data.frame(pred.list[[1]]) # in ggplot
  df_add<-as.data.frame(add.list[[1]])
  
  p<-ggplot()+
    xlab(colnames(df[1]))+
    ylab(colnames(df[2]))+
    geom_ribbon(aes(ymax=df_pred$fit + 2*df_pred$se.fit,ymin=df_pred$fit - 2*df_pred$se.fit,x=df[,1]),
                fill = "firebrick1",alpha = 0.6)+ #Before fill slategrey
    geom_line(aes(y=df_pred$fit,x=df[,1]),data=df,colour="red",size=1.3)+
    geom_point(aes(y=df_add[,2],x=df_add[,1]),data=df_add,colour="red",size=1.7)+
    geom_point(aes(y=df[,2],x=df[,1]),data=df,size=1.7)+
    theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))
  
  print(p)
  
}

#Create all the plots, and save into a list
plist<-list()
for (i in 1:length(index)){
  pl<-plot_quantile(output_dat[i],output_pred[i],add_output[i])
  plist[[i]]<-pl
  
}

detach(data)

############################## Quantile regression (limit) ################################

#################################### Relationships data #################################
attach(data)
x<-c("Mack","H_0","Cc_H","smooth_temp_kola","Mack","smooth_temp_kola")
y<-c("H_0","H_R2","H_R2","H_0","H_R2","H_R2")
index<-c(length(plist)+1:length(x))
RD<-data.frame(x,y,index)

############################## Quantile Regression (limiting) #############################
dat<-NULL
model<-NULL
pred<-NULL
add<-NULL
sum<-NULL
output_dat_rq<-list()
output_model_rq<-list()
output_pred_rq<-list()
add_output_rq<-list()
output_summary_rq<-list()

#Calculations
for (i in 1:length(index)){
  X<-as.character(RD[i,1])
  Y<-as.character(RD[i,2])
  
  xpo<-which(colnames(aber)==X)
  ypo<-which(colnames(aber)==Y)
  add<-data.frame(X1=aber[,xpo],X2=aber[,ypo])
  
  dat<-data.frame(na.omit(cbind(get(X),get(Y))))
  mod<-qgam(X2~s(X1, k=3),data=dat,qu=0.9)
  pred<- predict(mod, newdata = dat, se=TRUE)
  names(mod$coefficients)[2]<-X
  colnames(dat)<-c(X,Y)
  sum<-summary(mod)
  sum$formula<-paste(Y,"~",X)
  
  par(mfrow=c(2,3))
  gam.check(mod)
  acf(mod$residuals)
  plot.new()
  mtext(paste0(Y,"~",X), outer = TRUE,  cex=1, line=-1.5)
  
  output_dat_rq[[i]] <- dat
  output_model_rq[[i]] <- mod
  output_pred_rq[[i]] <- pred
  output_summary_rq[[i]] <- sum
  names(output_summary_rq)[[i]]<-sum$formula
  add_output_rq[[i]]<-add
}

plot_2quantile<-function(dat.list,pred.list,add.list,
                         dat.list.rq,pred.list.rq,add.list.rq){
  
  df<-as.data.frame(dat.list[[1]])   #Unlist because list are not supported
  df_pred<-as.data.frame(pred.list[[1]]) # in ggplot
  df_add<-as.data.frame(add.list[[1]])
  df.rq<-as.data.frame(dat.list.rq[[1]])   #Unlist because list are not supported
  df_pred.rq<-as.data.frame(pred.list.rq[[1]]) # in ggplot
  df_add.rq<-as.data.frame(add.list.rq[[1]])
  
  p<-ggplot()+
    xlab(colnames(df[1]))+
    ylab(colnames(df[2]))+
    geom_ribbon(aes(ymax=df_pred$fit + 2*df_pred$se.fit,ymin=df_pred$fit - 2*df_pred$se.fit,x=df[,1]),
                fill = "firebrick1",alpha = 0.6)+
    geom_ribbon(aes(ymax=df_pred.rq$fit + 2*df_pred.rq$se.fit,ymin=df_pred.rq$fit - 2*df_pred.rq$se.fit,x=df.rq[,1]),
                fill = "dodgerblue",alpha = 0.6)+
    geom_line(aes(y=df_pred$fit,x=df[,1]),data=df,colour="red",size=1.3)+
    #geom_line(aes(y=df_pred$fit + 2*df_pred$se.fit,x=df[,1]),data=df,colour="red",linetype = "dashed",size=1.3)+
    #geom_line(aes(y=df_pred$fit - 2*df_pred$se.fit,x=df[,1]),data=df,colour="red",linetype = "dashed",size=1.3)+
    geom_line(aes(y=df_pred.rq$fit,x=df[,1]),data=df.rq,colour="blue",size=1.3,linetype="dashed")+
    #geom_line(aes(y=df_pred.rq$fit + 2*df_pred.rq$se.fit,x=df.rq[,1]),data=df.rq,colour="blue",linetype = "dashed",size=1.3)+
    #geom_line(aes(y=df_pred.rq$fit - 2*df_pred.rq$se.fit,x=df.rq[,1]),data=df.rq,colour="blue",linetype = "dashed",size=1.3)+
    geom_point(aes(y=df_add[,2],x=df_add[,1]),data=df_add,colour="red",size=1.7)+
    geom_point(aes(y=df[,2],x=df[,1]),data=df,size=1.7)+
    theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))
  
  print(p)
  
}

#Create all the plots, and save into a list
plist_test<-list()
for (i in 1:length(index)){
  pl<-plot_2quantile(output_dat[i],output_pred[i],add_output[i],output_dat_rq[i],output_pred_rq[i],add_output_rq[i])
  plist_test[[i]]<-pl
}


detach(data)

################# Ricker #####
##Control
model.ricker.c1<-rq(log(data$H_0/data$SSB_H)~data$SSB_H, tau=0.5)
model.ricker.coef.c1<-model.ricker.c1$coefficients

par(mfrow=c(1,1))
acf(model.ricker.c1$residuals)

alpha_up_low.c1<-exp(summary(model.ricker.c1)$coefficients[1,c(2,3)])
beta_up_low.c1<--summary(model.ricker.c1)$coefficients[2,c(2,3)]

alpha.c1<-exp(model.ricker.coef.c1[1])
beta.c1<--model.ricker.coef.c1[2]

SSB.c1<-seq(0,8000, by=100)
R.c1<-alpha.c1*SSB.c1*exp(-beta.c1*SSB.c1) 
R_low.c1<-alpha_up_low.c1[1]*SSB.c1*exp(-beta_up_low.c1[1]*SSB.c1) 
R_up.c1<-alpha_up_low.c1[2]*SSB.c1*exp(-beta_up_low.c1[2]*SSB.c1) 


dat.SSB.c1<-na.omit(data.frame(H_0=data$H_0,SSB=data$SSB_H))

#Limiting
model.ricker1<-rq(log(data$H_0/data$SSB_H)~data$SSB_H, tau=0.9)
model.ricker.coef1<-model.ricker1$coefficients
acf(model.ricker1$residuals)

alpha_up_low1<-exp(summary(model.ricker1)$coefficients[1,c(2,3)])
beta_up_low1<--summary(model.ricker1)$coefficients[2,c(2,3)]

alpha1<-exp(model.ricker.coef1[1])
beta1<--model.ricker.coef1[2]

SSB1<-seq(0,8000, by=100)
R1<-alpha1*SSB1*exp(-beta1*SSB1) 
R_low1<-alpha_up_low1[1]*SSB1*exp(-beta_up_low1[1]*SSB1) 
R_up1<-alpha_up_low1[2]*SSB1*exp(-beta_up_low1[2]*SSB1) 


dat.SSB1<-na.omit(data.frame(H_0=data$H_0,SSB=data$SSB_H))

#Plot
p_H0SSB<-ggplot()+
  xlab("SSB")+
  ylab("H0")+
  #ylim(0,75000)+
  #ylim(0,800000)+
  geom_ribbon(aes(ymax=R_up1,
                  ymin=R_low1,x=SSB1),
              fill = "dodgerblue",alpha = 0.6)+
  geom_ribbon(aes(ymax=R_up.c1,
                  ymin=R_low.c1,x=SSB1),
              fill = "firebrick1",alpha = 0.6)+
  geom_line(aes(y=R.c1,x=SSB.c1),colour="red",size=1.3)+
  geom_line(aes(y=R1,x=SSB1),colour="blue",size=1.3,linetype="dashed")+
  geom_point(aes(y=H_0,x=SSB),data=dat.SSB.c1,size=1.7)+
  geom_point(aes(y=H_0,x=SSB_H),data=aber,size=1.7,colour="red")+ 
  coord_cartesian(ylim = c(0,800000), xlim =c(0,8000))+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

################# Ricker #####
##Control
model.ricker.c<-rq(log(data$H_R2/data$SSB_H)~data$SSB_H, tau=0.5)
model.ricker.coef.c<-model.ricker.c$coefficients
acf(model.ricker.c$residuals)

alpha_up_low.c<-exp(summary(model.ricker.c)$coefficients[1,c(2,3)])
beta_up_low.c<--summary(model.ricker.c)$coefficients[2,c(2,3)]

alpha.c<-exp(model.ricker.coef.c[1])
beta.c<--model.ricker.coef.c[2]

SSB.c<-seq(0,8000, by=100)
R.c<-alpha.c*SSB.c*exp(-beta.c*SSB.c) 
R_low.c<-alpha_up_low.c[1]*SSB.c*exp(-beta_up_low.c[1]*SSB.c) 
R_up.c<-alpha_up_low.c[2]*SSB.c*exp(-beta_up_low.c[2]*SSB.c) 


dat.SSB.c<-na.omit(data.frame(H_R2=data$H_R2,SSB=data$SSB_H))

#Limiting
model.ricker<-rq(log(data$H_R2/data$SSB_H)~data$SSB_H, tau=0.9)
model.ricker.coef<-model.ricker$coefficients
acf(model.ricker$residuals)

alpha_up_low<-exp(summary(model.ricker)$coefficients[1,c(2,3)])
beta_up_low<--summary(model.ricker)$coefficients[2,c(2,3)]

alpha<-exp(model.ricker.coef[1])
beta<--model.ricker.coef[2]

SSB<-seq(0,8000, by=100)
R<-alpha*SSB*exp(-beta*SSB) 
R_low<-alpha_up_low[1]*SSB*exp(-beta_up_low[1]*SSB) 
R_up<-alpha_up_low[2]*SSB*exp(-beta_up_low[2]*SSB) 


dat.SSB<-na.omit(data.frame(H_R2=data$H_R2,SSB=data$SSB_H))

#Plot
p7<-ggplot()+ #H2~SSB with confidence interval
  xlab("SSB")+
  ylab("HR2")+
  #ylim(0,75000)+
  ylim(0,700000)+
  geom_ribbon(aes(ymax=R_up,
                  ymin=R_low,x=SSB),
              fill = "dodgerblue",alpha = 0.6)+
  geom_ribbon(aes(ymax=R_up.c,
                  ymin=R_low.c,x=SSB),
              fill = "firebrick1",alpha = 0.6)+
  geom_line(aes(y=R.c,x=SSB.c),colour="red",size=1.3)+
  geom_line(aes(y=R,x=SSB),colour="blue",size=1.3,linetype="dashed")+
  geom_point(aes(y=H_R2,x=SSB),data=dat.SSB.c,size=1.7)+
  #geom_point(aes(y=H_R2,x=SSB_H),data=aber,size=1.7,colour="red")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))
p7bis<-ggplot()+#H2~SSB without confidence interval
  xlab("SSB")+
  ylab("HR2")+
  #ylim(0,75000)+
  ylim(0,75000)+
  #geom_ribbon(aes(ymax=R_up,
  #                ymin=R_low,x=SSB),
  #            fill = "dodgerblue",alpha = 0.6)+
  #geom_ribbon(aes(ymax=R_up.c,
  #                ymin=R_low.c,x=SSB),
  #            fill = "firebrick1",alpha = 0.6)+
  geom_line(aes(y=R.c,x=SSB.c),colour="red",size=1.3)+
  geom_line(aes(y=R,x=SSB),colour="blue",size=1.3,linetype="dashed")+
  geom_point(aes(y=H_R2,x=SSB),data=dat.SSB.c,size=1.7)+
  #geom_point(aes(y=H_R2,x=SSB_H),data=aber,size=1.7,colour="red")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))
############################################
dat_2<-data.frame(na.omit(cbind(data$H_R2,data$SSB_H)))
colnames(dat_2)<-c("H_R2","SSB_H")
modSR9<-qgam(log(H_R2/SSB_H)~SSB_H,data=dat_2,qu=0.9)
modSR5<-qgam(log(H_R2/SSB_H)~SSB_H,data=dat_2,qu=0.5)

dat_3<-data.frame(na.omit(cbind(data$H_0,data$SSB_H)))
colnames(dat_3)<-c("H_0","SSB_H")
mod0SR9<-qgam(log(H_0/SSB_H)~SSB_H,data=dat_3,qu=0.9)
mod0SR5<-qgam(log(H_0/SSB_H)~SSB_H,data=dat_3,qu=0.5)



#######################################################
library(scales)

tiff("Plot_results_conf.tiff", res=300, height=8, width=12,units = "in" )
grid.arrange(p_H0SSB+ylab("Abundance (Age0)")+ labs(title ="A)"),
             #plist[[9+2]]+ylab("Abundance (Age0)")+xlab("Salinity anomalies")+labs(title ="B)"),
             plist[[8+2]]+ylab("Abundance (Age0)")+xlab("ACW stress")+ labs(title ="B)"),
             plist[[11]]+ylab("Abundance (Age0)")+xlab("Salinity anomalies")+ labs(title ="C)"),
             plist[[5+2]]+ylab("Hatching date")+xlab("Spawners average age")+ylim(70,110)+ labs(title ="D)"),
             plist[[6+2]]+ylab("Hatching date")+xlab("Recruit spawner (%)")+ylim(70,110)+ labs(title ="E)"),
             plist[[7+2]]+ylab("Abundance (Age0)")+xlab("Hatching date")+ labs(title ="F)"),
             plist_test[[4]]+ylab("Abundance (Age0)")+xlab("Temperature")+ labs(title ="G)"),
             plist_test[[1]]+ylab("Abundance (Age0)")+ scale_x_continuous(breaks=c(15000000,22500000,30000000),labels = scientific)+xlab("Mackerel abundance")+ labs(title ="H)"),
             plist_test[[3]]+ylab("Abundance (Age2)")+xlab("Cod predation")+ labs(title ="I)"),
             plist[[10+2]]+ylab("Cod predation")+xlab("Cap:cod ratio")+ labs(title ="J)"),
             plist[[11+2]]+ylab("Cod predation")+xlab("Cod abundance")+
               scale_x_continuous(breaks=c(500000,1500000,2500000),labels = scientific)+labs(title ="K)"),
             plist_test[[2]]+ylab("Abundance (Age2)")+xlab("Abundance (Age0)")+ labs(title ="L)"),
             ncol=4)
dev.off()


#######################################################
#sup_4T<-data[which(data$smooth_temp_kola>4),]
#library(ggrepel)

#tiff("Plot_date_temperature.tiff", res=300, height=8, width=12,units = "in" )

#plist[[4]]+geom_text_repel(aes(x =sup_4T$smooth_temp_kola,
#                               y = sup_4T$H_0,
#                               label = sup_4T$years),size = 4)+
#  xlab("Temperature")+ylab("Recruits (Age0)")
#dev.off()

#######################################################
##Plot H2 
tiff("Plot_results2_conf.tiff", res=300, height=8, width=12,units = "in" )

grid.arrange(p7+ylab("Abundance (Age2)")+ labs(title ="A)"),
             plist[[16]]+ylab("Abundance (Age2)")+xlab("ACW stress")+ labs(title ="B)"),
             plist[[15]]+ylab("Abundance (Age2)")+xlab("Salinity anomalies")+ labs(title ="C)"),
             plist[[14]]+ylab("Abundance (Age2)")+xlab("Hatching date")+ labs(title ="D)"),
             plist_test[[6]]+ylab("Abundance (Age2)")+xlab("Temperature")+ labs(title ="E)"),
             plist_test[[5]]+ylab("Abundance (Age2)")+xlab("Mackerel abundance")+ labs(title ="F)"),

             
             #p7bis+ylab("Recruits (Age2)")+ labs(title ="F)"),
             ncol=3)
dev.off()

#######################################################
tiff("Plot_Fig4.tiff", res=300, height=4, width=6,units = "in" )
plotZtotZcod<-ggplot()+
  xlab("Ztot")+
  ylab("Zcod")+
  xlim(0,1.75)+
  ylim(0,0.5)+
  geom_abline(aes(intercept = 0, slope = 1),linetype="dashed",size=1.3)+
  geom_point(aes(y=Zcod,x=Ztot),data=data,size=2)+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

plotZtotZcod
dev.off()

#######################################################
##Export
library("xlsx")
#Statistics Q0.9
tab_stat<-rbind(output_summary[[1]]$p.table[,-3],
                output_summary[[2]]$p.table[,-3],
                output_summary[[3]]$p.table[,-3],
                output_summary[[4]]$p.table[,-3],
                output_summary[[5]]$p.table[,-3],
                output_summary[[6]]$p.table[,-3],
                output_summary[[7]]$p.table[,-3])
coefficient<-row.names(tab_stat)

formula<-rbind(output_summary[[1]]$formula,
               output_summary[[2]]$formula,
               output_summary[[3]]$formula,
               output_summary[[4]]$formula,
               output_summary[[5]]$formula,
               output_summary[[6]]$formula,
               output_summary[[7]]$formula)

save(tab_stat, file = "tab_stat.RData")
save(formula, file = "formula.RData")

#Statistics Q0.9

summ_mak<-summary(mod.mak)
summ_H0<-summary(mod.H0)
summ_cod<-summary(mod.cod)

tab_stat_90<-rbind(summ_mak$p.table[,-3],
                   summ_H0$p.table[,-3],
                   summ_cod$p.table[,-3])

formula_q90<-c(summ_mak$formula,
               summ_H0$formula,
               summ_cod$formula)

coef_SSB<-summary(model.ricker)$coefficients               
rownames(coef_SSB)<-c("ln(alpha)","beta")

save(tab_stat_90, file = "tab_stat_90.RData")
save(formula_q90, file = "formula_q90.RData")
save(coef_SSB, file = "coef_SSB.RData")

#Annex D
tiff("Plot1.tiff", width = 842, height = 595)
grid.arrange(p7+ labs(title ="A)"),
             p7_zoom+ labs(title ="B)"),
             p7_not_lim+ labs(title ="C)"),ncol=2)

dev.off()

