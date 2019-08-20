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
#logN2<-log(data$H_R2)
#logN0<-log(data$H_0)
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

#################################### Relationships data #################################
x<-c("Age_index1","Age_index2","mean_hatch","ACW_stress","Sal_I2","Cap_cod_rat","Cod")
y<-c("mean_hatch","mean_hatch","H_0","Sal_I2","H_0","Cc_H","Cc_H")
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
  mod<-qgam(X2~X1,data=dat,qu=0.5)
  pred<- predict(mod, newdata = dat, se=TRUE)
  names(mod$coefficients)[2]<-X
  colnames(dat)<-c(X,Y)
  sum<-summary(mod)
  sum$formula<-paste(Y,"~",X)
  
  
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
                    fill = "slategray3")+
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

################# Ricker

model.ricker<-rq(log(data$H_0/data$SSB_H)~data$SSB_H, tau=0.9)
model.ricker.coef<-model.ricker$coefficients

alpha_up_low<-exp(summary(model.ricker)$coefficients[1,c(2,3)])
beta_up_low<--summary(model.ricker)$coefficients[2,c(2,3)]

alpha<-exp(model.ricker.coef[1])
beta<--model.ricker.coef[2]

SSB<-seq(0,8000, by=100)
R<-alpha*SSB*exp(-beta*SSB) 
R_low<-alpha_up_low[1]*SSB*exp(-beta_up_low[1]*SSB) 
R_up<-alpha_up_low[2]*SSB*exp(-beta_up_low[2]*SSB) 


dat.SSB<-na.omit(data.frame(H0=data$H_0,SSB=data$SSB_H))


p7<-ggplot()+
  xlab("SSB")+
  ylab("H0")+
  ylim(0,800000)+
  geom_ribbon(aes(ymax=R_up,
                  ymin=R_low,x=SSB),
                fill = "slategray3")+
  geom_line(aes(y=R,x=SSB),colour="red",size=1.3)+
  geom_point(aes(y=H0,x=SSB),data=dat.SSB,size=1.7)+
  geom_point(aes(y=H_0,x=SSB_H),data=aber,size=1.7,colour="red")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

p7_not_lim<-ggplot()+
  xlab("SSB")+
  ylab("H0")+
  geom_ribbon(aes(ymax=R_up,
                  ymin=R_low,x=SSB),
              fill = "slategray3")+
  geom_line(aes(y=R,x=SSB),colour="red",size=1.3)+
  geom_point(aes(y=H0,x=SSB),data=dat.SSB,size=1.7)+
  geom_point(aes(y=H_0,x=SSB_H),data=aber,size=1.7,colour="red")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))



p7_zoom<-p7_not_lim+ coord_cartesian(ylim = c(0, 800000), xlim =c(0,8000))

grid.arrange(p7,p7_zoom,p7_not_lim,ncol=2)
############## Mackerel

dat.mak<-na.omit(data.frame(H0=data$H_0,mack=data$Mack,
                            log_H0=log(data$H_0),log_mak=log(data$Mack)))

mod.mak<-qgam(log_H0~log_mak,data=dat.mak,qu=0.9)
mod.mak.pred<- predict(mod.mak, newdata = dat.mak, se=TRUE)


p8<-ggplot()+
  xlab("Mack")+
  ylab("H0")+
  geom_ribbon(aes(ymax=exp(mod.mak.pred$fit+ 2*mod.mak.pred$se.fit),
                  ymin=exp(mod.mak.pred$fit- 2*mod.mak.pred$se.fit),x=dat.mak$mack),
              fill = "slategray3")+
  geom_line(aes(y=exp(mod.mak.pred$fit),x=dat.mak$mack),data=dat.mak,colour="red",size=1.3)+
  geom_point(aes(y=aber$H_0,x=aber$Mack),data=aber,colour="red",size=1.7)+
  geom_point(aes(y=dat.mak$H0,x=dat.mak$mack),data=dat.mak,size=1.7)+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))


############## H0

dat.H0<-na.omit(data.frame(H2=data$H_R2,H0=data$H_0,log_H2=log(data$H_R2),
                            log_H0=log(data$H_0)))



b<-seq(from=1, to=9,by=0.1)
b2<-rep(NA,length=length(b))
add<-cbind(b2,b2,b2,b)
colnames(add)<-colnames(dat.H0)
new_data<- rbind(dat.H0,add)
mod.H0<-qgam(log_H2~log_H0,data=dat.H0,qu=0.9)
mod.H0.pred<- predict(mod.H0,newdata = new_data,  se=TRUE)


line_y<-exp(mod.H0.pred$fit)
line_x<-exp(new_data$log_H0)
dat.line<-data.frame(line_x,line_y)

p9<-ggplot()+
  xlab("H0")+
  ylab("H2")+
  ylim(0,150000)+
  geom_ribbon(aes(ymax=exp(mod.H0.pred$fit+ 2*mod.H0.pred$se.fit),
                  ymin=exp(mod.H0.pred$fit- 2*mod.H0.pred$se.fit),x=dat.line$line_x),
              fill = "slategray3")+
  geom_line(aes(y=line_y,x=line_x),data=dat.line,colour="red",size=1.3)+
  geom_point(aes(y=aber$H_R2,x=aber$H_0),data=aber,colour="red",size=1.7)+
  geom_point(aes(y=dat.H0$H2,x=dat.H0$H0),data=dat.H0,size=1.7)+
  geom_abline(linetype="dashed")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

############## Cod predation

dat.cod<-na.omit(data.frame(H2=data$H_R2,Cod=data$Cc_H,log_H2=log(data$H_R2),
                           log_Cod=log(data$Cc_H)))

mod.cod<-qgam(log_H2~log_Cod,data=dat.cod,qu=0.9)
mod.cod.pred<- predict(mod.cod, newdata = dat.cod, se=TRUE)

line_y_cod<-c(exp(mod.cod.pred$fit),0)
line_x_cod<-c(dat.cod$Cod,0)
dat.line.cod<-data.frame(line_x_cod,line_y_cod)

p10<-ggplot()+
  xlab("Cod")+
  ylab("H2")+
  geom_ribbon(aes(ymax=exp(mod.cod.pred$fit+ 2*mod.cod.pred$se.fit),
                  ymin=exp(mod.cod.pred$fit- 2*mod.cod.pred$se.fit),x=dat.cod$Cod),
              fill = "slategray3")+
  geom_line(aes(y=exp(mod.cod.pred$fit),x=dat.cod$Cod),data=dat.cod,colour="red",size=1.3)+
  geom_point(aes(y=aber$H_R2,x=aber$Cc_H),data=aber,colour="red",size=1.7)+
  geom_point(aes(y=dat.cod$H2,x=dat.cod$Cod),data=dat.cod,size=1.7)+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

########################## Zcod vs Ztot

p11<-ggplot()+
  xlab("Ztot")+
  ylab("Zcod")+
  ylim(0,0.5)+
  geom_point(aes(y=aber$Zcod,x=aber$Ztot),data=aber,colour="red",size=1.7)+
  geom_point(aes(y=data$Zcod,x=data$Ztot),data=data,size=1.7)+
  geom_abline(linetype="dashed")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

##################### VPA vs Survey => H0

load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/TORESEN.RData")
VPA_H0<-Lag(TORESEN_good$H_VPA_R0,+2)
VPA_H2<-TORESEN_good$H_VPA_R2
VPA<-data.frame(VPA_H0,VPA_H2)

p12<-ggplot()+
  xlab("H0_VPA")+
  ylab("H2_VPA")+
  ylim(0,120000)+
  geom_point(aes(y=VPA_H2,x=VPA_H0),data=VPA,size=1.7)+
  geom_abline(linetype="dashed")+
  theme(axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

  

##### Complete list of plots with new plots

plist[[length(plist)+1]]<-print(p7)
plist[[length(plist)+1]]<-print(p8)
plist[[length(plist)+1]]<-print(p9)
plist[[length(plist)+1]]<-print(p10)
plist[[length(plist)+1]]<-print(p11)
plist[[length(plist)+1]]<-print(p12)


######################### Plot visualization #####################
library(scales)
#All
tiff("Plot3.tiff", width = 842, height = 595)
grid.arrange(plist[[8]]+ylab("Recruits (Age0)")+ labs(title ="A)"),
             plist[[9]]+ylab("Recruits (Age0)")+xlab("Mackerel abundance")+ labs(title ="B)"),
             plist[[10]]+ylab("Recruits (Age2)")+xlab("Recruits (Age0)")+ labs(title ="C)"),
             plist[[11]]+ylab("Recruits (Age2)")+xlab("Cod predation")+ labs(title ="D)"),
             plist[[5]]+ylab("Recruits (Age0)")+xlab("Salinity anomalies")+ labs(title ="E)"),
             plist[[3]]+ylab("Recruits (Age0)")+xlab("Hatching date")+ labs(title ="F)"),
             plist[[2]]+ylab("Hatching date")+xlab("Recruit spawner (%)")+ylim(70,105)+ labs(title ="G)"),
             plist[[1]]+ylab("Hatching date")+xlab("Spawners average age")+ylim(70,105)+ labs(title ="H)"),
             plist[[4]]+ylab("Salinity anomalies")+xlab("ACW stress")+ labs(title ="I)"),
             plist[[6]]+ylab("Cod predation")+xlab("Cap:cod ratio")+ labs(title ="J)"),
             plist[[7]]+ylab("Cod predation")+xlab("Cod abundance")
             + scale_x_continuous(labels = scientific)+ labs(title ="K)"),
             plist[[12]]+ labs(title ="L)"),
             ncol=4)
dev.off()

#Annex 1: VPA2 vs VPA0
tiff("Plot2.tiff", width = 842, height = 595)
plist[[13]]+xlab("Recruits (Age0-VPA)")+ylab("Recruits (Age2-VPA)")
dev.off()

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

formula<-rbind(output_summary[[1]]$formula,
               output_summary[[2]]$formula,
               output_summary[[3]]$formula,
               output_summary[[4]]$formula,
               output_summary[[5]]$formula,
               output_summary[[6]]$formula,
               output_summary[[7]]$formula)

write.xlsx(tab_stat,"tab_stat.xlsx")
write.xlsx(formula,"formula.xlsx")

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


write.xlsx(tab_stat_90,"tab_stat_90.xlsx")
write.xlsx(formula_q90,"formula_q90.xlsx")
write.xlsx(coef_SSB,"coef_SSB.xlsx")

#Annex D
tiff("Plot1.tiff", width = 842, height = 595)
grid.arrange(p7+ labs(title ="A)"),
             p7_zoom+ labs(title ="B)"),
             p7_not_lim+ labs(title ="C)"),ncol=2)

dev.off()

