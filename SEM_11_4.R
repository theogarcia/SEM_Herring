setwd("C:/Users/moi/Desktop/Stage/Script/SEM_Herring")
#### Packages ####
library(readr)
library(ggplot2)
library(mgcv)
library(qgam)
library(visreg)
library(quantreg)
library(gridExtra)
library(piecewiseSEM)
#### Data ####
data <- read.csv("data_1.csv", header=T, sep=";",dec=".")
attach(data)

data[,dim(data)[2]+1]<-(Ca_RT*10^9)/(C_RT*10^3)
colnames(data)[dim(data)[2]]<-"rat_Ca_C"

data[,dim(data)[2]+1]<-(SSB_Ca*10^9)/(C_RT*10^3)
colnames(data)[dim(data)[2]]<-"rat_Ca_C2"

large_cod<-C_R3+C_R4+C_R5+C_R6
a<-H_0[c(3:length(H_0),NA,NA)] #Lagged t-2 vector

b<-C_cH[c(2:length(C_cH),NA)] #Lagged t-1 vector

c<-large_cod[c(2:length(large_cod),NA)] #Lagged t-1 vector

d<-data$rat_Ca_C[c(2:length(data$rat_Ca_C),NA)] #Lagged t-1 vector

dataSEM<-data.frame(na.omit(cbind(years,c,d,a,b,H_R2)))
colnames(dataSEM)<-c("years","large_codlag","rat_Ca_Clag","H_0lag","C_cHlag","H_R2")
attach(dataSEM)

pairs(dataSEM)

##### Visualization of simple relationships ####

p1 <- ggplot(data=dataSEM, aes(x=large_codlag, y=C_cHlag))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method="auto",se=F)

p2 <- ggplot(data=dataSEM, aes(x=rat_Ca_Clag, y=C_cHlag))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method="auto",se=F)

p3 <- ggplot(data=dataSEM, aes(x=H_0lag, y=C_cHlag))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method="auto",se=F)

p4 <- ggplot(data=dataSEM, aes(x=C_cHlag, y=H_R2))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method="auto",se=F)

p5 <- ggplot(data=dataSEM, aes(x=H_0lag, y=H_R2))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method="auto",se=F)


grid.arrange(p1, p2, p3, p4,p5, ncol=3, nrow = 2)

##### Linear Quantile regression #####

names<-c("large_codlag","rat_Ca_Clag","H_0lag")
      
windows()
par(mfrow=c(2, 2))
for (name in names) {
  plot(C_cHlag~get(name), data=dataSEM, xlab=name,ylab="Cod consumption")
  abline(rq(C_cHlag~get(name), tau=0.95),col="red")
  abline(rq(C_cHlag~get(name), tau=0.9),col="orange")
  abline(rq(C_cHlag~get(name), tau=0.8),col="darkgreen")
  abline(lm(C_cHlag~get(name)),col="black")}

dev.off()
model1<-rq(H_R2~get(name), tau=0.95)
names<-c("C_cHlag","H_0lag")
windows()
par(mfrow=c(2, 1))
for (name in names) {
  plot(H_R2~get(name), data=dataSEM, xlab=name,ylab="Herring recruitment")
  abline(rq(H_R2~get(name), tau=0.95),col="red")
  abline(rq(H_R2~get(name), tau=0.9),col="orange")
  abline(rq(H_R2~get(name), tau=0.8),col="darkgreen")
  abline(lm(H_R2~get(name)),col="black")}
dev.off()


##### Quantile Gam 0.9 ####
quant_gam<-function(t){
  get( getOption( "device" ) )()
  split.screen( figs = c( 2, 1 ) )
  split.screen( figs = c( 1, 3 ), screen = 1 )
  split.screen( figs = c( 1, 2 ), screen = 2 )
      screen( 3 )
       plot(qgam(C_cHlag~s(large_codlag,k=3),data=dataSEM,qu=0.9),se=T)
       abline(h=0)
      screen( 4 )
       plot(qgam(C_cHlag~s(rat_Ca_Clag,k=3),data=dataSEM,qu=0.9),se=T)
       abline(h=0)
      screen( 5 )
        plot(qgam(C_cHlag~s(H_0lag),data=dataSEM,qu=t),se=T)
        abline(h=0)
      screen( 6 )
       plot(qgam(H_R2~s(C_cHlag),data=dataSEM,qu=t),se=T)
       abline(h=0)
      screen( 7 )
        plot(qgam(H_R2~s(H_0lag),data=dataSEM,qu=t),se=T)
        abline(h=0)
      close.screen( all = TRUE )
  }

quant_gam(0.9)
quant_gam(0.95)
##### Modeling this relationships #####

##### Model 1 ####
### Cod consumption 

cod_lm<-gam(C_cHlag~large_codlag+H_0lag+rat_Ca_Clag,data=dataSEM)
summary(cod_lm)

cod_gam1<-gam(C_cHlag~s(large_codlag)+s(H_0lag)+s(rat_Ca_Clag),data=dataSEM)
summary(cod_gam1)

cod_gam2<-gam(C_cHlag~s(large_codlag)+H_0lag+s(rat_Ca_Clag),data=dataSEM)
summary(cod_gam2)

AIC(cod_gam1,cod_gam2)
AIC(cod_gam1,cod_lm) #Cod_gam1 better

visreg2d(cod_gam1, xvar='large_codlag', yvar='rat_Ca_Clag', scale='response')
visreg2d(cod_lm, xvar='H_0lag', yvar='rat_Ca_Clag', scale='response')
visreg2d(cod_lm, xvar='H_0lag', yvar='large_codlag', scale='response')

### Herring recruitment

Her_lm<-gam(H_R2~C_cHlag+H_0lag,data=dataSEM)
summary(Her_lm)

Her_gam1<-gam(H_R2~s(C_cHlag)+s(H_0lag),data=dataSEM)
summary(Her_gam1)

AIC(Her_lm,Her_gam1) #Her_gam1 better
visreg2d(Her_gam1, xvar='C_cHlag', yvar='H_0lag', scale='response')
visreg2d(Her_lm, xvar='C_cHlag', yvar='H_0lag', scale='response')

#### SEM model 1 ####

#Gam explain better variability but here we use linear model in SEM, non-linear relationships are 
#not implemented in the package

BarentsSEM1<-psem(lm(C_cHlag~large_codlag+rat_Ca_Clag+H_0lag),
                    lm(H_R2~C_cHlag+H_0lag),
                    data=dataSEM
                    
)
summary(BarentsSEM1)
#### SEM model 2 ####

#Gam explain better variability but here we use linear model in SEM, non-linear relationships are 
#not implemented in the package

BarentsSEM2<-psem(lm(C_cHlag~large_codlag+H_0lag),
                  lm(H_R2~C_cHlag+H_0lag),
                  data=dataSEM
                  
)
summary(BarentsSEM2)


