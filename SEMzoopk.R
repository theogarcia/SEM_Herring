library(readr)
library(mgcv)
library(quantreg)
library(piecewiseSEM)

data <- read.csv("data_1.csv", header=T, sep=";",dec=".")
attach(data)

##SEM_test
Cond_H0<-H_0B/H_0

datazoopk<-data.frame(na.omit(cbind(T_S,Cond_H0,Cal_fin,H_0,years)))


pairs(datazoopk)


##### Linear Quantile regression #####

names<-c("T_S","Cal_fin")

windows()
par(mfrow=c(2,2))

plot(Cal_fin~T_S, data=datazoopk)
abline(rq(Cal_fin~T_S, tau=0.95),col="red")
abline(rq(Cal_fin~T_S, tau=0.9),col="orange")
abline(rq(Cal_fin~T_S, tau=0.8),col="darkgreen")
abline(lm(Cal_fin~T_S))

      
plot.new()     

for (name in names) {
  plot(Cond_H0~get(name), data=datazoopk, xlab=name,ylab="Cond_H0")
  abline(rq(Cond_H0~get(name), tau=0.95),col="red")
  abline(rq(Cond_H0~get(name), tau=0.9),col="orange")
  abline(rq(Cond_H0~get(name), tau=0.8),col="darkgreen")
  abline(lm(Cond_H0~get(name)),col="black")}
  


dev.off()

summary(lm(Cal_fin~T_S))
summary(lm(Cond_H0~T_S))
summary(lm(Cond_H0~Cal_fin))

####
SEM1<-psem(lm(Cond_H0~T_S+Cal_fin),
                  lm(Cal_fin~T_S),
                  data=datazoopk
                  
)
summary(SEM1)

SEM2<-psem(lm(Cond_H0~T_S+Cal_fin),
           data=datazoopk
           
)
summary(SEM2)

SEM3<-psem(lm(Cond_H0~Cal_fin),
           lm(Cal_fin~T_S),
           
           data=datazoopk
           
)
summary(SEM3)



SEM4<-psem(lm(Cond_H0~T_S),
           lm(Cal_fin~T_S),
           
           data=datazoopk
           
)
summary(SEM4)
