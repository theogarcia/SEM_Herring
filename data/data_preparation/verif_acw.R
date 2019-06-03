library(dplyr)
library(quantreg)
#### Align zero on plot #####
new_lim <- function(a, type = 1) {
  newdata_ratio <-  NULL
  i <- type * 2 - 1
  old_lim <- par("usr")[i:(i+1)] + c(diff(par("usr")[i:(i+1)]) * 0.04 / 1.08, 
                                     diff(par("usr")[i:(i+1)]) * -0.04 / 1.08)
  old_ratio <- old_lim[1] / old_lim[2]
  newdata_ratio <- if (max(a) <= 0) -1.0e+6 else min(a) / max(a)
  if (old_ratio >= newdata_ratio ) {
    new_min <- min(a)
    new_max <- min(a) / old_ratio
  } else {
    new_min <- max(a) * old_ratio
    new_max <- max(a)
  }
  c(new_min, new_max)
}
#####
Sal<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/Salinity.RData"))
ACW<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/ACW.RData"))
ACW_bis<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/oystein.RData"))
AFWG<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/AFWG.RData"))
Toresen<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/TORESEN.RData"))
WGIDE<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGIDE.RData"))

###### Compare TS ACW #####

par(mar = c(5,5,2,5))
plot(ACW$mean_year~c(1948:2018), type="l",xlab="Year", ylab="ACW",ylim=c(-0.09,0.2) )
par(new = T)
plot(ACW_bis$ACW_Men~c(1948:2018), type="l",col="red",ylim = new_lim(ACW_bis$ACW_Men, 2),xlab=NA, ylab=NA, axes=F)
axis(side = 4, col="red",col.axis="red")
mtext(side = 4, line = 3, 'ACW oystein', col="red")


#### Plot ACW sal #####
windows()
par(mfrow=c(2,2))
plot(ACW$mean_year~Sal$Sal_I2[-c(1:12)],ylab="ACW",xlab="Sal_I2")
text(x=-3, y=-0.05,labels=paste("cor=",round(cor(ACW$mean_year,Sal$Sal_I2[-c(1:12)]),3)))
plot(ACW_bis$ACW_Men~Sal$Sal_I2[-c(1:12)],ylab="ACW_O",xlab="Sal_I2")
text(x=-3, y=-0.005,labels=paste("cor=",round(cor(ACW_bis$ACW_Men,Sal$Sal_I2[-c(1:12)]),3)))
plot(ACW$mean_year~Sal$Sal_I1[-c(1:12)],ylab="ACW",xlab="Sal_I1")
text(x=-2, y=-0.05,labels=paste("cor=",round(cor(ACW$mean_year,Sal$Sal_I1[-c(1:12)]),3)))
plot(ACW_bis$ACW_Men~Sal$Sal_I1[-c(1:12)],ylab="ACW_O",xlab="Sal_I1")
text(x=-2, y=-0.005,labels=paste("cor=",round(cor(ACW_bis$ACW_Men,Sal$Sal_I1[-c(1:12)]),3)))

#### H_0~Sal & ACW ####
windows()
par(mfrow=c(2,2))
plot(AFWG$H_0[-c(1:73,112)]~Sal$Sal_I2[-c(1:44,83)],ylab="H_0",xlab="Sal_I2")
abline(rq(AFWG$H_0[-c(1:73,112)]~Sal$Sal_I2[-c(1:44,83)],tau=0.95), col="red")
abline(rq(AFWG$H_0[-c(1:73,112)]~Sal$Sal_I2[-c(1:44,83)],tau=0.9), col="orange")
plot(AFWG$H_0[-c(1:73,112)]~Sal$Sal_I1[-c(1:44,83)],ylab="H_0",xlab="Sal_I1")
abline(rq(AFWG$H_0[-c(1:73,112)]~Sal$Sal_I1[-c(1:44,83)],tau=0.95), col="red")
abline(rq(AFWG$H_0[-c(1:73,112)]~Sal$Sal_I1[-c(1:44,83)],tau=0.9), col="orange")
plot(AFWG$H_0[-c(1:73,112)]~ACW$mean_year[-c(1:32,71)],ylab="H_0",xlab="ACW")
abline(rq(AFWG$H_0[-c(1:73,112)]~ACW$mean_year[-c(1:32,71)],tau=0.95), col="red")
abline(rq(AFWG$H_0[-c(1:73,112)]~ACW$mean_year[-c(1:32,71)],tau=0.9), col="orange")
plot(AFWG$H_0[-c(1:73,112)]~ACW_bis$ACW_Men[-c(1:32,71)],ylab="H_0",xlab="ACW O")
abline(rq(AFWG$H_0[-c(1:73,112)]~ACW_bis$ACW_Men[-c(1:32,71)],tau=0.95), col="red")
abline(rq(AFWG$H_0[-c(1:73,112)]~ACW_bis$ACW_Men[-c(1:32,71)],tau=0.9), col="orange")

#### H_VPA_R0 ~ Sal & ACW ####
windows()
par(mfrow=c(2,2))
plot(Toresen$H_VPA_R0[-c(1:29,92)]~Sal$Sal_I2[-c(63:83)],ylab="H0_VPA",xlab="Sal_I2")
plot(Toresen$H_VPA_R0[-c(1:29,92)]~Sal$Sal_I1[-c(63:83)],ylab="H0_VPA",xlab="Sal_I1")
plot(Toresen$H_VPA_R0[-c(1:41,92)]~ACW$mean_year[-c(51:71)],ylab="H0_VPA",xlab="ACW_mean")
plot(Toresen$H_VPA_R0[-c(1:41,92)]~ACW_bis$ACW_Men[-c(51:71)],ylab="H0_VPA",xlab="ACW_meanbis")


#### Relations between XSAM, survey and VPA ####

plot(log(Toresen$H_VPA_R0[-c(1:74,92)])~log(AFWG$H_0[-c(1:74,92:112)]),ylab="log(VPA)",xlab="log(H_0)")
lm(log(Toresen$H_VPA_R0[-c(1:74,92)])~log(AFWG$H_0[-c(1:74,92:112)]))%>%summary()

plot(log(Toresen$H_VPA_R2[-c(1:81)])~log(WGIDE$H_R2[-c(1:81,93:112)]),ylab="log(VPA)",xlab="log(XSAM)")
lm(log(Toresen$H_VPA_R2[-c(1:81)])~log(WGIDE$H_R2[-c(1:81,93:112)]))%>%summary()




