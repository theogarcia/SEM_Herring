Toresen<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/TORESEN.RData"))
WGIDE<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGIDE.RData"))

plot(log10(Toresen$H_VPA_R2[-c(1:81)])~log10(WGIDE$H_R2[-c(1:81,93:112)]),ylab="log(VPA)",xlab="log(XSAM)")
lm(log10(Toresen$H_VPA_R2[-c(1:81)])~log10(WGIDE$H_R2[-c(1:81,93:112)]))%>%summary()
model1<-lm(log10(Toresen$H_VPA_R2[-c(1:81)])~log10(WGIDE$H_R2[-c(1:81,93:112)]))

b<-model1$coefficients[1]
a<-model1$coefficients[2]

log_corrected<-(log10(Toresen$H_VPA_R2)-b)/a
corrected<-10^log_corrected


par(mfrow=c(3,1))
plot(corrected~Toresen$years, type="l")
plot(Toresen$H_VPA_R2~Toresen$years, type="l")
plot(WGIDE$H_R2~WGIDE$years, type="l")

par(mfrow=c(1,2))
#plot(Toresen$H_VPA_R2[-c(1:81)]~c(1988:1998),type="l",col="green")
plot(corrected[-c(1:81)]~c(1988:1998),type="l", col="red")#,col="red")
lines(WGIDE$H_R2[-c(1:81,93:112)]~c(1988:1998))
lines(corrected2[-c(1:81,93:112)]~c(1988:1998),col="yellow")
###############
model2<-lm(log10(WGIDE$H_R2[-c(1:81,93:112)])~log10(Toresen$H_VPA_R2[-c(1:81)]))

b2<-model2$coefficients[1]
a2<-model2$coefficients[2]

log_corrected2<-(log10(WGIDE$H_R2)-b2)/a2
corrected2<-10^log_corrected2



plot(corrected2[-c(1:81,93:112)]~c(1988:1998),type="l",col="green")
lines(corrected[-c(1:81)]~c(1988:1998),col="red")

