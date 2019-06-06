library(dplyr)
Toresen<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/TORESEN.RData"))
WGIDE<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGIDE.RData"))

plot(log10(Toresen$H_VPA_R2[-c(1:81)])~log10(WGIDE$H_R2[-c(1:81,93:112)]),ylab="log(VPA)",xlab="log(XSAM)")
lm(log10(Toresen$H_VPA_R2[-c(1:81)])~log10(WGIDE$H_R2[-c(1:81,93:112)]))%>%summary()
model1<-lm(log10(Toresen$H_VPA_R2[-c(1:81)])~log10(WGIDE$H_R2[-c(1:81,93:112)]))

b<-model1$coefficients[1]
a<-model1$coefficients[2]

log_corrected<-(log10(Toresen$H_VPA_R2)-b)/a
corrected<-10^log_corrected

log_corrected2<-a*log10(WGIDE$H_R2)+b
corrected2<-10^log_corrected2



plot(Toresen$H_VPA_R2~Toresen$years, type="l", xlim=c(1900,2020), ylab="Abundance in millions", xlab="Years")
lines(corrected~Toresen$years, col="red")
lines(WGIDE$H_R2~WGIDE$years, col="orange")
lines(corrected2~WGIDE$years, col="blue")
legend("topleft", c("VPA", "XSAM", "VPA corr","XSAM corr"), col = c("black", "orange", "red", "blue"),
       text.col = "black", lty = c(1, 1, 1,1), cex=0.8,bty="n")

