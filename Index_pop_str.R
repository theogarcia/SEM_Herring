########################################################################################################
#####################Index age structure calcul (mean spawners)#########################################
########################################################################################################
setwd("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring")
data <- read.csv("data_1.csv", header=T, sep=";",dec=".")

colnames(data)

Age<-matrix(rep(2:12,31),ncol=11,byrow=T)
numb<-data[-c(1:81),5:15] #Abundance in each year class
weight<-cbind(data[-c(1:81),30:39],apply(data[-c(1:81),40:43],1,mean)) #Weight@age
colnames(weight)[11]<-"wg_H12"
mat<-cbind(data[-c(1:81),20:27],rep(1,31),rep(1,31),rep(1,31))
colnames(mat)[9:11]<-c("prop_H10","prop_H11","prop_H12")

library(abind)
mat_3D_num<-abind(Age,numb,weight,mat,rev.along=0)
mat_3D_den<-abind(numb,weight,mat,rev.along=0)
num<-apply(apply(mat_3D_num,MARGIN=c(1,2),prod),MARGIN=1,sum)
den<-apply(apply(mat_3D_den,MARGIN=c(1,2),prod),MARGIN=1,sum)
Index<-num/den
Index<-cbind(data[-c(1:81),1],Index)
#save(Index, file = "Index_pop_str.Rdata")


