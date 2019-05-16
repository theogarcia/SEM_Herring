########################################################################################################
#####################Index age structure calcul (mean spawners)#########################################
########################################################################################################
setwd("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring")
data <- read.csv("data_1.csv", header=T, sep=";",dec=".")

colnames(data)

Age<-matrix(rep(2:12,31),ncol=11,byrow=T) #Age class
numb<-data[-c(1:81),5:15] #Abundance in each year class
weight<-cbind(data[-c(1:81),30:39],apply(data[-c(1:81),40:43],1,mean)) #Weight@age
colnames(weight)[11]<-"wg_H12"
mat<-cbind(data[-c(1:81),20:27],rep(1,31),rep(1,31),rep(1,31)) #%spawner@age
colnames(mat)[9:11]<-c("prop_H10","prop_H11","prop_H12")

library(abind)
mat_3D_num<-abind(Age,numb,weight,mat,rev.along=0)
mat_3D_den<-abind(numb,weight,mat,rev.along=0)
num<-apply(apply(mat_3D_num,MARGIN=c(1,2),prod),MARGIN=1,sum)
den<-apply(apply(mat_3D_den,MARGIN=c(1,2),prod),MARGIN=1,sum)
Index<-num/den
Index<-cbind(data[-c(1:81),1],Index)
#save(Index, file = "Index_pop_str.Rdata")

########################################################################################################
#####################Index age structure calcul (mean new spawners)##################################
########################################################################################################

mat #%of mature spawners in each year class (2-12)
new_mat<-matrix(NA,dim(mat)[1],dim(mat)[2])
i<-1
j<-1

for(i in 1:dim(mat)[1]){
  
  for(j in 1:dim(mat)[2]){
    if (i-1==0 |j-1==0){
      new_mat[i,j]<-NA}
    else{new_mat[i,j]<-mat[i,j]-mat[i-1,j-1]}
  }
  
}
names<-rep(NA,length(2:12))

for(v in 2:12){
  names[v+1]<-print(paste("prop_new_mature_at_age",v))
}
names<-names[3:13]
colnames(new_mat)<-names
new_mat[,1]<-rep(0,length(new_mat[,1])) #% new mature spawners@age (assuming not at age2)


newmat_3D_num<-abind(Age,numb,weight,new_mat,rev.along=0)
newmat_3D_den<-abind(numb,weight,new_mat,rev.along=0)
num2<-apply(apply(newmat_3D_num,MARGIN=c(1,2),prod),MARGIN=1,sum)
den2<-apply(apply(newmat_3D_den,MARGIN=c(1,2),prod),MARGIN=1,sum)
Index_new<-num2/den2
Index_new<-cbind(data[-c(1:81),1],Index_new)
#save(Index_new, file = "Index_new_str.Rdata")
