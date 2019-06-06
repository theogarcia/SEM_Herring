library(RNCEP)

uflx<-NCEP.gather.gaussian("uflx.sfc",c(4,8),c(1948,2018),c(64,68),c(8.5,15))
vflx<-NCEP.gather.gaussian("vflx.sfc",c(4,8),c(1948,2018),c(64,68),c(8.5,15))

uflx_good<-uflx*-1
vflx_good<-vflx*-1

save(uflx_good,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/uflx.RData")
save(vflx_good,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/vflx.RData")


####### Map to choose the points  ########

y<-as.numeric(as.character(rownames(uflx_good)))
x<-as.numeric(as.character(colnames(uflx_good)))
coor<-expand.grid(x=x,y=y)
coor2<-data.frame(c(9,10,11,12,12),c(64,65,66,67,68))
colnames(coor2)<-c("x","y")
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(data.table)
world <- ne_countries(scale = "medium", returnclass = "sf")
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
scaler <- 0.05
poly_cont<-data.frame(c(13.125,15,9.375,7.5),c(69.5217,69.5217,63.8079,63.8079))
colnames(poly_cont)<-c("x","y")

fond_de_carte<- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(4, 20), ylim = c(61, 71), expand = FALSE)+
  scale_x_continuous() +
  scale_y_continuous()+
  geom_point(data=coor, aes(x=x, y=y),size=2)+
  geom_polygon(data=poly_cont,aes(x=x,y=y), colour="black", fill=NA)+
  geom_point(data=coor2, aes(x=x, y=y),colour = "red",size=2)

fond_de_carte

################################################
################################################

load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/uflx.RData")
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/vflx.RData")

library(SDMTools)



angle<-1.0472 # #Angle corrected
calcul_projection<-function(Z){
  
  if(Z[2]>0 & Z[1]>0){#2positifs
    
    beta<-atan2(Z[2],Z[1])
    if(beta>angle){
      alpha<-beta-angle
      
    }else{
      alpha<-angle-beta
    }
    pz<-cos(alpha)*sqrt((Z[1]^2)+(Z[2]^2))
    
  } else if(Z[2]<0 & Z[1]>0){#yneg,xpos
    
    beta_prim2<-atan2(abs(Z[2]),abs(Z[1]))
    beta<-2*pi-beta_prim2
    alpha<-beta-angle
    pz<-cos(alpha)*sqrt((Z[1]^2)+(Z[2]^2))
    
    
    
    
  }else if(Z[2]>0 & Z[1]<0){ #ypos,xneg
    
    beta_prim3<-atan2(abs(Z[2]),abs(Z[1]))
    beta<-pi-beta_prim3
    alpha<-beta-angle
    pz<-cos(alpha)*sqrt((Z[1]^2)+(Z[2]^2))
    
    
    
  }else { #xpos,y
    
    betaprim<-atan2(abs(Z[2]),abs(Z[1]))
    beta<-betaprim+pi
    alpha<-beta-angle
    pz<-cos(alpha)*sqrt((Z[1]^2)+(Z[2]^2))
    
    
  }
  
}

xpos<-x
ypos<-y
x<-rep(xpos,each=length(ypos))
y<-rep(ypos,times=length(xpos))
surface<-data.frame(x,y)
data<-pnt.in.poly(surface,poly_cont)
inside<-subset(data,data$pip==1)
coor<-expand.grid(x=xpos,y=ypos)


time<-c(dimnames(uflx_good)[[3]])


wind<-NULL
Stress_mean<-NULL
Stress_sd<-NULL
for(i in 1:length(time)){
  uflu<-c(uflx_good[,,i])
  vflu<-c(vflx_good[,,i])
  d<-cbind(coor,time[i],uflu,vflu)
  wind<-merge(x=d,y=inside, by=c("x","y"))
  wind[,6]<-apply(wind[,4:5],MARGIN=1,FUN=calcul_projection)
  colnames(wind)[6]<-c("Stress")
  Stress_mean[i]<-mean(wind$Stress)
  Stress_sd[i]<-sd(wind$Stress)
  print(i)
  
}


date<-time[1:length(Stress_mean)]
Geo_stress<-cbind(Stress_mean,Stress_sd,date)
Geo_stress<-as.data.frame(Geo_stress)
Geo_stress[,3]<-as.Date(Geo_stress2$date,"%Y_%m_%d_%H")
Geo_stress[,4]<-format(Geo_stress2$date,"%m")
colnames(Geo_stress)[4]<-"month"
Geo_stress[,5]<-format(Geo_stress$date,"%Y")
colnames(Geo_stress)[5]<-"year"
Geo_stress$Stress_mean<-as.numeric(as.character(Geo_stress$Stress_mean))
Geo_stress$Stress_sd<-as.numeric(as.character(Geo_stress$Stress_sd))
save(Geo_stress,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Geo_stress.RData")
load(file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Geo_stress.RData")


mean_year<-tapply(Geo_stress$Stress_mean, Geo_stress$year, FUN = mean)
sd_year<-tapply(Geo_stress$Stress_mean, Geo_stress$year, FUN = sd)

mean_month<-tapply(Geo_stress$Stress_mean, Geo_stress$month, FUN = mean)
sd_month<-tapply(Geo_stress$Stress_mean, Geo_stress$month, FUN = sd)

ACWstress<-list(mean_year,sd_year,mean_month,sd_month)
names(ACWstress)<-c("mean_year","sd_year","mean_month","sd_month")
save(ACWstress,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/ACWstress.RData")

par(mfrow=c(2,1))
plot(ACWstress$mean_month~c(4:8), type="b", xlab="Month", ylab="ACWstress mean", pch=4,col="red")
plot(ACWstress$sd_month~c(4:8), type="b", xlab="Month", ylab="ACWstress sd ", pch=4,col="red")
plot(ACWstress$mean_year~c(1948:2018), type="l", xlab="Year", ylab="ACWstress mean", pch=4,col="red")
plot(ACWstress$sd_year~c(1948:2018), type="l", xlab="Year", ylab="ACWstress sd ", pch=4,col="red")
