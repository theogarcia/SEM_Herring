load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/uwind_good.RData")
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/vwind_good.RData")
library(SDMTools)



angle<-pi/4
calcul_projection<-function(Z){
  
  if(Z[2]>0 & Z[1]>0){#2positifs
    
    beta<-atan2(Z[2],Z[1])
    alpha<-beta-angle
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
    
    beta<-atan2(abs(Z[2]),abs(Z[1]))+pi
    
    alpha<-beta-angle
    pz<-cos(alpha)*sqrt((Z[1]^2)+(Z[2]^2))
    
    
  }
  
}

xpos<-seq(2,20,by=0.5)
ypos<-seq(70,58,by=-0.5)
x<-rep(xpos,each=length(ypos))
y<-rep(ypos,times=length(xpos))
surface<-data.frame(x,y)
poly_cont<-data.frame(c(5,10,14.5,11.5,3.5),c(62,64,67.5,68,62))
colnames(poly_cont)<-c("x","y")
data<-pnt.in.poly(surface,poly_cont)
inside<-subset(data,data$pip==1)

coor<-expand.grid(x=seq(70,58,by=-0.5),y=seq(2,20,by=0.5))
colnames(coor)<-c("y","x")

time<-c(dimnames(uwind_good)[[3]])

wind<-NULL
Stress_mean<-NULL
Stress_sd<-NULL
for(i in 1:5){
          uwind<-c(uwind_good[,,i])*-1
          vwind<-c(vwind_good[,,i])*-1
          d<-cbind(coor,time[i],uwind,vwind)
          wind<-merge(x=d,y=inside, by=c("x","y"))
          wind[,6]<-apply(wind[,4:5],MARGIN=1,FUN=calcul_projection)
          colnames(wind)[6]<-c("Stress")
          Stress_mean[i]<-mean(wind$Stress)
          Stress_sd[i]<-sd(wind$Stress)
          title<-paste0("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/wind_at_time/wind",i,".RData")
          print(title)
          save(wind,file=title)
}

date<-time[1:length(Stress_mean)]
Geo_stress<-cbind(Stress_mean,Stress_sd,date)


########## Data visualization for March 1st of 1948 #####

wind1<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/wind_at_time/wind1.RData"))
wind2<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/wind_at_time/wind2.RData"))
wind3<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/wind_at_time/wind3.RData"))
wind4<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/wind_at_time/wind4.RData"))

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

fond_de_carte<- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(3, 15), ylim = c(61, 69), expand = FALSE)+
  geom_polygon(data=poly_cont,aes(x=x,y=y), colour="black", fill=NA)
p1<-fond_de_carte+geom_segment(data=wind1, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.09,"cm")),  color="blue")+
  annotate("text", x = 11, y = 62,
           size=3, label = paste("Stress mean =",round(Stress_mean[1],2),"\n","Stress sd =",round(Stress_sd[1],2)))
p2<-fond_de_carte+geom_segment(data=wind2, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.09,"cm")),  color="blue")+
  annotate("text", x = 11, y = 62,
           size=3, label = paste("Stress mean =",round(Stress_mean[2],2),"\n","Stress sd =",round(Stress_sd[2],2)))
p3<-fond_de_carte+geom_segment(data=wind3, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.09,"cm")),  color="blue")+
  annotate("text", x = 11, y = 62,
           size=3, label = paste("Stress mean =",round(Stress_mean[3],2),"\n","Stress sd =",round(Stress_sd[3],2)))
p4<-fond_de_carte+geom_segment(data=wind4, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.09,"cm")),  color="blue")+
  annotate("text", x = 11, y = 62,
           size=3, label = paste("Stress mean =",round(Stress_mean[4],2),"\n","Stress sd =",round(Stress_sd[4],2)))


grid.arrange(p1,p2,p3,p4)

