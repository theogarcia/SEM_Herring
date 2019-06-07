load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/uflx.RData")
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/vflx.RData")
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Geo_stress.RData")

library(SDMTools)

########## Data visualization for March 1st of 1948 #####
xpos<-as.numeric(as.character(colnames(uflx_good)))
ypos<-as.numeric(as.character(rownames(uflx_good)))
x<-rep(xpos,each=length(ypos))
y<-rep(ypos,times=length(xpos))
surface<-data.frame(x,y)
poly_cont<-data.frame(c(13.125,15,9.375,7.5),c(69.5217,69.5217,63.8079,63.8079))
colnames(poly_cont)<-c("x","y")
data<-pnt.in.poly(surface,poly_cont)
inside<-subset(data,data$pip==1)
coor<-expand.grid(x=xpos,y=ypos)


time<-c(dimnames(uflx_good)[[3]])


wind<-NULL
for(i in 1:4){
  uwind<-c(uflx_good[,,i])
  vwind<-c(vflx_good[,,i])
  d<-cbind(coor,time[i],uwind,vwind)
  wind<-merge(x=d,y=inside, by=c("x","y"))
  title<-paste0("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/wind_at_time/wind",i,".RData")
  print(title)
  save(wind,file=title)
}
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
scaler <- 0.9


fond_de_carte<- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(5, 19), ylim = c(62, 70), expand = FALSE)+
  scale_x_continuous() +
  scale_y_continuous()+
  geom_polygon(data=poly_cont,aes(x=x,y=y), colour="black", fill=NA)+xlab(label = "Lon")+ylab(label="Lat")


fond_de_carte


p1<-fond_de_carte+geom_segment(data=wind1, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.15,"cm")),  color="blue")+
  annotate("text", x = 14, y = 63,
           size=2, label = paste("Stress mean =",round(Geo_stress[1,1],2),"\n","Stress sd =",round(Geo_stress[1,2],2)))
p2<-fond_de_carte+geom_segment(data=wind2, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.15,"cm")),  color="blue")+
  annotate("text", x = 14, y = 63,
           size=2, label = paste("Stress mean =",round(Geo_stress[2,1],2),"\n","Stress sd =",round(Geo_stress[2,2],2)))
p3<-fond_de_carte+geom_segment(data=wind3, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.15,"cm")),  color="blue")+
  annotate("text", x = 14, y = 63,
           size=2, label = paste("Stress mean =",round(Geo_stress[3,1],2),"\n","Stress sd =",round(Geo_stress[3,2],2)))
p4<-fond_de_carte+geom_segment(data=wind4, mapping= aes(x=x, y=y, xend=x+uwind*scaler, yend=y+vwind*scaler), arrow=arrow(type="closed",length=unit(0.15,"cm")),  color="blue")+
  annotate("text", x = 14, y = 63,
           size=2, label = paste("Stress mean =",round(Geo_stress[4,1],2),"\n","Stress sd =",round(Geo_stress[4,2],2)))


grid.arrange(p1,p2,p3,p4)

