library(rWind)
library(raster)
library(rworldmap)
library(gdistance)
library(fields)
library(lubridate)
library(shape)
library(RNCEP)
library(abind)

setwd("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring")

#### Download data from NCEP ####
uwind <- NCEP.gather(variable='uwnd.sig995', level='surface',months.minmax=c(4,8), 
                          years.minmax=c(2017,2018),lat.southnorth=c(58,70), 
                          lon.westeast=c(2,20),
                          reanalysis2 = FALSE, return.units = TRUE)

vwind <- NCEP.gather(variable='vwnd.sig995', level='surface',months.minmax=c(4,8), 
                     years.minmax=c(2017,2018),lat.southnorth=c(58,70), 
                     lon.westeast=c(2,20),
                     reanalysis2 = FALSE, return.units = TRUE)

##### Combine U-Wind and V-wind arrays ####

#Names of dimensions
row.names<-unlist(dimnames(uwind)[1])
column.names<-unlist(dimnames(uwind)[2])
time.names<-unlist(dimnames(uwind)[3])
axe.names<-c("U","v")
liste.names<-list(row.names,column.names,time.names,axe.names)

## 4D array with U-wind and V-wind
zwind<-array(c(uwind ,vwind),dim=c(dim(vwind),2),dimnames=liste.names)

##### Function to calcultate projection on angle in radians (here 45° or pi/4) #### 
library(REdaS)

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
  
  #print(paste("x=",Z[1],"y=",Z[2]))
  #print(paste("beta=",beta))
  #print(paste("alpha=",alpha))
  #print(paste("pz=",pz))
  #print(pz)
  
}


#### Stress calculation ####
stress_45<-apply(z,MARGIN=c(1,2,3),FUN=calcul_projection)



