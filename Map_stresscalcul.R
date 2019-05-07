library(rWind)
library(raster)
library(rworldmap)
library(gdistance)
library(fields)
library(lubridate)
library(shape)
library(RNCEP)

setwd("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring")
uwind <- NCEP.gather(variable='uwnd.sig995', level='surface',months.minmax=c(4,8), 
                          years.minmax=c(1948,2018),lat.southnorth=c(58,70), 
                          lon.westeast=c(2,20),
                          reanalysis2 = FALSE, return.units = TRUE)

vwind <- NCEP.gather(variable='vwnd.sig995', level='surface',months.minmax=c(4,8), 
                     years.minmax=c(1948,2018),lat.southnorth=c(58,70), 
                     lon.westeast=c(2,20),
                     reanalysis2 = FALSE, return.units = TRUE)



z_1<-c(2,3)
z_2<-c(-2,3)
z_3<-c(2,-3)
z_4<-c(-2,-3)


angle<-pi/4

library(REdaS)

calcul_projection<-function(Z,angle){
  
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
  
  print(paste("x=",Z[1],"y=",Z[2]))
  print(paste("beta=",beta))
  print(paste("alpha=",alpha))
  print(paste("pz=",pz))

}

calcul_projection(z_1,angle)
calcul_projection(z_2,angle)
calcul_projection(z_3,angle)
calcul_projection(z_4,angle)
