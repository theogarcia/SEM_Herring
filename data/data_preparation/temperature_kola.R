KOLA<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/KOLA.RData"))
Temp_kola<-apply(KOLA[,2:5],1,mean)
require(forecast)
library(lubridate)
year<-c(1951:2017)
smooth_temp_kola<-ma(Temp_kola, order=5)
Temperature_Kola<-data.frame(Temp_kola,smooth_temp_kola,year )

save(Temperature_Kola,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Temperature_Kola.RData")
