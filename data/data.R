#########################################################################
#########################  LOAD DATA  ###################################
#########################################################################

WGIDE<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGIDE.RData"))
WGINOR<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGINOR.RData"))
AFWG<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/AFWG.RData"))
SJOMIL<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/SJOMIL.RData"))
Salinity<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/Salinity.RData"))
ACWstress<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/ACWstress.RData"))
Index_perc_str<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Index_perc.RData"))
Index_pop_str<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Index_pop_str.RData"))
Mean_Hatch_Date<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Mean_Hatch_Date.RData"))
Cod_cons<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Cod.RData"))
Kola<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Temperature_Kola.RData"))
TORESEN<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/TORESEN.RData"))

######################## Select Ts in datasets ###########################
TORESEN<-TORESEN[,c(1,27,29)]
Mack<-apply(WGIDE[,c(38:50)],MARGIN=1,FUN=sum)
wgide<-data.frame(WGIDE[,c(1,2,13)],Mack)

wginor<-data.frame(WGINOR[,c(1,2)])

Cod<-apply(AFWG[,c(2:5)],MARGIN=1,FUN=sum)
Cap_cod_rat<-(AFWG$SSB_Ca*10^9)/(AFWG$C_RT*10^3)
afwg<-data.frame(AFWG[,c(1,24)],Cod,Cap_cod_rat)

colnames(Salinity)[1]<-c("years")

sjomil<-data.frame(SJOMIL[,c(1,3,4)])

acw_2<-data.frame(ACWstress$mean_year,ACWstress$sd_year)
years <- rownames(acw_2)
rownames(acw_2) <- NULL
acw <- cbind(years,acw_2)
#Cod_cons<-Cod_cons[,c(1,4)]
colnames(acw)[c(2,3)]<-c("ACW_stress","ACW_stab")

colnames(Index_perc_str)<-c("years","Age_index2")
colnames(Index_pop_str)<-c("years","Age_index1")

colnames(Mean_Hatch_Date)[2]<-"years"

colnames(Cod_cons)[2]<-"Cc_H"


colnames(Kola)[3]<-c("years")
######################### Merge into an unique dataset ##################################

data<-Reduce(function(x,y) merge(x = x, y = y, by = "years",all=T), list(wgide, wginor, afwg,sjomil,acw,Index_perc_str,Index_pop_str,Salinity,Mean_Hatch_Date,Cod_cons,Kola,TORESEN))
save(data, file = "C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data.Rdata")

