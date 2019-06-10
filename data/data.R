#########################################################################
#########################  LOAD DATA  ###################################
#########################################################################

WGIDE<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGIDE.RData"))
WGINOR<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/WGINOR.RData"))
AFWG<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/AFWG.RData"))
SJOMIL<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/SJOMIL.RData"))
Salinity<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/Salinity.RData"))
ACWstress<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/ACWstress.RData"))
Index_new_str<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Index_new_str.RData"))
Index_pop_str<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Index_pop_str.RData"))
Mean_Hatch_Date<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Mean_Hatch_Date.RData"))

######################## Select Ts in datasets ###########################

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
colnames(acw)[c(2,3)]<-c("ACW_stress","ACW_stab")

colnames(Index_new_str)<-c("years","Age_index2")
colnames(Index_pop_str)<-c("years","Age_index1")

colnames(Mean_Hatch_Date)[2]<-"years"

######################### Merge into an unique dataset ##################################

data<-Reduce(function(x,y) merge(x = x, y = y, by = "years",all=T), list(wgide, wginor, afwg,sjomil,acw,Index_new_str,Index_pop_str,Mean_Hatch_Date))
save(data, file = "C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data.Rdata")

