larvae<-get(load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/original_data/LARVAE.RData"))
larvae$date <- as.Date(paste(larvae$yyyy,larvae$mm,larvae$dd,sep="-"), "%Y-%m-%d")

subset_larvae<-larvae[9:16]
delay<-c(2,5,9,11,18,26,30)
stg<-c(colnames(larvae)[9:15])
ref<-data.frame(stg,delay)

hatch_date<-NULL
numb_egg<-NULL
mydata <- list()

for(i in 1 : 7){
numb_egg<-(subset_larvae[,i]/(0.9^ref$delay[i]))
hatch_date<-subset_larvae[,8]-ref$delay[i]
mydata[[i]]<-data.frame(hatch_date,numb_egg)
}

library(rlist)
mydf <- list.rbind(mydata)


#############

df<-aggregate(mydf$numb_egg, by=list(Category=mydf$hatch_date), FUN=sum)
colnames(df)<-c("hatch_date","numb_egg")

df$Year<-format(df$hatch_date,"%Y")

cumperc<-function(x){
  100*cumsum(x)/sum(x)
}

df$cumper<-unlist(tapply(df$numb_egg, df$Year, FUN =cumperc )) ######cum sum <- relative

sup_mean<-df[which(df$cumper>=50),1]
year<-format(df[which(df$cumper>=50),1],"%Y")
mean_hatch<-data.frame(sup_mean,year )

blabla<-NULL
blablalist<-list()
for(i in 1987:2016){
  blabla<-first(mean_hatch[which(mean_hatch$year==i),1])
  blablalist[[i-1986]]<-blabla
}

d <- do.call("c", blablalist)


inf_mean<-df[which(df$cumper<50),1]
year2<-format(df[which(df$cumper<50),1],"%Y")
mean_hatch_inf<-data.frame(inf_mean,year2 )

blabla2<-NULL
blablalist2<-list()
for(i in 1987:2016){
  blabla2<-last(mean_hatch_inf[which(mean_hatch_inf$year2==i),1])
  blablalist2[[i-1986]]<-blabla2
}

d2 <- do.call("c", blablalist2)
cbind(df[df$hatch_date %in% d,4],df[df$hatch_date %in% d2,4])



library(lubridate)
year<-c(1987:2016)
Mean_Hatch_Date<-data.frame(yday(d),year )
names(Mean_Hatch_Date)<-c("mean_hatch","year")
save(Mean_Hatch_Date,file="C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data_preparation/output/Mean_Hatch_Date.RData")


