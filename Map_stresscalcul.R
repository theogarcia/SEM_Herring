####Work directory ####
setwd("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring")
###################################################################
####Function to calculate projection of wind on a defined angle####
###################################################################
####Library required####
library(REdaS)
library(SDMTools)
library(RNCEP)
#Two function used here have been created/modified are used here: calcul_projection and NCEP.gather.surface2

####Description of argument of the function####
#xpolygon=coordinates of polygon
#ypolygon=y coordinates of polygon
#month=vector with id of month
#yearminmax=vector with month that you to calculate
#angle=angle of projection in radians

####Fonction code####
proj_wind<-function(xpolygon,ypolygon,month,yearminmax,angle)
                      {
  
#####Compile functions of projection and download of NCEP data #####
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
  
  NCEP.gather.surface2<-function (variable, months.minmax, years.minmax, lat.minmax, 
                                  lon.minmax, reanalysis2 = FALSE, return.units = TRUE, increments = NULL, 
                                  pb = NULL) {
    if (any(c("air.sig995", "lftx.sfc", "lftx4.sfc", "omega.sig995", 
              "pottmp.sig995", "pr_wtr.eatm", "pres.sfc", "rhum.sig995", 
              "slp", "mslp", "uwnd.sig995", "vwnd.sig995") == variable) == 
        FALSE) {
      stop(paste("'", variable, "'", " not a valid variable name in reference to the surface.", 
                 sep = ""))
    }
    not.in.reanalysis2 <- c("air.sig995", "lftx.sfc", "lftx4.sfc", 
                            "omega.sig995", "pottmp.sig995", "rhum.sig995", "slp", 
                            "uwnd.sig995", "vwnd.sig995")
    not.in.reanalysis1 <- c("mslp")
    if (reanalysis2 == TRUE && any(not.in.reanalysis2 == variable)) {
      reanalysis2 <- FALSE
      warning("This variable is not available at the surface in the Reanalysis II dataset.  Using Reanalysis I instead.")
    }
    if (reanalysis2 == FALSE && any(not.in.reanalysis1 == variable)) {
      reanalysis2 <- TRUE
      warning("This variable is not available at the surface in the Reanalysis I dataset.  Using Reanalysis II instead.")
    }
    possible.lats <- rev(seq(from=-90,to=90, by=0.5))
    possible.lons <- seq(from=0,to=357.5, by=0.5)
    lat.minmax[1] <- floor(lat.minmax[1]/0.5) * 0.5
    lat.minmax[2] <- (ceiling(lat.minmax[2]/0.5) * 0.5)
    lon.minmax[1] <- floor(lon.minmax[1]/0.5) * 0.5
    lon.minmax[2] <- ifelse((ceiling(lon.minmax[2]/0.5) * 0.5) > 
                              357.5, 357.5, (ceiling(lon.minmax[2]/0.5) * 0.5))
    lat.range <- c(which(possible.lats == lat.minmax[2]) - 1, 
                   which(possible.lats == lat.minmax[1]) - 1)
    lon.range <- c(which(possible.lons == lon.minmax[1]) - 1, 
                   which(possible.lons == lon.minmax[2]) - 1)
    years <- seq(years.minmax[1], years.minmax[length(years.minmax)])
    months <- seq(months.minmax[1], months.minmax[length(months.minmax)])
    end.day <- c()
    order <- 1
    for (i in years) {
      for (m in months) {
        if (as.numeric(format(as.Date(paste("1", m, i, sep = "/"), 
                                      format("%d/%m/%Y"), tz = "UTC") + 30, "%m")) == 
            m) {
          end.day[order] <- 31
          order <- order + 1
        }
        else if (as.numeric(format(as.Date(paste("1", m, 
                                                 i, sep = "/"), format("%d/%m/%Y"), tz = "UTC") + 
                                   29, "%m")) == m) {
          end.day[order] <- 30
          order <- order + 1
        }
        else if (as.numeric(format(as.Date(paste("1", m, 
                                                 i, sep = "/"), format("%d/%m/%Y"), tz = "UTC") + 
                                   28, "%m")) == m) {
          end.day[order] <- 29
          order <- order + 1
        }
        else if (as.numeric(format(as.Date(paste("1", m, 
                                                 i, sep = "/"), format("%d/%m/%Y"), tz = "UTC") + 
                                   27, "%m")) == m) {
          end.day[order] <- 28
          order <- order + 1
        }
      }
    }
    beg.day <- rep(1, length(end.day))
    year.names <- c()
    loop <- 1
    for (y in 1:length(years)) {
      year.names <- append(year.names, rep(years[y], sum(end.day[loop:(loop + 
                                                                         length(months) - 1)]) * 4))
      loop <- loop + length(months)
    }
    month.names <- c()
    loop <- 1
    for (y in 1:length(years)) {
      for (m in 1:length(months)) {
        month.names <- append(month.names, sprintf("%02d", 
                                                   rep(months[m], each = end.day[loop] * 4)))
        loop <- loop + 1
      }
    }
    day.names <- c()
    for (i in 1:length(end.day)) {
      day.names <- append(day.names, sprintf("%02d", rep(seq(1, 
                                                             end.day[i]), each = 4)))
    }
    hour.names <- sprintf("%02d", rep(seq(0, 18, length.out = 4), 
                                      length(day.names)/4))
    time.names <- paste(year.names, month.names, day.names, 
                        hour.names, sep = "_")
    out.wx.data <- array(data = NA, dim = c(length(seq(lat.minmax[1], 
                                                       lat.minmax[2], by = 0.5)), length(seq(lon.minmax[1], 
                                                                                             lon.minmax[2], by = 0.5)), length(time.names)), dimnames = list(rev(seq(lat.minmax[1], 
                                                                                                                                                                     lat.minmax[2], by = 0.5)), seq(lon.minmax[1], lon.minmax[2], 
                                                                                                                                                                                                    by = 0.5), time.names))
    outdata <- c()
    scale.offset.missingvals.temp <- tempfile()
    out.temp <- tempfile()
    name <- strsplit(variable, "\\.")[[1]][1]
    level <- ifelse(length(strsplit(variable, "\\.")[[1]]) > 
                      1, strsplit(variable, "\\.")[[1]][2], "")
    loop.num <- 1
    observations <- c()
    for (year in years) {
      beg.jdate <- as.numeric(difftime(as.POSIXct(paste(year, 
                                                        "/", months[1], "/", beg.day[loop.num], " ", "0:0:0", 
                                                        sep = ""), format = "%Y/%m/%d %H:%M:%S", tz = "UTC"), 
                                       as.POSIXct(paste(year, "/1/1 0:0:0", sep = ""), 
                                                  "%Y/%m/%d %H:%M:%S", tz = "UTC"), units = "hours"))/6
      end.jdate <- as.numeric(difftime(as.POSIXct(paste(year, 
                                                        "/", months[length(months)], "/", end.day[loop.num * 
                                                                                                    length(months)], " ", "18:0:0", sep = ""), format = "%Y/%m/%d %H:%M:%S", 
                                                  tz = "UTC"), as.POSIXct(paste(year, "/1/1 0:0:0", 
                                                                                sep = ""), "%Y/%m/%d %H:%M:%S", tz = "UTC"), units = "hours"))/6
      columns <- length(seq(lon.minmax[1], lon.minmax[2], 
                            by = 0.5)) + 1
      actual.columns <- columns - 1
      rows <- length(seq(lat.minmax[1], lat.minmax[2], by = 0.5))
      if (loop.num == 1) {
        trying.out <- 1
        fail <- 0
        while (trying.out != 0) {
          trying.out <- try(download.file(paste("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis", 
                                                ifelse(reanalysis2 == TRUE, "2", ""), "/surface/", 
                                                variable, ".", year, ".nc.das", sep = ""), 
                                          mode = "wb", method = "libcurl", scale.offset.missingvals.temp), 
                            silent = TRUE)
          fail <- fail + 1
          if (fail >= 5) {
            stop(paste("\nThere is a problem connecting to the NCEP database with the information provided.\n\t\nTry entering http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis", 
                       ifelse(reanalysis2 == TRUE, "2", ""), "/surface/", 
                       variable, ".", year, ".nc.das into a web browser to obtain an error message.", 
                       sep = ""))
          }
        }
        add.offset <- if (reanalysis2 == TRUE) {
          as.numeric(strsplit(strsplit(grep("add_offset", 
                                            x = readLines(scale.offset.missingvals.temp), 
                                            value = TRUE, fixed = TRUE), ";")[[1]][1], 
                              "add_offset ")[[1]][2])
        }
        else {
          0
        }
        scale.factor <- if (reanalysis2 == TRUE) {
          as.numeric(strsplit(strsplit(grep("scale_factor", 
                                            x = readLines(scale.offset.missingvals.temp), 
                                            value = TRUE, fixed = TRUE), ";")[[1]][1], 
                              "scale_factor ")[[1]][2])
        }
        else {
          1
        }
        missing.values <- as.numeric(strsplit(strsplit(grep("missing_value", 
                                                            x = readLines(scale.offset.missingvals.temp), 
                                                            value = TRUE, fixed = TRUE), ";")[[1]][1], "missing_value ")[[1]][2])
        if (return.units == TRUE) {
          var.loc.units <- min(grep(name, x = readLines(scale.offset.missingvals.temp), 
                                    value = FALSE, fixed = TRUE))
          all.loc.units <- grep("String units", x = readLines(scale.offset.missingvals.temp), 
                                value = FALSE, fixed = TRUE)
          all.units <- grep("String units", x = readLines(scale.offset.missingvals.temp), 
                            value = TRUE, fixed = TRUE)
          units <- strsplit(all.units[which(all.loc.units > 
                                              var.loc.units)[1]], "\"")[[1]][2]
        }
      }
      trying.out <- 1
      fail <- 0
      while (trying.out != 0) {
        trying.out <- try(download.file(paste("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis", 
                                              ifelse(reanalysis2 == TRUE, "2", ""), "/surface/", 
                                              variable, ".", year, ".nc.ascii?", name, "[", 
                                              beg.jdate, ":", end.jdate, "][", lat.range[1], 
                                              ":", ifelse(length(lat.range) > 1, lat.range[2], 
                                                          lat.range[1]), "][", lon.range[1], ":", ifelse(length(lon.range) > 
                                                                                                           1, lon.range[2], lon.range[1]), "]", sep = ""), 
                                        mode = "wb", method = "libcurl", out.temp), 
                          silent = TRUE)
        fail <- fail + 1
        if (fail >= 5) {
          stop(paste("\nThere is a problem connecting to the NCEP database with the information provided.\n\t\nTry entering http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis", 
                     ifelse(reanalysis2 == TRUE, "2", ""), "/surface/", 
                     variable, ".", year, ".nc.ascii?", name, "[", 
                     beg.jdate, ":", end.jdate, "][", lat.range[1], 
                     ":", ifelse(length(lat.range) > 1, lat.range[2], 
                                 lat.range[1]), "][", lon.range[1], ":", 
                     ifelse(length(lon.range) > 1, lon.range[2], 
                            lon.range[1]), "] into a web browser to obtain an error message.", 
                     sep = ""))
        }
      }
      outdata <- read.table(file = out.temp, sep = ",", skip = 12, 
                            header = FALSE, na.strings = missing.values, nrows = ((end.jdate - 
                                                                                     beg.jdate) + 1) * rows)
      observations[loop.num] <- ifelse(loop.num == 1, length(outdata$V2)/rows, 
                                       observations[loop.num - 1] + length(outdata$V2)/rows)
      obs <- seq(ifelse(loop.num == 1, 1, observations[loop.num - 
                                                         1] + 1), observations[loop.num])
      for (i in 1:length(obs)) {
        n <- seq(1, length(outdata$V2), by = rows)
        t.out <- outdata[c(seq(n[i], n[i] + rows - 1)), 
                         c(2:columns)] * scale.factor + add.offset
        t.out[t.out == missing.values * scale.factor + add.offset] <- NA
        out.wx.data[1:rows, 1:actual.columns, obs[i]] <- as.matrix(t.out)
      }
      if (!is.null(pb)) {
        cval <- pb$getVal()
        Sys.sleep(1e-06)
        setTkProgressBar(pb, cval + 1, label = paste(round((cval + 
                                                              1)/increments * 100, 0), "% done"))
      }
      loop.num <- loop.num + 1
      unlink(c(out.temp, scale.offset.missingvals.temp))
    }
    if (return.units == TRUE) {
      print(noquote(paste("Units of variable '", variable, 
                          "' are ", units, sep = "")))
    }
    if (!is.null(pb)) {
      if (pb$getVal() == increments) {
        close(pb)
      }
    }
    return(out.wx.data)
  }
  
###########Choose coordinates of polygon to select ####
      poly_cont<-data.frame(xpolygon,ypolygon)
      colnames(poly_cont)<-c("x","y")
      
############Choose coordinates of big area to download ####
      xpos<-seq(min(xpolygon),max(xpolygon),by=0.5)
      ypos<-seq(min(ypolygon),max(ypolygon),by=0.5)
      x<-rep(xpos,each=length(ypos))
      y<-rep(ypos,times=length(xpos))
      surface<-data.frame(x,y)
      
      
      data<-pnt.in.poly(surface,poly_cont)
      
      inside<-data[which(data$pip==1),1:2]
      id_inside<-as.numeric(row.names(inside))
#########Loop to have mean and sd of stress#####
      year<-min(yearminmax):max(yearminmax)

      
      stress_mean<-rep(NA,times=length(year))
      vwind_mean<-rep(NA,times=length(year))
      uwind_mean<-rep(NA,times=length(year))      
      stress_sd<-rep(NA,times=length(year))

          for(i in year){vwind<- NCEP.gather.surface2(variable='vwnd.sig995',months.minmax=month, 
                              years.minmax=c(i,i),lat.minmax=c(min(ypos),max(ypos)), 
                              lon.minmax=c(min(xpos),max(xpos)),return.units = F)

                        print(paste(i,"v-wind downloaded"))
                        
                        uwind<- NCEP.gather.surface2(variable='uwnd.sig995',months.minmax=month, 
                             years.minmax=c(i,i),lat.minmax=c(min(ypos),max(ypos)), 
                             lon.minmax=c(min(xpos),max(xpos)),return.units = F)
                        print(paste(i,"u-wind downloaded"))

                        vwind2<-vwind*-1
                        uwind2<-uwind*-1

                        ####Mean
                        vmean<-apply(vwind2,MARGIN=c(1,2),mean)
                        umean<-apply(uwind2,MARGIN=c(1,2),mean)
                        
                        vwind_mean[i-(min(year-1))]<-mean(as.vector(vmean[id_inside]))
                        uwind_mean[i-(min(year-1))]<-mean(as.vector(umean[id_inside]))                        

                        row.names<-unlist(dimnames(vmean)[1])
                        column.names<-unlist(dimnames(vmean)[2])
                        axe.names<-c("U","v")
                        liste.names<-list(row.names,column.names,axe.names)
                        
                        zwind<-array(c(umean ,vmean),dim=c(dim(vmean),2),dimnames=liste.names)
                        
                        stress_45<-as.vector(apply(zwind,MARGIN=c(1,2),FUN=calcul_projection))
                        
                        select<-stress_45[id_inside]
                        
                        stress_mean[i-(min(year-1))]<-mean(select)
                        
                        #### SD
                        
                        vsd<-apply(vwind2,MARGIN=c(1,2),sd)
                        usd<-apply(uwind2,MARGIN=c(1,2),sd)
                        
                        row.names<-unlist(dimnames(vsd)[1])
                        column.names<-unlist(dimnames(vsd)[2])
                        axe.names<-c("U","v")
                        liste.names<-list(row.names,column.names,axe.names)
                        
                        zwind_sd<-array(c(usd ,vsd),dim=c(dim(vsd),2),dimnames=liste.names)
                        
                        stress_45_sd<-as.vector(apply(zwind_sd,MARGIN=c(1,2),FUN=calcul_projection))
                        
                        select_sd<-stress_45_sd[id_inside]
                        
                        stress_sd[i-(min(year)-1)]<-mean(select_sd)
                        print(paste("Mean and sd of",i,"are calculated"))

    }

      
      newList <- list("values" = data.frame(stress_mean,vwind_mean,uwind_mean,stress_sd,year),
                      "angle" = angle)
      return(newList)
      
}



###################### Not in the function #####
####data calculated with ####
#xpolygon<-c(5,10,14.5,11.5,3.5) ###x-axis
#ypolygon<-c(62,64,67.5,68,62)   ###y-axis
#months=c(4,8)
#year<-1948:2018
#angle=pi/4

#save(stress_sd, file = "stress_sd.RData")
#save(stress_mean, file = "stress_mean.RData")

