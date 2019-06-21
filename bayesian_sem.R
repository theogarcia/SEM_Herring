library(piecewiseSEM)
data("keeley")
library(brms)

rich_mod <- bf(rich ~ firesev + cover)
cover_mod <- bf(cover ~ firesev)

k_fit_brms <- brm(rich_mod +
                    cover_mod + 
                    set_rescor(FALSE), 
                  data=keeley,
                  cores=4, chains = 2)
plot(k_fit_brms)

##################################################################################
library(Hmisc) #Lag function
#################################################################
load("C:/Users/moi/Desktop/Stage/Script/SEM_Herring/SEM_Herring/data/data.RData")

#################################### Lag variables #####################################################
##### Lag -1 #####

data$Cc_H<-Lag(data$Cc_H,-1)
data$Cod<-Lag(data$Cod,-1)
data$Cap_cod_rat<-Lag(data$Cap_cod_rat,-1)

##### Lag -2 ####

data$Puff<-Lag(data$Puff,-2)
data$Mack<-Lag(data$Mack,-2)
data$H_0<-Lag(data$H_0,-2)
data$T_Ssum<-Lag(data$T_Ssum,-2)
data$Cal_fin<-Lag(data$Cal_fin,-2)
data$ACW_stress<-Lag(data$ACW_stress,-2)
data$ACW_stab<-Lag(data$ACW_stab,-2)
data$ACW_stab<-Lag(data$ACW_stab,-2)
data$mean_hatch<-Lag(data$mean_hatch,-2)
data$Sal_I2<-Lag(data$Sal_I2,-2)
data$Sal_I1<-Lag(data$Sal_I1,-2)
data$SSB_H<-Lag(data$SSB_H,-2)
data$Age_index1<-Lag(data$Age_index1,-2)
data$Age_index2<-Lag(data$Age_index2,-2)
attach(data)

#################################### Quantile regressions ###############################################
herr <- bf(H_R2 ~ H_0 + Cc_H, quantile = 0.9)
cod <- bf(Cc_H ~ Cap_cod_rat, quantile = 0.9)

herr_fit_brms <- brm(herr +
                    cod+ 
                    set_rescor(FALSE), 
                  data=data,family = asym_laplace(),
                  cores=4, chains = 2)


plot(herr_fit_brms)


summary(herr_fit_brms)

plot(marginal_effects(herr_fit_brms), points = TRUE)


herr0 <- bf(H_0 ~ Mack + Cal_fin)
mack <- bf(Mack ~ T_Ssum)
zoo<-bf(Cal_fin~ T_Ssum)

herr0_fit_brms <- brm(herr0 +
                       mack+
                       zoo+
                       set_rescor(FALSE), 
                     data=data,
                     cores=4, chains = 2)


plot(herr0_fit_brms)


summary(herr0_fit_brms)


