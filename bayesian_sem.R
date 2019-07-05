library(piecewiseSEM)
data("keeley")
library(brms)
library(dplyr) 
library(quantreg)

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

#################################### Bayesian SEM ###############################################
##########  Linear model and quantile linear model 

##### Mackerel
fitmack<-brm(bf(H_0~Mack),data=data)

summary(fitmack)
plot(fitmack)
plot(marginal_effects(fitmack), points = TRUE)

fitmack_q<-brm(bf(H_0~Mack, quantile=0.9),data=data, family=asym_laplace)

summary(fitmack_q)
plot(fitmack_q)
#plot(marginal_effects(fitmack_q), points = TRUE)


##### Coastal current
fitCC<-brm(bf(H_0~Sal_I1),data=data)

summary(fitCC)
plot(fitCC)
plot(marginal_effects(fitCC), points = TRUE)

fitCC_q<-brm(bf(H_0~Sal_I1, quantile=0.9),data=data, family=asym_laplace)

summary(fitCC_q)
plot(fitCC_q)
#plot(marginal_effects(fitCC_q), points = TRUE)

##### Hatch timing
fitHT<-brm(bf(H_0~mean_hatch),data=data)

summary(fitHT)
plot(fitHT)
plot(marginal_effects(fitHT), points = TRUE)

fitHT_q<-brm(bf(H_0~mean_hatch, quantile=0.9),data=data, family=asym_laplace)

summary(fitHT_q)
plot(fitHT_q)
#plot(marginal_effects(fitHT_q), points = TRUE)



##### Spawning stock
fitSSB<-brm(bf(H_0~SSB_H),data=data)

summary(fitSSB)
plot(fitSSB)
plot(marginal_effects(fitSSB), points = TRUE)

fitSSB_q<-brm(bf(H_0~SSB_H, quantile=0.9),data=data, family=asym_laplace)

summary(fitSSB_q)
plot(fitSSB_q)
#plot(marginal_effects(fitHT_q), points = TRUE)

##### Macth Zoopk
fitCF<-brm(bf(H_0~Cal_fin),data=data)

summary(fitCF)
plot(fitCF)
plot(marginal_effects(fitCF), points = TRUE)

fitCF_q<-brm(bf(H_0~Cal_fin, quantile=0.9),data=data, family=asym_laplace)

summary(fitCF_q)
plot(fitCF_q)
#plot(marginal_effects(fitCF_q), points = TRUE)

##### Herring 0
fitH0<-brm(bf(H_0~Cal_fin+Sal_I1+Mack+SSB_H+mean_hatch),data=data)

summary(fitH0)
plot(fitH0)
plot(marginal_effects(fitH0), points = TRUE)

fitH0_q<-brm(bf(H_0~Cal_fin+Sal_I1+Mack+SSB_H+mean_hatch, quantile=0.9),data=data, family=asym_laplace, iter = 2000, chains = 4)

summary(fitH0_q)
plot(fitH0_q)
#plot(marginal_effects(fitH0_q), points = TRUE)



##### For non linear model-> modify prior
prior1 <- prior(normal(-1,1), nlpar = "a") +
  prior(normal(1,1), nlpar = "b")

fit1 <- brm(bf(H_R2 ~ a * Cc_H^2 + b * Cc_H  , a + b ~ 1, nl = TRUE),
            data = data, prior = prior1)



prior <- prior(normal(10000, 100), class = Intercept)+
          prior(normal(0, 10), class = b)+
          prior(cauchy(0, 1), class = sigma)

fit<-brm(bf(H_R2 ~ 1 + Cc_H + I(Cc_H^2)),
    data = data, prior = prior,family = gaussian,)



summary(fit)
plot(fit)
plot(marginal_effects(fit), points = TRUE)

#########FIT QUADRATIC FUNCTION AND QUANTILE QUADRATIC FUNCTION ##############
# start with just a simple quadratic approach
# y ~ a * x^2 + b * x  + c

my.equation <-H_R2 ~ a * Cc_H^2 + b * Cc_H  + c

# fit the equation to the data via "non-linear least squares"
# choose some good starting values for parameter estimation
nls.fit <- nls(my.equation,
               data = data,
               start = list(a = 2, b = 3, c = 1))

# look at the result
summary(nls.fit)


# create a dummy range of that we use to predict speed from our fitted model
predict_range <- data.frame(Cc_H = seq(min(Cc_H, na.rm=T), max(Cc_H,na.rm=T), length = 250))

# calculate for each x-range value the corresponding y-range
my.line <- within(predict_range, y <- predict(nls.fit, newdata = predict_range))

# add the line to the existing graph
# This line represents the "mean" fit, no quantile regression involved
# plot data
plot(Cc_H,H_R2, xlab="Cod predation", ylab="Herring at age 2")
lines( y~ Cc_H, data = my.line, col = "red")


my.rq <- nlrq(my.equation,
              data = data,
              start = list(a = 2, b = 3, c = 20000),
              tau = .9)
summary(my.rq)



my.line9 <- within(predict_range, 
                    y <- predict(my.rq, 
                                 newdata = predict_range))
plot(Cc_H,H_R2, xlab="Cod predation", ylab="Herring at age 2")
lines(y ~ Cc_H, data = my.line, col = "red")
lines(y ~ Cc_H, data = my.line9, col = "blue")


