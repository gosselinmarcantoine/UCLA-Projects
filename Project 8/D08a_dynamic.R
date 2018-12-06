###########################################################################################
# D08a Dynamic Models
# By William Yu, UCLA Anderson Forecast
# March 5, 2016, updated Nov 12, 2018
###########################################################################################
install.packages("dynlm")
install.packages("vars")
library(quantmod) 
library(tseries)
library(forecast)
library(readxl) 
library(fpp2)
library(dynlm) 
library(vars)

setwd("~/Documents/DataScience/UCLADatascience/Project 7")
svalley <- read_excel("W07a_Svalley.xlsx")
svalley.ts <- ts(svalley, frequency = 12, start = c(1990,1))
setwd("~/Documents/DataScience/UCLADatascience/Project 8")



fit03 <-lm(sce.yr~ nasd.yr, data=svalley.ts)  # nasdaq.yr:  YOY returns 
summary(fit03)
checkresiduals(fit03)

fit03a=auto.arima(svalley.ts[,"sce.yr"], xreg=svalley.ts[,"nasd.yr"])
summary(fit03a)
checkresiduals(fit03a)

# Keep in mind, in order to forecast y(sce.yr), you need to forecast x (nasd.yr) first
n=nrow(svalley.ts)
fcast=forecast(fit03a, xreg=rep(svalley.ts[n,"nasd.yr"],24))
autoplot(fcast)+xlab("Year")+ylab("Year-over-year Growth Rate")

# Determinstic trend --linear trend
getSymbols("GDPC1", src="FRED")  
gdp.ts <-ts(GDPC1,frequency=4,start=c(1947,1))
gdp84=window(gdp.ts, start=c(1984,1))
model01 = tslm(GDPC1~trend, data=gdp84) # linear model for time series
fcast01 = forecast(model01, h=10)
fcast01

# Determinstic trend with ARIMA errors
trend02=seq_along(gdp84)
model02 = auto.arima(gdp84, d=0,xreg=trend02)
summary(model02)
fcast02 = forecast(model02, xreg=cbind(trend=length(gdp84)+1:10))

# Stochastic trend
model03 = auto.arima(gdp84,d=1)
fcast03 = forecast(model03, h=10)

autoplot(gdp84) +
  autolayer(fcast03, series="Stochastic trend") +
  autolayer(fcast02, series="Determinstic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Real GDP") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")

# Lagged Predictor --------------------------------------------------------

# Lagged predictors. Test 0, 1, 2 or 3 lags.
head(insurance)
Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"],-1),
  AdLag2 = stats::lag(insurance[,"TV.advert"],-2),
  AdLag3 = stats::lag(insurance[,"TV.advert"],-3)) %>%
  head(NROW(insurance))

# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],stationary=TRUE) # This xreg makes the arima model for the error term.
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], stationary=TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], stationary=TRUE)

# Choose the optimal lag length for advertising based on the AICs
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

# Import the excel data into Data frame --> Best model: One lag (current month and last month)

bestfit <- auto.arima(insurance[,1], xreg=Advert[,1:2], stationary=TRUE)
summary(bestfit)

# Assuming the future monthly advertising are 8 units
fcast05 <- forecast(bestfit, h=20, xreg=cbind(AdLag0 = rep(8,20),
                           AdLag1 = c(Advert[40,1], rep(8,19))))
autoplot(fcast05) + ylab("Quotes") + ggtitle("Forecast quotes with future advertising set to 8")

##
## Dynamic Harmonic Regression
##
getSymbols("MRTSSM7221USN", src="FRED") # https://fred.stlouisfed.org/series/MRTSSM7221USN
eat=ts(MRTSSM7221USN, frequency=12, start=c(1992,1))
eat10<- window(eat, start=2010)

plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(eat10, xreg = fourier(eat10, K = i), seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit, xreg=fourier(eat10, K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) + ylab("") 
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]], plots[[4]],plots[[5]],plots[[6]], nrow=3)

##
## Dynamic OLS Model
##
getSymbols("WTISPLC", src="FRED")     # WTI Crude Oil Prices; 
                                      # https://fred.stlouisfed.org/series/WTISPLC#0
oilm=WTISPLC
head(oilm)
oil=apply.quarterly(oilm, mean, na.rm=T)  # Convert average monthly oil prices to quarterly prices
oil.ts=ts(oil,frequency=4, start=c(1947,1))
gdp=ts(GDPC1, frequency=4, start=c(1947,1))

oilg=diff(log(oil.ts))
yg=diff(log(gdp))

# A so-so model: Adj. R2=0.05, Residuals have some autocorrelction. Not a good model
model10a <- dynlm(yg ~ oilg+ L(oilg))   
summary(model10a)  
checkresiduals(model10a)  # Residuals have some autocorrelction. Not a good model 

# A better model: AR(1) model; Adj. R2=0.13
model10b <- dynlm(yg ~ L(yg))
summary(model10b) 
checkresiduals(model10b)

# A better model: Dynamic linear model; Adj. R2=0.17
model10c <- dynlm(yg ~ L(yg)+oilg+L(oilg))
summary(model10c) 
checkresiduals(model10c)

# Alternative way: yg1.ts <- lag(yg.ts,k=-1)

##
## VAR Models
##
getSymbols("SPCS10RSA", src="FRED")  # https://fred.stlouisfed.org/series/SPCS10RSA
cshome = SPCS10RSA
cshomer=diff(log(cshome))  # calculate growth rate, home price return
getSymbols("HOUST", src="FRED") # Housing Starts; https://fred.stlouisfed.org/series/HOUST
hs = HOUST
hsr=diff(log(hs))
?VAR
var1=cbind(cshomer, hsr)
var59=ts(var1,frequency=12,start=c(1959,1))
var87=window(var59,start=c(1987,2),end=c(2018,8))
plot(var87)
model.v1 <- VAR(var87, p=12, type="const") # p is the number of legs
summary(model.v1)
fcast.v <-predict(model.v1, n.ahead = 24, ci=0.95)
plot(fcast.v)
fcast.v

# type has four types: "const", "trend", "both", "none", type of deterministic regressors to include. 

# compared to ARIMA models
model.hsr=auto.arima(hsr)        # ARIMA (2,1,2)
model.homer=auto.arima(cshomer)     # ARIMA (3,0,4)
summary(model.hsr)   #ARIMA(2,0,0) ar1=-0.36, ar2=-0.11
summary(model.homer) #ARIMA(2,0,0) ar1=0.93, ar2=0.04
for_hsr <- forecast(model.hsr,24) 
for_homer <- forecast(model.homer,24)

par(mfrow=c(2,1))
plot(for_homer)
plot(for_hsr)
par(mfrow=c(1,1))

