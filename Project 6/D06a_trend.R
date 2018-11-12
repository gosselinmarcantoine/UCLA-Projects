######################################################################################################
# D06a Models for trends
# By William Yu, UCLA Anderson Forecast
# 10/27/2018, updated 11/6/2018
##################################################################################################### 
install.packages("quantmod")  
install.packages("tseries") 
install.packages("fpp2") 
library(quantmod) 
library(tseries)
library(fpp2)

# Package "quantmod" can import data directly from various sources
# Import data directly from FRED (Federal Reserve Economic Data; https://research.stlouisfed.org/fred2/)
# Real GDP: https://fred.stlouisfed.org/series/GDPC1
# Reference: https://rpubs.com/DanielSLee/ImportingManagingFinancialData

?getSymbolLookup
# src: yahoo, google, FRED, oanda, csv, MySQL, RData
getSymbols("GDPC1", src="FRED")   # "getSymbols" is the function to download the data
plot(GDPC1)   
head(GDPC1)
plot(log(GDPC1))

gdp=data.frame(GDPC1)
trend=data.frame(c(1:length(GDPC1)))
trend2=trend^2
lrgdp=data.frame(log(GDPC1))
gdp.df=cbind(gdp,trend,trend2,lrgdp)
names(gdp.df)=c("rgdp","trend","trend2","lrgdp")
gdp.ts <-ts(gdp.df,frequency=4,start=c(1947,1))

# Model 1: Linear Trend Model
model01 <- lm(rgdp~trend,data=gdp.df)
summary(model01)  
fit01 <- predict(model01)
fit01.ts <-ts(fit01,frequency=4,start=c(1947,1))
ts.plot(GDPC1, fit01.ts, col=c("black","red"))

# Check residuals
plot(model01,1)

# Model 2 Quadratic Trend dModel
model02 <- lm(rgdp~trend+trend2,data=gdp.df)
summary(model02)  
fit02 <- predict(model02)
fit02.ts <-ts(fit02,frequency=4,start=c(1947,1))
ts.plot(GDPC1, fit02.ts, lty=c(1,2), lwd=c(1,2))
plot(model02,1)

# Model 3 Log-Linear Trend Model
model03 <- lm(lrgdp~trend,data=gdp.df)
summary(model03)
fit03 <- predict(model03)
fit03.ts <-ts(fit03,frequency=4,start=c(1947,1))
ts.plot(GDPC1, exp(fit03.ts), lty=c(1,5), lwd=c(1,3),col=c("black","blue"))
plot(model03,1)

##
## Start the sample from 1984Q1
##
?window
gdp84=window(gdp.ts, start=c(1984,1))
n=nrow(gdp84)
gdp.df2=data.frame(gdp84)
gdp.df2$trend=seq(1,n)
gdp.df2$trend2=(gdp.df2$trend)^2

# Model 1: Linear Trend Model
model01a <- lm(rgdp~trend,data=gdp.df2)
summary(model01a)  
fit01a <- predict(model01a)
fit01a.ts <-ts(fit01a,frequency=4,start=c(1984,1))
ts.plot(gdp84[,"rgdp"], fit01a.ts, col=c("black","red"))
plot(model01a,1)

# Model 2: Quadratic Trend dModel
model02a <- lm(rgdp~trend+trend2,data=gdp.df2)
summary(model02a)  
fit02a <- predict(model02a)
fit02a.ts <-ts(fit02a,frequency=4,start=c(1984,1))
ts.plot(gdp84[,"rgdp"], fit02a.ts, lty=c(1,2), lwd=c(1,2))
plot(model02a,1)

# Model 3: Log-Linear Trend Model
model03a <- lm(lrgdp~trend,data=gdp.df2)
summary(model03a)
fit03a <- predict(model03a)
fit03a.ts <-ts(fit03a,frequency=4,start=c(1984,1))
ts.plot(gdp84[,"rgdp"], exp(fit03a.ts), lty=c(1,5), lwd=c(1,3),col=c("black","blue"))
plot(model03a,1)

#
# Out-of-sample check or testset check
#
testq=12  # Using the final 12 quarters/3 years as testing period
m=nrow(gdp.df2)-12
gdp.df2b=gdp.df2[1:m,]
gdp.df2t=gdp.df2[(m+1):nrow(gdp.df2),]

# Model 1: Linear Trend Model
model01b <- lm(rgdp~trend,data=gdp.df2b)
summary(model01b)  
fit01b <- predict(model01b,newdata=gdp.df2t)
rmse1=sqrt(mean((fit01b-gdp.df2t[,"rgdp"])^2))

# Model 2: Quadratic Trend dModel
model02b <- lm(rgdp~trend+trend2,data=gdp.df2b)
summary(model02b)  
fit02b <- predict(model02b,newdata=gdp.df2t)
rmse2=sqrt(mean((fit02b-gdp.df2t[,"rgdp"])^2))

# Model 3: Log-Linear Trend Model
model03b <- lm(lrgdp~trend,data=gdp.df2b)
summary(model03b)
fit03b <- predict(model03b,newdata=gdp.df2t)
rmse3=sqrt(mean((exp(fit03b)-gdp.df2t[,"rgdp"])^2))

rmse1;rmse2;rmse3

fit01b.ts <-ts(fit01b,frequency=4,start=c(2015,2))
fit02b.ts <-ts(fit02b,frequency=4,start=c(2015,2))
fit03b.ts <-ts(fit03b,frequency=4,start=c(2015,2))
ts.plot(gdp84[-(1:m),"rgdp"], fit01b.ts, col=c("black","red"))
ts.plot(gdp84[-(1:m),"rgdp"], fit02b.ts, lty=c(1,2), lwd=c(1,2))
ts.plot(gdp84[-(1:m),"rgdp"], exp(fit03b.ts), lty=c(1,5), lwd=c(1,3),col=c("black","blue"))

##
## Using tslm function
##
h=10
model.01c = tslm(rgdp~trend, data=gdp.ts) # trend is a generic, function supplied argument
fcast.01c = forecast(model.01c, h=h) # h is the forcasting period for prediction

model.spline = tslm(rgdp~trend+I(trend^2)+I(trend^3), data=gdp.ts)
fcast.spl = forecast(model.spline,h=h)

autoplot(gdp.ts[,"rgdp"]) +
  autolayer(fitted(model.01c), series="Linear") +
  autolayer(fitted(model.spline), series="Cubic Spline") +
  autolayer(fcast.01c, series="Linear") +
  autolayer(fcast.spl, series="Cubic Spline") +
  xlab("Year") + ylab("Real GDP") +
  ggtitle("U.S. Real GDP and Forecasts") +
  guides(colour = guide_legend(title = ""))

# Natural Cubic Smoothing Splines
gdp.ts[,"rgdp"] %>% splinef(lambda=0) %>% autoplot()  
gdp.ts[,"rgdp"] %>% splinef(lambda=0) %>% checkresiduals()  
