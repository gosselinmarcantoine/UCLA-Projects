###########################################################################################
# Models for Cycles
# By William Yu, Feb 19, 2016, Revised May 14, 2017
###########################################################################################

# Arma is especially good for short term forcasting. Example: Two weeks is short term but 2 years is long term. More art than science here. ~ things change over time and affect forcast power.


install.packages("forecast")
install.packages("quantmod") 
install.packages("tseries") 
library(forecast)
library(quantmod) 
library(tseries)

# First Change the working directory where the data are for your own laptop!!
setwd("~/Documents/DataScience/UCLADatascience/Project 7")

# Import the data into Data frame
nasdaq.df <- read.csv("W7_Nasdaq.csv")
nasdaq = nasdaq.df[,"nasdaq"]    # https://fred.stlouisfed.org/series/NASDAQCOM
nasdaqr = nasdaq.df[,"nasdaqr"]
random = nasdaq.df[,"random"]
cshomer = nasdaq.df[,"cshomer"]  # https://fred.stlouisfed.org/series/CSUSHPISA

par(mfrow=c(2,2))

plot(nasdaq, type="l")
plot(nasdaqr,type="l" )
plot(random, type="l")
plot(cshomer, type="l" )
par(mfrow=c(1,1))

# Autocorrelation Function
acf(nasdaq,lag.max=48)
acf(random,lag.max=48)
acf(nasdaqr,lag.max=48)
acf(cshomer,lag.max=48) 

# Partial Autocorrection Function
pacf(nasdaq,lag.max=48)
pacf(random,lag.max=48)
pacf(nasdaqr,lag.max=48)
pacf(cshomer,lag.max=48)

par(mfrow=c(1,1))

##################################################################################################
# Model 6: ARMA Models
##################################################################################################
model6a <- arima(cshomer, order=c(1,0,0)) # AR(1) 
model6b <- arima(cshomer, order=c(0,0,1)) # MA(1) 
model6c <- arima(cshomer, order=c(1,0,1)) # ARMA(1,1) 
model6d <- arima(cshomer, order=c(2,0,0)) # AR(2)
model6e <- arima(cshomer, order=c(0,0,2)) # MA(2)
model6f <- arima(cshomer, order=c(2,0,1)) # ARMA(2,1)
model6g <- arima(cshomer, order=c(1,0,2)) # ARMA(1,2)
model6h <- arima(cshomer, order=c(2,0,2)) # ARMA(2,2)

summary(model6a)
summary(model6d)
summary(model6h)

# Forecast error evaluation with Root Mean Square Error (RMSE): 
accuracy(model6a)
accuracy(model6b)
accuracy(model6c)
accuracy(model6d)
accuracy(model6e)
accuracy(model6f)   
accuracy(model6g)
accuracy(model6h)   # the best one with the lowest RMSE

for6h <- forecast(model6h,24)  # forecast the next 24 periods
for6h
plot(for6h)
abline(h=0)
grid()

###########################################################################################################
# Revisit the decomposition function for Retail Sales (NSA) in R03_decompose
###########################################################################################################
getSymbols("RETAILSMNSA", src="FRED")         # https://research.stlouisfed.org/fred2/series/RETAILSMNSA

retailn <-ts(RETAILSMNSA,frequency=12,start=c(1992,1))  # Non-seasonally adjusted data

# Decompostion of time series data
retail.comps = decompose(retailn)
plot(retail.comps)
plot(retail.comps$random)
retail.random=as.data.frame(retail.comps$random)

acf(retail.random,lag.max=48)
acf(retail.random,lag.max=48, na.action = na.pass)
pacf(retail.random,lag.max=48, na.action = na.pass)

###################################################################################################
# How to explain Santa Clara County Jog Growth Pattern?
###################################################################################################
nasdaq.ts=ts(nasdaq.df[180:503,], frequency=12,start=c(1990,1))
par(mfrow=c(1,2))
plot(nasdaq.ts[,"scce"], ylab="Santa Clara Jobs")
plot(nasdaq.ts[,"nasdaq"], ylab="NASDAQ Index")
par(mfrow=c(1,1))

# Test to see if NASDAQ Index could predict job growth in Sana Clara COunty (Silicon Valley)
fit00 <-lm(scce~ nasdaq,data=nasdaq.df)         # Non-stationary regression
summary(fit00)                                  # There is a risk of spurious regression: Not recommend!!
fit01 <-lm(scce.mr~ nasdaqr,data=nasdaq.df)     # sccr.mr: Santa Clara County Monthly Job growth
summary(fit01)
fit02 <-lm(scce.yr~ nasdaq.yr,data=nasdaq.df)   # scce.yr and nasdaq.yr: Year-over-year returns
summary(fit02)
fit03 <-lm(scce.yr~ nasdaq.yr6,data=nasdaq.df)  # nasdaq.yr6:  YOY returns at 6 month lag
summary(fit03)
fit04 <-lm(scce.yr~ nasdaq.yr12,data=nasdaq.df) # nasdaq.yr12: YOY returns at 12 month lag
summary(fit04)

###########################################################################################################
# Model 7: ARMA Seasonal Model
###########################################################################################################
getSymbols("HOUSTNSA", src="FRED") # Housing Starts; https://research.stlouisfed.org/fred2/series/HOUSTNSA
plot(HOUSTNSA)

# Import the xlsx data into Data frame
hs <- as.data.frame(HOUSTNSA)

acf(hs,lag.max=48)  # Autocorrelation
pacf(hs,lag.max=48) # Partial Autocorrelation

# Model 7: Seasonal ARMA Models
model7a <- arima(hs, order=c(2,0,2))                    #ARMA(2,2) without seasonal factor 
model7b <- arima(hs, order=c(2,0,2), seasonal=c(12,0,0)) #ARMA(2,2) & seasonal factor in AR term
model7c <- arima(hs, order=c(2,0,2), seasonal=c(0,0,12)) #ARMA(2,2) & seasonal factor in MA term
model7d <- arima(hs, order=c(2,0,2), seasonal=c(12,0,12)) #ARMA(2,2) & seasonal factor in ARMA terms
model7e <- arima(hs, order=c(0,0,0), seasonal=c(12,0,12)) #seasonal model without cycle model

# Forecast error evaluation with Root Mean Square Error (RMSE): 
accuracy(model7a)
accuracy(model7b)
accuracy(model7c)
accuracy(model7d)   # the best model with the lowest RMSE
accuracy(model7e)

# Check autocorrelation of residuals
acf(residuals(model7d),lag.max=48)  # Autocorrelation
pacf(residuals(model7d),lag.max=48) # Partial Autocorrelation

for7d <- forecast(model7d,48)  # Forecast seasonally adjusted series for the next 48 periods
for7d
plot(for7d)


