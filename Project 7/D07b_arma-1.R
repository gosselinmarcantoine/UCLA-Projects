###########################################################################################
# D07b Models for Cycle, ARMA
# By William Yu, UCLA Anderson Forecast
# Feb 19, 2016, updated Nov 7, 2018
###########################################################################################
library(forecast)
library(quantmod) 
library(tseries)

getSymbols("NASDAQCOM", src="FRED")   # https://fred.stlouisfed.org/series/NASDAQCOM
nasd=NASDAQCOM  # Daily data
nasdm=apply.monthly(nasd, mean, na.rm=T)   # Nasdaq Index: convert it to monthly data
nasdmr=diff(log(nasdm))   # calculate growth rate, stock price return
random=ts(rnorm(574))

# Cas Shiller Home Price Index, US-10 City average
getSymbols("SPCS10RSA", src="FRED")  # https://fred.stlouisfed.org/series/SPCS10RSA
cshome = SPCS10RSA
cshomer=diff(log(cshome))  # calculate growth rate, home price return

par(mfrow=c(2,2))
plot(nasdm)
plot(nasdmr)
plot(random)
plot(cshomer)
par(mfrow=c(1,1))

# Autocorrelation Function
par(mfrow=c(2,2))
acf(nasdm,lag.max=48)
acf(random,lag.max=48)
acf(nasdmr,lag.max=48, na.action=na.pass)
acf(cshomer,lag.max=48, na.action=na.pass) 

ggAcf(nasdm,lag=48)
ggAcf(random,lag=48)
ggAcf(nasdmr,lag=48)
ggAcf(cshomer,lag=48)

# Partial Autocorrection Function
pacf(nasdm,lag.max=48)
pacf(random,lag.max=48)
pacf(nasdmr,lag.max=48,na.action=na.pass)
pacf(cshomer,lag.max=48,na.action=na.pass)
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

# Revisit the decomposition function for Retail Sales (NSA) in R03_decompose

getSymbols("RETAILSMNSA", src="FRED")         # https://fred.stlouisfed.org/series/RETAILSMNSA
retailn <-ts(RETAILSMNSA,frequency=12,start=c(1992,1))  # Non-seasonally adjusted data

# Decompostion of time series data
retail.comps = decompose(retailn)
plot(retail.comps)
plot(retail.comps$random)
retail.random=as.data.frame(retail.comps$random)

acf(retail.random,lag.max=48,na.action = na.pass)
pacf(retail.random,lag.max=48, na.action = na.pass)



