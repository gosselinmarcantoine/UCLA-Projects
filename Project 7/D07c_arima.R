###########################################################################################
# D07c ARIMA Models
# By William Yu, UCLA Anderson Forecast
# Feb 19, 2016, updated Nov 8, 2018
###########################################################################################
library(readxl)
library(forecast)
library(quantmod) 
library(tseries)

setwd("~/Documents/DataScience/UCLADatascience/Project 7")

###################################################################################################
# How to explain Santa Clara County Jog Growth Pattern?
###################################################################################################
svalley <- read_excel("W07a_Svalley.xlsx")  
svalley.ts=ts(svalley, frequency=12, start=c(1990,1))

par(mfrow=c(2,1))
plot(svalley.ts[,"sce"], ylab="Santa Clara Jobs")
plot(svalley.ts[,"nasd"], ylab="NASDAQ Index")
par(mfrow=c(1,1))

sce.comps = decompose(svalley.ts[,"sce"])
sce1=svalley.ts[,"sce"]-sce.comps$seasonal

# Seasonal adjustment: Some probelm in the 1990s, in which there was no seasonality in the orginal series
par(mfrow=c(2,1))
plot(sce1, ylab="Santa Clara Jobs")
plot(svalley.ts[,"nasd"], ylab="NASDAQ Index")
par(mfrow=c(1,1))

# Test to see if NASDAQ Index could predict job growth in Sana Clara COunty (Silicon Valley)
fit01 <-lm(sce1~ nasd, data=svalley.ts)         # Non-stationary regression
summary(fit01)                                  # There is a risk of spurious regression: Not recommend!!
fit02 <-lm(sce.mr~ nasdr, data=svalley.ts)     # sccr.mr: Santa Clara County Monthly Job growth
summary(fit02)
fit03 <-lm(sce.yr~ nasd.yr, data= svalley.ts)  # scce.yr and nasdaq.yr: Year-over-year returns
summary(fit03)
fit04 <-lm(sce.yr~ nasd.yr6, data=svalley.ts)  # nasdaq.yr6:  YOY returns at 6 month lag
summary(fit04)
fit05 <-lm(sce.yr~ nasd.yr12,data=svalley.ts)  # nasdaq.yr12: YOY returns at 12 month lag
summary(fit05)

checkresiduals(fit04)

###########################################################################################################
# Model 7: ARMA Seasonal Model
###########################################################################################################
getSymbols("HOUSTNSA", src="FRED") # Housing Starts; https://fred.stlouisfed.org/series/HOUSTNSA
plot(HOUSTNSA)

# Import the xlsx data into Data frame
hs <- as.data.frame(HOUSTNSA)
hs <- HOUSTNSA

ggAcf(hs,lag=48)  # Autocorrelation
ggPacf(hs,lag=48) # Partial Autocorrelation

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
ggAcf(residuals(model7d),lag=48)  # Autocorrelation
ggPacf(residuals(model7d),lag=48) # Partial Autocorrelation
checkresiduals(model7d)

for7d <- forecast(model7d,48)  # Forecast seasonally adjusted series for the next 48 periods
for7d
plot(for7d)

# Import the xlsx data into Data frame
getSymbols("SP500", src="FRED") # S&P 500; https://fred.stlouisfed.org/series/SP500
getSymbols("DCOILWTICO", src="FRED") # WTI Crude Oil Prices; https://fred.stlouisfed.org/series/DCOILWTICO
getSymbols("DEXUSEU", src="FRED") # US/Euro Exchange Rate; https://fred.stlouisfed.org/series/DEXUSEU
getSymbols("PAYEMS", src="FRED") # US Payroll Employment; https://fred.stlouisfed.org/series/PAYEMS
getSymbols("DGS10", src="FRED") # 10-year Treasury Rate; https://fred.stlouisfed.org/series/DGS10
getSymbols("CPIAUCSL", src="FRED") # Consumer Price Index; https://fred.stlouisfed.org/series/CPIAUCSL
getSymbols("TOTALSA", src="FRED") # Total Vehicle Sales; https://fred.stlouisfed.org/series/TOTALSA
getSymbols("POPTHM", src="FRED") # US Population; https://fred.stlouisfed.org/series/POPTHM
getSymbols("CSUSHPISA", src="FRED") # US National Home Price; https://fred.stlouisfed.org/series/CSUSHPISA

sp500 = SP500
oil = DCOILWTICO
useu = DEXUSEU
payroll = PAYEMS
tr10 = DGS10
cpi = CPIAUCSL
autos = TOTALSA
pop = POPTHM
homeprice = CSUSHPISA

par(mfrow=c(3,3))
ts.plot(sp500)
ts.plot(oil)
ts.plot(useu)
ts.plot(payroll)
ts.plot(tr10)
ts.plot(cpi)
ts.plot(autos)
ts.plot(pop)
ts.plot(homeprice)
par(mfrow=c(1,1))

##
## Unit Root Test: Null Hypothesis - the seires has  
##
install.packages("urca")
library(urca)




# Model 8: ARIMA Models
auto.arima(sp500)     #ARIMA(1,1,0)
auto.arima(oil)       #ARIMA(2,1,2)
auto.arima(useu)      #ARIMA(0,1,0)
auto.arima(payroll)   #ARIMA(2,1,2)
auto.arima(tr10)      #ARIMA(2,1,4)
auto.arima(cpi)       #ARIMA(3,2,1)
auto.arima(autos)     #ARIMA(2,1,1)
auto.arima(pop)       #ARIMA(5,2,1)
auto.arima(homeprice) #ARIMA(2,1,1)

model8_sp500 <- arima(sp500, order=c(1,1,0))                    
model8_oil <- arima(oil, order=c(2,1,2)) 
model8_useu <- arima(useu, order=c(0,1,0))                    
model8_payroll <- arima(payroll, order=c(2,1,2)) 
model8_tr10 <- arima(tr10, order=c(2,1,4))                    
model8_cpi <- arima(cpi, order=c(3,2,1)) 
model8_autos <- arima(autos, order=c(2,1,1))
model8_pop <- arima(pop, order=c(5,2,1))
model8_homeprice <- arima(homeprice, order=c(2,1,1))                    

for8_sp500 <- forecast(model8_sp500,180)   # forecast the next 180 periods
for8_oil <- forecast(model8_oil,180)
for8_useu <- forecast(model8_useu,180)
for8_payroll <- forecast(model8_payroll,180)
for8_tr10 <- forecast(model8_tr10,180)
for8_cpi <- forecast(model8_cpi,180)
for8_autos <- forecast(model8_autos,180)
for8_pop <- forecast(model8_pop,180)
for8_homeprice <- forecast(model8_homeprice,180)

par(mfrow=c(3,3))
plot(for8_sp500)
plot(for8_oil)
plot(for8_useu)
plot(for8_payroll)
plot(for8_tr10)
plot(for8_cpi)
plot(for8_autos)
plot(for8_pop)
plot(for8_homeprice)
par(mfrow=c(1,1))

for8_autos


