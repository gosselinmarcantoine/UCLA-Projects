######################################################################################################
# D07a Time Series Model
# By William Yu, UCLA Anderson Forecast
# 11/1/2018
###################################################################################################### 
library(quantmod) 
library(tseries)
library(fpp2)

getSymbols("DSPIC96", src="FRED")     # Real Disposable Personal Income;        https://fred.stlouisfed.org/series/DSPIC96
getSymbols("PCEC96", src="FRED")      # Real Personal Consumption Expenditures; https://fred.stlouisfed.org/series/PCEC96
getSymbols("RETAILSMNSA", src="FRED") # Retail Sales Non-Seasonal Adjustment;   https://fred.stlouisfed.org/series/RETAILSMNSA

income=DSPIC96  
consumption=PCEC96  
retailn=RETAILSMNSA

incomeg=diff(log(income)) # diff(log()) is looking for the growth rate of x -- in this case income
consumeg=diff(log(consumption))
retailg=diff(log(retailn))

incomeg12=rollapply(incomeg,12,sum, align="right")
consumeg12=rollapply(consumeg,12,sum, align="right")

data1=cbind(income, consumption, incomeg,consumeg, incomeg12,consumeg12)
names(data1)=c("income","consumption","incomeg","consumeg", "incomeg12","consumeg12")

fit01=lm(consumption~income, data=data1) # Looks great! (but isn't)
summary(fit01)

fit02=lm(consumeg~incomeg, data=data1) # Doesn't seem good anymore...
summary(fit02)

fit03=lm(consumeg12~incomeg12, data=data1)
summary(fit03) # year aggregate explains that consumption lags behind income by a little which throws fit02 (monthly growth rate) off. 

checkresiduals(fit01) # Autocorrelation (ACF) here is ACF of the residuals. You want insignificant ACF.
checkresiduals(fit02)
checkresiduals(fit03) # ACF here makes sense because the rolling sum overlap (we did that with rollapply)

CV(fit01)
CV(fit02)
CV(fit03)

##
## ACF (Autocorrelation Function)
##
ggAcf(income, lag=48) # Trend
ggAcf(incomeg, lag=48) # lag 2 can predict 
ggAcf(incomeg12,lag=48) # expected because of overlap
ggAcf(retailg, lag=48) # Seasonal growth rate

noise=ts(rnorm(50))
autoplot(noise)
ggAcf(noise) # What random noise should look like

##
## Autocorrelation Tests
##
# lag=10 for non-seasonal data and lag=2m for seasonal data where m is the period of seasoanlity or T/5 (whoever is smaller)
# When p-value < 0.05, reject the null: the series has no-autocorrelation. In other words, the series has autocorrelation.
res1=residuals(fit01)
plot(res1, type="l")
ggAcf(res1)
# Box-Pierce Test
Box.test(res1, lag=10, fitdf=0)  # res1 is autocorrelated
Box.test(noise, lag=10, fitdf=0) # noise is not autocorrelated

# Ljung-Box Test
Box.test(res1, lag=10, fitdf=0)  # res1 is autocorrelated
Box.test(noise, lag=10, fitdf=0) # noise is not autocorrelated

## Some Simple Forecast Methods

##
## Average Method: all forecasts are equal to the average of the historical data.
##
meanf(incomeg12,8)

##
## Naive Method: all forecasts to be the value of the last observation. 
##
naive(income,12)

## Random Walk Forecast: all forecasts to be the value of the last observation.
rwf(income, 12)

## Random Walk with Drift Forecast: Forecasts to increase or decrease over ime, 
## where the amount of change over time (called the drift) is set to be the average change seen in the historical data.
rwf(income, 12, drift=T)

## Forecast function/black box
forecast(income, 12)

##
## Calendar Adjustments
##
retail.ts <-ts(retailn,frequency=12,start=c(1992,1)) 
data2=cbind(Monthly=retail.ts, DailyAverage=retail.ts/monthdays(retail.ts))
autoplot(data2, facet=T) + xlab("Years") + ylab("Retail Sales") 

##
## Data transformation
##

# Box-Cox transformations

# Different method similar to taking the log. Fixes problem of variation affected by inflation.

lambda=seq(-2,2,by=0.1)
x=4
w=(x^lambda-1)/lambda # this is the box-Cox equation
plot(lambda,w)

lambda=-1
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

lambda=1
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

lambda=2
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

getSymbols("IPB50001N", src="FRED") # Industrial Production;   https://fred.stlouisfed.org/series/IPB50001N
ip=IPB50001N
autoplot(ip)

lam=BoxCox.lambda(ip) # finds the best lambda value to make model linear/ remove inflation related problems.
lam
autoplot(BoxCox(ip,lam)) 
autoplot(log(ip))

##
## Cross Validation
##
e1=tsCV(income, rwf, drift=T,h=1)
sqrt(mean(e1^2, na.rm=T))
e2=tsCV(income, rwf, drift=F,h=1)
sqrt(mean(e2^2, na.rm=T))
e3=tsCV(income, forecast, h=1)
sqrt(mean(e3^2, na.rm=T))

income %>% tsCV(rwf, drift=T,h=1) -> e
e^2 %>% mean(na.rm=T) %>% sqrt()



