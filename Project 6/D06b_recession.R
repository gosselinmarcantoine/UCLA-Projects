######################################################################################################
# D06b Moving average and sum methods; Stock market decline and recession
# By William Yu, UCLA Anderson Forecast
# 10/27/2018
##################################################################################################### 
install.packages("tseries") 
install.packages("fpp2") 
library(tseries)
library(fpp2)
library(zoo)

# Set your working directory in R
setwd("~/Documents/DataScience/UCLADatascience/Project 6")

# Import the CSV Date file into data frame. 
shiller <- read.csv("W06a_shiller.csv") 
plot(shiller[,"Price"], type="l")
head(shiller)

shiller$Recession=ifelse(shiller$Recession=="N",0,1)

sprice = ts(shiller$Price,frequency=12,start=c(1871,1)) # ts converts data table into a timeseries 
recession =ts(shiller$Recession,frequency=12,start=c(1871,1))

# frequency: annaul=1; quarterly=4, monthly=12; weekly=52
# daily will be more complex
plot(sprice)

?rollapply # Take avg of last 12 months for x month of y year.
sp12ma=rollapply(sprice,12,mean, align="right") # allign right means we associate the avg to the last month not the first month (left) or 6th month (center).
sp120ma=rollapply(sprice,120,mean, align="right")
price.ma=cbind(sprice,sp12ma,sp120ma)
price.1980=window(price.ma, start=c(1980,1))
autoplot(price.1980)

lprice=log(sprice)
plot(lprice,main="S&P 500 Log Real Stock Prices",xlab="Year",ylab="Log Prices")
autoplot(lprice,main="S&P 500 Log Real Stock Prices",xlab="Year",ylab="Log Prices")
return=diff(log(sprice))
head(return)
summary(12*return)

return6s=rollapply(return,6,sum, align="right")
return9s=rollapply(return,9,sum, align="right")
return12s=rollapply(return,12,sum, align="right")
head(return6s)
head(return9s)
head(return12s)

shiller.all=cbind(sprice,recession,lprice,return,return6s,return9s,return12s)
head(shiller.all)
shiller.47=window(shiller.all, start=c(1947,1))
head(shiller.47)

par(mfrow=c(2,1))
plot(shiller.47[,"return6s"])
abline(h=-0.1, col="red")
plot(shiller.47[,"recession"])
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(shiller.47[,"return6s"])
abline(h=-0.2, col="red")
plot(shiller.47[,"recession"])
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(shiller.47[,"return12s"])
abline(h=-0.2, col="red")
plot(shiller.47[,"recession"])
par(mfrow=c(1,1))

autoplot(shiller.47[,c("return12s","recession")])


