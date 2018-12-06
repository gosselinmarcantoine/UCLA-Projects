#####################################################################################################
# D09a for analysis how ads spending impacts transactions 
# By William Yu, UCLA Anderson Forecast
# created on Oct 9, 2017, updated Nov 20, 2018
##################################################################################################### 
library(tseries)
library(fpp2)
# Import the data
# Set your working directory in R
setwd("~/Documents/DataScience/UCLADatascience/Project 9")

# Import the CSV Date file into data frame. 
tv.data = read.csv("W09a_tvlift.csv") 

# Variables description
# trans: Transactions
# sb: Brand-paid Search
# sb1: Brand-paid Search Lag Day 1
# sbm3: Brand-paid Search moving average 3 days
# snb: Non-Brand-paid Search
# snb1: Non-Brand-paid Search Lag Day 1
# snbm3: Non-Brand-paid Search moving average 3 days
# tv: TV ad
# tv1: TV ad Lag Day 1
# tvm3: Tv ad moving average 3 days

par(mfrow=c(2,2))
plot(tv.data[,"trans"], type="l",main="Sales")
plot(tv.data[,"sb"], type="l",main="Brand-paid search AD spending")
plot(tv.data[,"snb"], type="l",main="Non-brand-paid search Ad spending")
plot(tv.data[,"tv"], type="l",main="TV Ad spending")
par(mfrow=c(1,1))

# models
fit01 <- lm(trans ~ sb+snb+tv, data=tv.data)  
ggAcf(residuals(fit01))
# tv is neither statistically or economic significant.
# sb and nsb are both statistically significant; sb is more economic significant while nsb is more statistically significant.

fit02 <- lm(trans ~ sb+sb1+snb+snb1+tv+tv1, data=tv.data)
ggAcf(residuals(fit02))
# Most lags are not stat significant, meaning that search and ad only have impact on the same day, no lasting effect.
# R squared doesn't increase 

fit03 <- lm(trans ~ sb+sb1+sb2+snb+snb1+snb2+tv+tv1+tv2, data=tv.data)
ggAcf(residuals(fit03))
fit04 <- lm(trans ~ trans1+sb+snb+tv, data=tv.data)
ggAcf(residuals(fit04))
fit05 <- lm(trans ~ sbm3+snbm3+tvm3, data=tv.data)
ggAcf(residuals(fit05))

# Only TV ads
fit06 <- lm(trans ~ tv, data=tv.data)
ggAcf(residuals(fit06))
# R squared is 0.29
fit07 <- lm(trans ~ tvm3, data=tv.data)
ggAcf(residuals(fit07))
# R squared is 0.26, so this is an inferior model

# Variables interactions
fit08 <- lm(trans ~ sb+snb+tv+sb*tv+snb*tv, data=tv.data)   
ggAcf(residuals(fit08))
# Tv still doesn't help

summary(fit01)
summary(fit02)
summary(fit03)
summary(fit04)
summary(fit05)
summary(fit06)
summary(fit07)
summary(fit08)

#
# Add Weekday dummy
#
fit11 <- lm(trans ~ sb+snb+tv+w1+w2+w4+w5+w6+w7, data=tv.data)    
summary(fit11)
ggAcf(residuals(fit11))

fit12 <- lm(trans ~ trans1+sb+snb+tv+w1+w2+w4+w5+w6+w7, data=tv.data)    
summary(fit12)
ggAcf(residuals(fit12))

fit13 <- lm(trans ~ trans1+sb+snb+tv+tv1, data=tv.data)    
summary(fit13)
ggAcf(residuals(fit13))

fit14 <- lm(snb ~ tv, data=tv.data1)    
summary(fit14)
ggAcf(residuals(fit14))

fit15 <- lm(sb ~ tv, data=tv.data1)    
summary(fit15)
ggAcf(residuals(fit15))

# transactions are higher on Monday, Friday, Saturday, and Sunday
#
# For Sample Period of 11/10/2016 to 3/3/2017 when TV ad is active
# 

tv.data1 <- subset(tv.data[224:337,]) 
fit21 <- lm(trans ~ sb+snb+tv, data=tv.data1)  
summary(fit21)
ggAcf(residuals(fit21))

fit22 <- lm(trans ~ snb+tv, data=tv.data1)  
summary(fit22)
ggAcf(residuals(fit22))

fit23 <- lm(trans ~ sb+snb, data=tv.data1)  
summary(fit23)
ggAcf(residuals(fit23))

fit24 <- lm(trans ~ trans1+sb+snb+tv, data=tv.data1)  
summary(fit24)
ggAcf(residuals(fit24))

fit25 <- lm(trans ~ trans1+sb+snb+tv+w1+w2+w4+w5+w6+w7, data=tv.data1)  
summary(fit25)
ggAcf(residuals(fit25))

# sb is not stat significant; TV is statistically significant

