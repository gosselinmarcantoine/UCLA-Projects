######################################################################################################
# D04a Resampling Methods 
# By William Yu, UCLA Anderson Forecast
# 10/15/2018
##################################################################################################### 
setwd("~/Documents/DataScience/UCLADatascience/Project 4")

##
## Chapter 5 Lab : Cross-Validation and the Bootstrap
##

# The Validation Set Approach
library(ISLR)
library(ggplot2)

?Auto    # Gas mileage, horsepower for 392 vehicles
head(Auto)

attach(Auto)
plot(horsepower, mpg, pch=20)

fit01=lm(mpg ~ horsepower,data=Auto)
summary(fit01)
fit02=lm(mpg~poly(horsepower,2),data=Auto)
summary(fit02)
fit03=lm(mpg~poly(horsepower,3),data=Auto)
summary(fit03)

ggplot(Auto, aes(x=horsepower,y=mpg))+
  geom_point()+
  geom_line(aes(y=predict(fit01)),color="black", size=1)+
  geom_line(aes(y=predict(fit02)),color="blue", size=1)+
  geom_line(aes(y=predict(fit03)),color="red", size=1)

?sample
what=sample(20,10)
what
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

# Calculating MSE (Mean squared error)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

install.packages("boot")
library(boot)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit) # leave one out CV function: cv.glm()
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# k-Fold Cross-Validation: 10 folds/10 groups

set.seed(17)
cv.error.10=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

##
## The Bootstrap
##
# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
