######################################################################################################
# D03c Logistic Regression
# By William Yu, UCLA Anderson Forecast
# 10/10/2018
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Data")

# Simulation of two scatter charts

x=rnorm(60, mean=10, sd=5)
y=4+0.8*x+rnorm(60, mean=0, sd=2)
plot(x,y,pch=20)
abline(lm(y ~ x), col="blue")

fit1=lm(y~x)
summary(fit1)

x1=rnorm(60, mean=10, sd=5)
y1=rnorm(60, mean=0, sd=2)
plot(x1,y1, pch=20)
abline(lm(y1 ~ x1), col="blue")

fit2=lm(y1~x1)
summary(fit2)

x2=c(18,20,21)   # simulate 3 outliers
y2=-5+0.5*x2 

x3=c(x,x2)
y3=c(y,y2)

plot(x3,y3,pch=20)
abline(lm(y ~ x), col="blue")
abline(lm(y3~x3),col="red")

fit3=lm(y3~x3)
summary(fit3)

##
## Robust Regression
## An alternative to least squares regression when data are contaminated with outliers 
## or influential observations. It can also be used for the purpose of detecting influential observations
##
library(MASS)
library(foreign)
cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
head(cdata)
# crime: violent crime per 100,000 people. murder: murder per 1M people. pctmetro: % of people living in metro area.
# pctwhite: % of population that is white. pcths: % of population with a high school education or above. 
# poverty: % of population living under poverty line. single: % of population that are single parents.
cor(cdata[,-c(1,2)])
fit1=lm(crime~pctmetro+pctwhite+pcths+poverty+single,data=cdata) 
summary(fit1)
fit2=lm(crime~pctmetro+poverty+single,data=cdata) 
summary(fit2)

opar=par(mfrow=c(2,2), oma=c(0,0,1.1,0))
plot(fit2, las=1)

# Robust Regression
rbt1=rlm(crime~pctmetro+poverty+single,data=cdata) 
summary(rbt1)
names(rbt1)
# See the weight of each obersvation. If all observations have weights of 1, then it is linear regression (OLS)
weights=data.frame(state=cdata$state, resid=rbt1$resid, weight=rbt1$w)
weights1=weights[order(weights$weight),]
weights1

# Simulation of a Logistic Function chart
x=seq(0,10,0.1)
y1=exp(x)
y2=exp(-x)
y3=-exp(-x)
y4=1/exp(-x)
y5=10*exp(x)/(100+exp(x))
y6=1/(0.1+10*exp(-x))

par(mfrow=c(3,2))  
plot(x,y1, pch=19)
plot(x,y2, pch=19)
plot(x,y3, pch=19)
plot(x,y4, pch=19)
plot(x,y5, pch=19)
plot(x,y6, pch=19)
par(mfrow=c(1,1))  

##
## Logistic Regression
## 
## To learn more models, you can see here: https://stats.idre.ucla.edu/other/dae/

admitg <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
# gre:GRE (Graduate Record Exam scores)
# rank: the prestige of the undergraduate college. 1 has the highest prestige.
head(admitg)
summary(admitg)
sapply(admitg,sd)
?xtabs
xtabs(~admit+rank,data=admitg)

logit1 = glm(admit~gre+gpa+rank, data=admitg, family="binomial") # whats the difference between glm and lm
summary(logit1) # Remember that even though gre has smaller estimate (econ. signif.) it reflect 1 of 800 points while GPA is out of 4.

# Calculate the probablity of admission for a student: GRE: 760, GOA:3.8, University rank: 2
# Probability=exp(a+bx)/(1+exp(a+bx))
logit1$coefficients
logit1$coef
y=logit1$coef[1]+logit1$coef[2]*760+logit1$coef[3]*3.8+logit1$coef[4]*2
y
prob=exp(y)/(1+exp(y)) # plugging in the value of alpha and betas in the formula into logistic regression formula
prob

# response will produce predicted probability
prob2=predict(logit1,data.frame(gre=760,gpa=3.8,rank=2), data=admitg,type="response")
prob2

# Convert rank to a facotr to indicate that rank should be treated as a categorical variable
admitg$rank=factor(admitg$rank)
logit2 = glm(admit~gre+gpa+rank, data=admitg, family="binomial")
summary(logit2)

#
# Logistic Regression: Heart data with Qualitative variables
#

install.packages("readxl")                        
library(readxl)                                  
heart <- read_excel("W03d_heart.xlsx")
head(heart)

logi.fit=glm(AHD~.,data=heart,family=binomial)

heart$AHD <- ifelse(heart$AHD =="Yes", 1,0)       # Replace character "Yes","No" with 1, 0
head(heart)

table(heart$ChestPain)
table(heart$Thal)

logi.fit=glm(AHD~.,data=heart,family=binomial)
summary(logi.fit)

# Logistic Regression: Stock market data
install.packages("ISLR")  
library(ISLR)
?Smarket
Smarket[1:10,]
fix(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)

fits=lm(Today~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket)
summary(fits)

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

train=(Year<2005)
plot(train)
Smarket.2005=Smarket[!train,]
Smarket.05 <- subset(Smarket, Year==2005) 
dim(Smarket.2005)
dim(Smarket.05)

Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)

glm.probs=predict(glm.fits,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction.2005)
(77+44)/252
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
(35+106)/252
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")




