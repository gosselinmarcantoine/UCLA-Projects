######################################################################################################
# D03d Classification
# By William Yu, UCLA Anderson Forecast
# 10/11/2018
##################################################################################################### 

# Read Logistic Regression: Stock market data from D03c_logit.R

##
## K-Nearest Neighbors (KNN)
##

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=10)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data
?Caravan
fix(Caravan)
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
(873+10)/1000
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
(921+5)/1000
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
(930+4)/1000
knn.pred=knn(train.X,test.X,train.Y,k=10)
table(knn.pred,test.Y)
(941+0)/1000
knn.pred=knn(train.X,test.X,train.Y,k=50)
table(knn.pred,test.Y)
(941+0)/1000

class(Caravan$Purchase)
glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
summary(glm.fits)

glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
934/1000

glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)

#
# Practice: Wisconsin Breast Cancer data 
#
admitg <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")

##
## Linear Discriminant Analysis
##

library(ISLR)
library(MASS)
head(Smarket)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

