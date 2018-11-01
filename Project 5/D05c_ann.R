######################################################################################################
# D05c Artifical Neural Networks (ANNs)
# By William Yu, UCLA Anderson Forecast
# 10/20/2018
##################################################################################################### 

install.packages("neuralnet")       
library(neuralnet) 
library(leaps)
library(boot)

setwd("~/Documents/DataScience/UCLADatascience/Project 2")
caschool <- read.csv("W2c_API.csv")
setwd("~/Documents/DataScience/UCLADatascience/Project 5")
head(caschool)
caschool=caschool[,-1]
head(caschool)

colSums(is.na(caschool)) # Detect missing values
caschool=na.omit(caschool) # Remove those rows with missing values

# Linear regression model selection
reg=regsubsets(api~.,caschool,nvmax=13)
reg.summary=summary(reg)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
names(caschool)
coef(reg,12)
coef(reg,11)

fit1=glm(api~.,data=caschool)
fit2=glm(api~.-charter,data=caschool)
fit3=glm(api~.-charter-lausd,data=caschool)

cv.error1=cv.glm(caschool,fit1,K=10)$delta[1]
cv.error2=cv.glm(caschool,fit2,K=10)$delta[1]
cv.error3=cv.glm(caschool,fit3,K=10)$delta[1]
cv.error1;cv.error2;cv.error3

part <- createDataPartition(caschool$api, p=0.60, list=F)
train <- caschool[part,]
test <- caschool[-part,]

reg1=lm(api~.-charter,data=train)
pred.reg1 <- predict(reg1, newdata=test)
mse1=sum((pred.reg1-test$api)^2)/nrow(test)
mse1

# ANNs (Artificial Neural Networks) - the Beginning of Deep Learning

ann1 <- neuralnet(api~chci+alternative+lausd+lacounty+english+black+asian+latino+white
                  +elep+highp+poverty, data=train)
plot(ann1)

ann5 <- neuralnet(api~chci+alternative+lausd+lacounty+english+black+asian+latino+white
                  +elep+highp+poverty, data=train, hidden=5)
plot(ann5)

result.ann1 = compute(ann1,test[1:12])    
pred.ann1=result.ann1$net.result
output=cbind(pred.ann1,test$api)
output[1:20,]
mse2=sum(pred.ann1-test$api)^2/nrow(test)
mse2

result.ann5 = compute(ann5,test[1:12])       
pred.ann5=result.ann5$net.result
output=cbind(pred.ann5,test$api)
output[1:20,]
mse3=sum(pred.ann5-test$api)^2/nrow(test)
mse3

mse1;mse2;mse3
