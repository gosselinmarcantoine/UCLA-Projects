######################################################################################################
# D04a Model Selection and Regularization 
# By William Yu, UCLA Anderson Forecast
# 10/13/2018
##################################################################################################### 
getwd()
setwd("C:/Users/wyu/documents/zip08/2018 Q4 Fall_XData/Data")

##
## Chapter 6 Lab 1: Subset Selection Methods
##
## Best Subset Selection: Fully in-sample

library(ISLR)
fix(Hitters) # would need to download xquartz from https://www.xquartz.org/
?Hitters
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

install.packages("leaps")
library(leaps)

reg = lm(Salary ~ ., Hitters)
summary(reg)

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full) # star for which variable to include per number of vars included in model.
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19) # nvmax expands lines returned
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary) # different qualifying mesures to see which number of variables is best to include

# Show R^2 increases from 0.32 to 0.55 when the RHS variables increase from 1 to 19.
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)  # identify the location of the maximum point of a vector
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp) # Cp is AIC
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# plot(regfit.full,scale="r2")
# plot(regfit.full,scale="adjr2")
# plot(regfit.full,scale="Cp")
# plot(regfit.full,scale="bic")

coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
# Forward is when you choose one first, select the best then add a second and so on. backward is when you have all the variables and then take out the variable with the least information added. "information = R^2" Full is a more complex approach where it tries multiple ways... it takes longer to run.

# library(MASS) # Don't use this one... not so good.
# fit <-lm(Salary ~ ., data=Hitters)
# summary(fit)
# step <- stepAIC(fit, direction="both")

##
## Choosing Among Models: In-sample and Out-of-sample--Train and Test Sets
##
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi # Multiplication %*% of dataframe by vector
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)


predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)

##
## Chapter 6 Lab 2: Lasso Regression
##

# Lasso adds a correction (or weight) to variables (betas) if it "finds" that a variables pulls astray the regression. Similar to how the robust regression adds weight to compensate from outliers affecting the regression.

install.packages("glmnet")
library(glmnet)

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

grid=10^seq(10,-2,length=100)
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

##
## Chapter 6 Lab 3: Principal Components Regression
##

# Mostly used when there are a multitude of variables where you don't necessarily know what the variables are or how they affect the dependent variable.

install.packages("pls")
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)