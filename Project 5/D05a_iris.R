######################################################################################################
# D05a Analyzing Iris Data  
# By William Yu, UCLA Anderson Forecast
# 10/20/2018
##################################################################################################### 
setwd("~/Documents/DataScience/UCLADatascience/Project 5")

install.packages("caret")    
install.packages('e1071', dependencies=TRUE)
install.packages("lattice")
install.packages("rattle")
library(caret)
library(lattice)
library(e1071)
library(ggplot2)
library(rattle)

str(iris)
head(iris)

# We use the dataset to create a partition (60% training 40% testing)
fullset <- createDataPartition(iris$Species, p=0.60, list=FALSE)

# select 60% of data to train the models
trainset <- iris[fullset,]
# select 40% of the data for testing
testset <- iris[-fullset,]

dim(trainset)

# Tree model
fit.tree <- train(Species~., data=trainset, method="rpart")
fancyRpartPlot(fit.tree$finalModel)

pred.tree <- predict(fit.tree, newdata=testset)
table(pred.tree,testset$Species)
correct <- pred.tree == testset$Species
qplot(Petal.Length, Petal.Width, colour=correct, data=testset)

# Random Forest
fit.rf <- train(Species~., data=trainset, method="rf")
pred.rf <- predict(fit.rf, newdata=testset)
table(pred.rf,testset$Species)

correct <- pred.rf == testset$Species
qplot(Petal.Length,Petal.Width,colour=correct,data=testset)

# Naive Bayes
fit.bayes <- naiveBayes(Species~., data=trainset)
pred.bayes <- predict(fit.bayes, newdata=testset)
table(pred.bayes,testset$Species)

# SVM
fit.svm <- svm(Species~., data=trainset, kernel="radial")
pred.svm <- predict(fit.svm, newdata=testset)
table(pred.svm,testset$Species)

#
# We use the dataset to create a partition (90% training 10% testing)
#

fullset <- createDataPartition(iris$Species, p=0.90, list=FALSE)

# select 90% of data to train the models
trainset <- iris[fullset,]
# select 10% of the data for testing
testset <- iris[-fullset,]

dim(trainset)

# list types for each attribute
sapply(trainset, Species)
head(trainset)

# List the levels for the Species
levels(trainset$Species)

# Summarize the Species distribution
table(trainset$Species)
percent1 <- prop.table(table(trainset$Species))
cbind(freq=table(trainset$Species), percentgage=percent1)

summary(trainset)

# Plots
x1 = trainset[,1:4]
y1 = trainset[,5]

featurePlot(x=x1,y=y1, plot="box")
featurePlot(x=x1,y=y1, plot="density")

qplot(Petal.Length,Petal.Width,colour=Species,data=iris)
qplot(Sepal.Length,Sepal.Width,colour=Species,data=iris)

#
# 10-fold Cross-Validation
#
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Linear algorithms
# Linear Discriminant Analysis (LDA)
set.seed(7)
fit.lda <- train(Species~., data=iris, method="lda", metric=metric, trControl=control)

# Nonlinear algorithms
# Classfication and Regression Trees (CART)
set.seed(7)
fit.cart <- train(Species~., data=iris, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (KNN)
set.seed(7)
fit.knn <- train(Species~., data=iris, method="knn", metric=metric, trControl=control)

# Support Vector Machines (SVM)
set.seed(7)
fit.svm <- train(Species~., data=iris, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=iris, method="rf", metric=metric, trControl=control)

# Select Best Model
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# Kappa Statistics (0<=k<=1)
# Adjust accuracy by accounting for the possibility of a correct prediction by chance alone.
# Very good: 0.8<k<1; good: 0.6<k<0.8; moderate: 0.4<k<0.6; fair: 0.2<k<0.4; poor: 0<k<0.2

# Summarize the Best Model
print(fit.lda)

# Make Predictions
predict1 <- predict(fit.lda, testset)
confusionMatrix(predict1, testset$Species)
table(predict1,testset$Species)

