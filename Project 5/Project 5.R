setwd("~/Documents/DataScience/UCLADatascience/Project 5")

library(readxl)
library(caret)
library(leaps)
library(tidyverse)
library(corrplot)

blood_train <- read_xlsx("Blood_traindata.xlsx")
blood_test <- read_xlsx("blood_testdata.xlsx")
head(blood_train)
head(blood_test)

blood_train <- select(blood_train, c(-"ID", -"Total Volume Donated (c.c.)"))
blood_test <- select(blood_test, c(-"ID", -"Total Volume Donated (c.c.)"))
names(blood_train) <- c("Last_donation", "Number_donations", "First_donation", "Donation")
names(blood_test) <- c("Last_donation", "Number_donations", "First_donation", "Donation", "Percentage")

# Variable selection (train-test) for logistic regression -----------------
set <- createDataPartition(blood_train$Donation, p=0.75, list=FALSE)
trainset <- blood_train[set,]
testset <- blood_train[-set,]
head(trainset, 15)
sum(trainset$Donation)/300
sum(testset$Donation)/100

# Model Training
logistic_reg <- glm(Donation ~ 
                      Last_donation * Number_donations * First_donation, 
                    data = trainset, 
                    family = "binomial") # binomial when dependent var is 0 or 1, True or False because the errors have to be 0 or 1. Taking out Total Volume Donated because it is a multiple of Number of Donations.
summary(logistic_reg)

# Ameliorating the model
var_select <- regsubsets(Donation ~ 
                           Last_donation * Number_donations * First_donation, 
                         data = trainset)
var_summary <- summary(var_select)
which.max(var_summary$adjr2)
which.min(var_summary$cp)
which.min(var_summary$bic)

logistic_reg2 <- glm(Donation ~ 
                       Number_donations + 
                       Number_donations:First_donation + 
                       Last_donation:Number_donations + 
                       Last_donation:Number_donations:First_donation, 
                     data = trainset)
summary(logistic_reg2)

# Testset Prediction (logistic Regression) --------------------------------

testset_answers <- predict(logistic_reg2, testset, data=trainset, type="response")

pred_test <- cbind(testset, testset_answers) %>% 
  mutate(guess = ifelse(testset_answers >= .5, 1, 0), 
         correct = ifelse(Donation == guess, 1, 0)
         )

# Confusion matrix 
table(pred_test$guess, pred_test$Donation)
(perc_correct <- sum(pred_test$correct / nrow(pred_test)))

# logistic_reg2 does not seem to be much better than logistic_reg, that said, it contains 2 fewer variables. It is also worth noting that the logistic model seem to be worse than guessing. Predicting all donor will not give blood render a 76% accuracy rate.


# 10-Fold CV (all classification techniques) ------------------------------

str(blood_train) # We need to have Donation as a factor
blood_train$Donation <- as.factor(blood_train$Donation)
blood_train <- blood_train %>% 
  select(Last_donation, 
         Number_donations, 
         First_donation, 
         Donation)

control <- trainControl(method="cv", number=10)

# Linear algorithms
# Linear Discriminant Analysis (LDA)
# set.seed(1)
fit.lda <- train(Donation ~ ., 
                 data = blood_train, 
                 method = "lda", 
                 metric = "Accuracy", 
                 trControl = control)

# Nonlinear algorithms
# Classfication and Regression Trees (CART)
# set.seed(2)
fit.cart <- train(Donation ~ ., 
                 data=blood_train, method="rpart", 
                 metric = "Accuracy", 
                 trControl = control)

# k-Nearest Neighbors (KNN)
# set.seed(3)
fit.knn <- train(Donation ~ ., 
                 data=blood_train, method="knn", 
                 metric = "Accuracy", 
                 trControl = control)

# Support Vector Machines (SVM)
# set.seed(4)
fit.svm <- train(Donation ~ ., 
                 data = blood_train, 
                 method = "svmRadial", 
                 metric = "Accuracy", 
                 trControl = control)

# Random Forest
# set.seed(5)
fit.rf <- train(Donation ~ ., 
                data = blood_train, 
                method="rf", 
                metric = "Accuracy", 
                trControl = control)

# Select Best Model
# summarize accuracy of models
(results <- resamples(list(lda = fit.lda, 
                          cart = fit.cart, 
                          knn = fit.knn, 
                          svm = fit.svm, 
                          rf = fit.rf)))
summary(results)
# While the accuracy means for the models are all about the same (.75), the kappa means vary more indicating that the random forest is clearly a better model. Comparing the logistic regression model, it would appear that the logistic regression is better than our random forest in regards to the accuracy, however, we do not get to see the kappa albeit, there is significant error associated with the logistic regression. For that reason, I will proceed with the random forest model.

print(fit.rf)

# Predictions to blood_test -----------------------------------------------

predict_rf <- predict(fit.rf, blood_test)

blood_test$Donation <- predict_rf 
blood_test <- cbind(blood_test, predict(fit.rf, blood_test, type = "prob"))
blood_test <- blood_test %>% mutate(Percentage = ifelse(Donation == 0, `0`, `1`)) %>% select(-`0`, -`1`)
# Percentage is the certainty level for the predicted Donation value.

write.csv(x = blood_test, file = "BloodDonationPredictions")
