setwd('~/Documents/DataScience/UCLADatascience/Project 7')

# Libraries needed:
library(readxl)
library(purrr)
library(tidyverse)
library(plm)
library(imputeTS)
library(corrplot)
library(caret)
library(glmnet)
library(mlbench)
library(psych)
library(VIM)
library(foreign)
library(car)
library(prediction)

# Dependent variable: 'Poverty gap at $3.20 a day (2012 PPP) (%)'

df <- read_excel('W03b_wdi.xlsx')
head(df)
map_dbl(df, ~sum(is.na(.))/ nrow(df)) # Lots of missing values (2017 seems to have partial data)
map_dbl(df[, 1:4], ~length(unique(.))) # 1591 variables: data is not tidy
df$`Country Code` <- factor(df$`Country Code`)

# Pre-processing ----------------------------------------------------------

# Simplify dataframe
indicator_reference <- subset(df, select = c("Indicator Name", "Indicator Code")) %>% distinct()
df <- subset(df, select = -`Indicator Name`)
Country_reference <- subset(df, select = c("Country Name", "Country Code")) %>% distinct()
df <- subset(df, select = -`Country Name`)

# Remove countries without value for Indicator Code: SI.POV.LMIC.GP in 2012
CountryCodes <- filter(df[df[, 2] == 'SI.POV.LMIC.GP', 1], df[df[, 2] == 'SI.POV.LMIC.GP', 55] != 'NA')
df <- subset(df, `Country Code` %in% CountryCodes$`Country Code`)
rm(CountryCodes)

# subset out years 1960 to 1980, and 2017 (2017 has partial data)
df <- subset(df, select = -c(`1960`:`1980`, `2017`))

# Separates dataframe into training and testing sets
df_test <- subset(df, select = c(1:2, 35:38)) # Excludes 2017 as well
df <- subset(df, select = 1:34)

# gathering time columns into one year column
df <- df %>% gather(key = 'Year', value = 'value', c(3:34)) # %>% filter(value != 'NA') 
df_test <- df_test %>% gather(key = 'Year', value = 'value', c(3:6))
df$Year <- factor(df$Year)

# Spread Indicator code to different columns
df <- df %>% spread(key = 'Indicator Code', value = 'value')
df_test <- df_test %>% spread(key = 'Indicator Code', value = 'value')

# Remove Indicators with over 25% missing values in training set
dep_var <- subset(df, select = 'SI.POV.LMIC.GP')
df <- subset(df, select = map_dbl(df, ~sum(is.na(.))/ nrow(df)) <= .25)
df <- cbind(df, dep_var)
df_test <- subset(df_test, select = names(df)) # Selecting the same variables as the training dataset
rm(dep_var)


# Variable Selection ------------------------------------------------------

# Imputation would be too intensive to conduct on all the data. Instead we will subset a few years' worth of the data, conduct imputation ultimately variable selection to figure the best independent variables to use, and then subset the desired independent variables from the original dataset, impute the variables and apply the model chosen.

# Restrict dataset to 2007 and 2012
df_x <- df %>% filter(df[, 2] %in% c('2012', '2007')) # Add 2007 because some variables don't have values from 2009 - 2012. Choosing 2007 because we want a year that is similar to the test set 2013-2017 and 2008 differs considerably because of a major world fiancial collapse.
map_dbl(df_x, ~sum(is.na(.))/ nrow(df_x))
df_x <- kNN(df_x, variable = names(df_x[, 3:321]), k = 3, imp_var = FALSE)
map_dbl(df_x, ~sum(is.na(.))/ nrow(df_x))

# Filter methods do not adress multi-collinearity problems well and wrapper methods handles large numbers of explanatory variables poorly (long run-time). As a result, I will use an embbeded method for variable selection. These methods include: Ridge, LASSO, and Elastic Net. Note: LASSO (and Elastic Net) are what we want here because it does variable selection by taking into consideration multicollinearity.

# Dependent variable examination
hist(df$SI.POV.LMIC.GP) # Non-normal distrubution so we cannot use a linear model; use GLM instead.

# Custom Control Parameter
custom <- trainControl(method = 'repeatedcv',
                       number = 10,
                       repeats = 5,
                       verboseIter = FALSE) # if verboseIter = TRUE, See the model running 

# glm models
set.seed(12)
ridge <- train(SI.POV.LMIC.GP ~ .,
               df_x[3:322],
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0, # alpha = 0: ridge | alpha = 1: lasso | alpha >0 & <1 means elastic net
                                      lambda =  seq(0.0001, 1, length = 5)),
               trControl = custom,
               na.action = na.omit)

set.seed(12)
lasso <- train(SI.POV.LMIC.GP ~ .,
               df_x[3:322],
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1, # alpha = 0: ridge | alpha = 1: lasso | alpha >0 & <1 means elastic net
                                      lambda =  seq(0.0001, 1, length = 5)),
               trControl = custom,
               na.action = na.omit) 

set.seed(12)
elasticnet <- train(SI.POV.LMIC.GP ~ .,
             df_x[3:322],
             method = 'glmnet',
             tuneGrid = expand.grid(alpha = seq(0, 1, length = 10), # alpha = 0: ridge | alpha = 1: lasso | alpha >0 & <1 means elastic net
                                    lambda =  seq(0.0001, 1, length = 5)),
             trControl = custom,
             na.action = na.omit) 

# Compare Models
model_list <- list(Ridge = ridge, Lasso = lasso, ElasticNet = elasticnet)
res <- resamples(model_list)
summary(res)

# Elastic Net is a slightly better model.

# Plot Results
elasticnet
plot(elasticnet$finalModel, xvar = 'lambda', label = T)
plot(elasticnet$finalModel, xvar = 'dev', label = T) # We see great risks of overfitting past .9
plot(varImp(elasticnet, scale = F))

# Graphs above show that there is an exponential trend in explanatory importance in the variables.

# Meaningful variable names
imp <- data.frame(varImp(elasticnet, scale = F)[1])
(vars <- rownames(imp)[order(imp$Overall, decreasing=TRUE)[1:34]])

rm(df_x, imp, lasso, ridge, custom, res, model_list)

# Vars shows 5 of the best explanatory variables
filter(indicator_reference, `Indicator Code` %in% vars)


# Data Formatting ---------------------------------------------------------

# Reduce data sets with key variables
df <- subset(df, select = c("Country Code", "Year", "SI.POV.LMIC.GP", vars))
dependent_variable <- df_test$SI.POV.LMIC.GP
df_test <- subset(df_test, select = c("Country Code", "Year", vars))

df <- kNN(df, variable = vars, k = 5, imp_var = FALSE)
df_test <- kNN(df_test, variable = vars, k = 5, imp_var = FALSE)

df <- rename(df, 'Country' = 'Country Code')
df_test <- rename(df_test, 'Country' = 'Country Code')

# Panel Data Analysis Model -----------------------------------------------

# Our data is panel data because our time series is repartitionned across another variable, in this case countries. Here is a great video to explain how to handle panel data time series analysis: https://www.youtube.com/watch?v=f01WjeCdgEA

# For step-by-step help: https://www.princeton.edu/~otorres/Panel101R.pdf

scatterplot(SI.POV.LMIC.GP ~ Year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=df)

linear_model <- lm(
  as.formula(paste0("SI.POV.LMIC.GP ~ ", paste(vars, collapse = " + "))),
  data = df)

# Fixed effect panel data model
fxd_effect <- plm(
  as.formula(paste0("SI.POV.LMIC.GP ~ ", "Country + ", paste(vars, collapse = " + "))),
  data = df,
  method = "within",
  index = c("Country", "Year"))

# Random effect panel model
random <- plm(
  as.formula(paste0("SI.POV.LMIC.GP ~ ", paste(vars, collapse = " + "))),
  data = df,
  method = "within",
  model = "random",
  index = c("Country", "Year"))

# Time effect panel data model
time_effect <- plm(
  as.formula(paste0("SI.POV.LMIC.GP ~ ", "lag(Year, 1) + ", paste(vars, collapse = " + "))),
  data = df,
  method = "within",
  effect = "time",
  index = c("Country", "Year")) # Fixed effect model

# Comparing the models ----------------------------------------------------

linear <- summary(linear_model) # linear model is usually the basis for comparison
linear$adj.r.squared

fxd <- summary(fxd_effect)
fxd$r.squared

# Comparing the models
pFtest(fxd_effect, linear_model) # P-value is small, fxd_effect is better.

rand <- summary(random)
rand$r.squared

# Choice b/w Fixed Effect and Random Effect
# Hausman Test
phtest(fxd_effect, random)  # If p-value is <0.05, then the fixed effect model is better than random effect!
pFtest(fxd_effect, random)  # we keep the fixed effect

time <- summary(time_effect)
time$r.squared

# Model Prediction --------------------------------------------------------

# prediction(time_effect, data = df_test, at = NULL, calculate_se = FALSE) returns the following error: Error in crossprod(beta, t(X)) : non-conformable arguments.
# Unfortunately, forecasting with panel data is complex and there are no good function presently in R that enables forecasting from unbalanced panel data models. For that reason, I will select the best model from the models I can generate a forecast.

# Linear model forecast
LMprediction <- predict(linear_model, df_test)
Predicted <- cbind(df_test, Actual = dependent_variable, Prediction = LMprediction)

# Prediction accuracy
Predicted$Prediction <- as.numeric(Predicted$Prediction)
Predicted$Actual <- as.numeric(Predicted$Actual)
Predicted <- Predicted %>% mutate(Error = Actual - Prediction)
sqrt(sum(Predicted$Error^2, na.rm = TRUE))
sum(!is.na(Predicted$Error))