rm(list = ls())
T1<-Sys.time()

# Load Libraries
require('sqldf')
require('caTools')
require('data.table')
require('caret')
require('ggplot2')

# set your directory
setwd("/Users/dipeshsurana/Documents/Machine Learning")

# Load Data
data = read.csv('Gold.csv')
data$Nextopen = shift(data$Open,n=1,fill=0,type="lead")
dataset = data[,-1]

# Splitting the dataset into the Training set and Test set 
set.seed(123) 
split = sample.split(dataset$Nextopen, SplitRatio = 0.8) 
training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE)

#Exploration of Data
pairs(training_set)

# Build linear regression model
regressor = lm(formula = Nextopen ~ ., 
               data = training_set)
summary(regressor)

# After evaluating p-value at 95% confidence threshold
regressor_new = lm(formula = Nextopen ~ Open+High+Close,data=training_set)
summary(regressor_new)

# Other useful functions 
coefficients(regressor_new) # model coefficients
confint(regressor_new, level=0.95) # CIs for model parameters 
fitted(regressor_new) # predicted values
residuals(regressor_new) # residuals
anova(regressor_new) # anova table 
vcov(regressor_new) # covariance matrix for model parameters 
influence(regressor_new) # regression diagnostics

# compare models
anova(regressor,regressor_new)

# Predicting the test set results 
y_pred = predict(regressor_new, newdata = test_set)

#actual & predicted dataframe
AP<-data.frame(test_set$Nextopen,y_pred)

#Evaluation of model
AIC(regressor_new)
BIC(regressor_new)
RMSE(y_pred,test_set$Nextopen)
cor(AP)

# Monitor
predict(regressor_new,data.frame(Open=1296.35,High=1297.75,Close=1295.55))

