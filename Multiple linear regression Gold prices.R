rm(list = ls())
T1<-Sys.time()

# Required Libraries
require('sqldf')
require('caTools')
require('data.table')
require('caret')
require('ggplot2')
require('e1071')

# set your directory
setwd("/Users/dipeshsurana/Documents/Machine Learning")

# Load Data
data = read.csv('Gold.csv')

# display first 20 rows of data
head(data, n=20)

# dsiplay the dimensions of your data
dim(data)

# list types for each attribute
sapply(data, class)

# Creating Target variable for regression
data$Nextopen = shift(data$Open,n=1,fill=0,type="lead")

# Creating Target variable for cliassification
data$Nextdirection = ifelse(data$Nextopen>data$Open,"Bull","Bear")

# list types for each attribute including target variables
sapply(data, class)

#Exploration of Data
pairs(training_set)

# distribution of class variable for classification 
y = data$Nextdirection
cbind(freq=table(y), percentage=prop.table(table(y))*100)

# distribution of class variable for regression using histogram
x = data$Nextopen
hist(x)

# summarize the dataset
summary(data)

# calculate standard deviation for all attributes
sapply(data[,2:8], sd)

# calculate skewness for each variable
skew = apply(data[,2:8], 2, skewness)
# display skewness, larger/smaller deviations from 0 show more skew
print(skew)

# calculate a correlation matrix for numeric variables
correlations <- cor(data[,2:8])
# display the correlation matrix
print(correlations)

# plot the price curve
plot(c(data$Date,as.numeric(data$Open)),type='l')

# Dropping Nextdirection variable for regression model
dataset = subset(data,select=-c(Nextdirection))
dataset = data[,-1]

# Splitting the dataset into the Training set and Test set 
set.seed(123) 
split = sample.split(dataset$Nextopen, SplitRatio = 0.8) 
training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE)

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
