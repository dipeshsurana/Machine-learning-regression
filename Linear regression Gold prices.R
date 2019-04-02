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
data$Vol = shift(data$Vol, n=1, fill=0, type="lag")
dataset = data[c('Vol','Close')]

# Splitting the dataset into the Training set and Test set 
set.seed(123) 
split = sample.split(dataset$Close, SplitRatio = 0.8) 
training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE)

# Build linear regression model
regressor = lm(formula = Close ~ ., 
               data = training_set)
summary(regressor)

# Predicting the Test set results 
y_pred = predict(regressor, newdata = test_set)

# Visualising the Training set results 
ggplot() + geom_point(aes(x = training_set$Vol,  
                          y = training_set$Close), colour = 'red') +
  geom_line(aes(x = training_set$Vol, 
                y = predict(regressor, newdata = training_set)), colour = 'blue') +
  ggtitle('Close vs Volume (Training set)') +
  xlab('Volume') +
  ylab('Close') 

# Visualising the Test set results 
ggplot() + geom_point(aes(x = test_set$Vol, y = test_set$Close), 
             colour = 'red') +
  geom_line(aes(x = training_set$Vol, 
                y = predict(regressor, newdata = training_set)), colour = 'blue') +
  ggtitle('Close vs Vol (Test set)') +
  xlab('Vol') +
  ylab('Close') 
