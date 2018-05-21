#load packages
library (caret)
library(ggplot2)
library(corrplot)
#load data
train <- read.csv("D:/Data_Science/Data/titanic/train.csv",StringsAsFactors=FALSE)
test <- read.csv("D:/Data_Science/Data/titanic/test.csv", StringsAsFactors=FALSE)

#quick summary of the data
dim(train)

head(train)

sapply(train, class)

summary(train)