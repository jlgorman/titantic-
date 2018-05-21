#load packages
library (caret)
library(ggplot2)
library(corrplot)
library(e1071)
#load data
train <- read.csv("D:/Data_Science/Data/titanic/train.csv" , header = TRUE)
test <- read.csv("D:/Data_Science/Data/titanic/test.csv", header = TRUE)

#quick summary of the data
dim(train)

head(train)

sapply(train, class)

summary(train)

##wrangling
#convert survived to factor
train[,2] <- as.factor((train[,2]))


# 10-fold cross validation with 2 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=2)
metric <- "Accuracy"

###compare several algorithms
##Linear Algorithms: 
#Logistic Regression
set.seed(5)
glm.fit <- train(Survived~., data=train, method="glm", metric=metric, trControl=trainControl, na.action = na.pass)


#Linear Discriminate Analysis 
set.seed(5)
fit.lda <- train(Survived~., data=datasetTrain, method="lda", metric=metric, trControl=trainControl)



#Linear Discriminate Analysis (LDA)
#and Regularized Logistic Regression (GLMNET).
#Non-Linear Algorithms: k-Nearest Neighbors (KNN), Classification and Regression Trees (CART),
#Naive Bayes (NB) and Support Vector Machines with Radial Basis Functions (SVM).