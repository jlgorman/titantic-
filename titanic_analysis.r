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

##verifying dataframe
#is.data.frame

####Dealing with missing values
##look for missing values (find several in age)
train$Fare[train$Fare == 0] <- NA
train[c(62, 830), 'Embarked'] <- NA
data.frame('missing_values' = sapply(train, function(x) sum(is.na(x))))

#fill missing values with mean for similiar passengers 
Fare_mean <- median(train$Fare[train$Pclass == 1], na.rm = T)
ggplot(train[train$Pclass == 1 & train$Fare >= Fare_mean,], aes(x = Embarked, fill = 'coral')) +
  geom_bar(show.legend = F) +
  theme_classic()
train[c(62, 830), 'Embarked'] <- 'C'


tab <- summaryBy(Fare ~ Embarked + Pclass, data = train, FUN = median, na.rm = T)
tab
train$Fare[train$Embarked =='S' & train$Pclass == '1' & is.na(train$Fare)] <- tab$Fare.median[7]
train$Fare[train$Embarked =='S' & train$Pclass == '2' & is.na(train$Fare)] <- tab$Fare.median[8]
train$Fare[train$Embarked =='S' & train$Pclass == '3' & is.na(train$Fare)] <- tab$Fare.median[9]


nam <- names(train)[c(1, 4, 9, 11)]
imp <- mice(data = train[,!names(train) %in% nam], seed = 8, printFlag = F)
data.frame(count = 1:5, median = apply(imp$imp$Age, 2, median))


imp_titanic <- complete(imp, 3)
train$Age <- imp_titanic$Age
data.frame('missing_values' = sapply(train, function(x) sum(is.na(x))))

#drop unneccesary variables in effort to increase performance
train <- train[,c(-1, -4, -9, -11, -12)]



##analyses
# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

###compare several algorithms
##Linear Algorithms: 
#Logistic Regression
set.seed(5)
glm.fit <- train(Survived~., data=train, method="glm", metric=metric, trControl=trainControl, na.action = na.pass)


#Linear Discriminate Analysis 
set.seed(5)
lda.fit <- train(Survived~., data=train, method="lda", metric=metric, trControl=trainControl, na.action = na.pass)


# Regularized Logistic Regression 
set.seed(5)
glmnet.fit <- train(Survived~., data=train, method="glmnet", metric=metric, trControl=trainControl, na.action = na.pass)

# KNN
set.seed(5)
knn.fit<- train(Survived~., data=train, method="knn", metric=metric, trControl=trainControl, na.action = na.pass)

#Classification and Regression Trees
set.seed(5)
cart.fit <- train(Survived~., data=train, method="rpart", metric=metric,
    trControl=trainControl)

#Naive Bayes
set.seed(5)
nb.fit <- train(Survived~., data=train, method="nb", metric=metric, trControl=trainControl)

#SVM
set.seed(5)
svm.fit <- train(Survived~., data=train, method="svmRadial", metric=metric,
    trControl=trainControl)

#Compare algorithms
results <- resamples(list(LG=glm.fit, LDA=lda.fit, GLMNET=glmnet.fit, KNN=knn.fit,
    CART=cart.fit, NB=nb.fit, SVM=svm.fit))
summary(results)

dotplot(results)

