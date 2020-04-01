library(ggplot2)
library(caret)
library(e1071)
library(rattle)
library(kernlab)
library(RANN)
library(ISLR)
library(splines)
library(randomForest)
library(gbm)

data(Wage)
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,] 
testing <- Wage[-inTrain,]

modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)

qplot(predict(modFit,testing),wage,data=testing)
