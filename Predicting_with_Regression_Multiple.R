library(ggplot2)
library(caret)
library(e1071)
library(rattle)
library(kernlab)
library(RANN)
library(ISLR)
library(splines)

data(Wage) 
Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],y = training$wage,plot="pairs")

qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)

modFit<- train(wage ~ age + jobclass + education,method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

plot(finMod,1,pch=19,cex=0.5,col="#00000010")

qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

plot(finMod$residuals,pch=19)

pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)
