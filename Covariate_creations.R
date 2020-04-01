library(ggplot2)
library(caret)
library(e1071)
library(rattle)
library(kernlab)
library(RANN)
library(ISLR)
library(splines)

data(Wage);

inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,] 
testing <- Wage[-inTrain,]

table(training$jobclass)

#creating dummy variables for qualitative variables
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))

#Removing zero covariates
#identify variables that have very similar values
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv

#spline basis
bsBasis <- bs(training$age,df=3) #creating a polinomial variable
bsBasis

lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

#applying this to the test data set
predict(bsBasis,age=testing$age)

