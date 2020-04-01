library(ggplot2)
library(caret)
library(e1071)
library(rattle)
library(kernlab)
library(RANN)
library(ISLR)
library(splines)

data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

names(spam)[c(34,32)] #names of variables that are highly crrelated
par(mfrow=c(1,1))
plot(spam[,34],spam[,32])

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

#We can see that X is more useful here as there is clear variability 
#while y is centered around the same value
#using X instead of the 2 variables is the basic idea of variable reduction

#SVD - Singular Value Decomposition (similar to PCA) X = UDV^T
#where 
#X is the dataset
#U are orthogonal(left singular vectors)
#V are orthogonal(right singular vectors)
#D is a diagonal matrix (single values)

#PCA in R
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

prComp$rotation #looking how the PCA was performed

#performing PCA in the spam dataset
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

#PCA using caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#applying PCA on the training dataset
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
#modelFit <- train(training$type ~ .,method="glm",data=trainPC)
modelFit <- train(x = trainPC, y = training$type,method="glm")

#applying PCA on the test dataset
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))
