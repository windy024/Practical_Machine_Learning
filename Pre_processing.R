library(caret)
library(kernlab)
library(ggplot2)
library(RANN)

data(spam)
names(spam)

inTrain <-createDataPartition(y=spam$type,p=0.75,list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

#plotting the variable average run length of capital letters in a row
hist(training$capitalAve,xlab="Average capital run length")

#we can see that the values are skewed 
#almost all the observations contain very low values
#hence we should standardize this variable so that the algorithm isn't confused 

mean(training$capitalAve)
#5.356006

sd(training$capitalAve)
#33.86992

#To standardize we subtract the mean and divide the value by the standard deviation
#This will make the mean 0 and the standard deviation 1
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)

mean(trainCapAveS)
#-8.694804e-18

sd(trainCapAveS)
#1

hist(trainCapAveS)

#while applying this to the test data, the observations 
#should be subtracted by the mean of the training data set 
#and divided by the standard deviation of the training data set 

preObj <- preProcess(training[,-58],method = c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve

mean(trainCapAveS)
#-8.694804e-18

sd(trainCapAveS)
#1

#applying to the test set can also be done with the object we created
testCapAveS <- predict(preObj,testing[,-58])$capitalAve

mean(testCapAveS)
#-0.01943043

sd(testCapAveS)
#0.7145363

#But, even after standardization, the variable is not normally distributed
#Hence, we use the "BoxCox" method of transformation which makes a continuous variable 
#almost normally distributed using maximum likelyhood method

#BoxCox transformation 
preObj <- preProcess(training[,-58],method = c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve

par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

#We see that this still does not take care of the problem fully
#there are still some values that are highly skewed outside the normal distribution
#and the qq plot also indicated that there is a skew
#this is possible because of repeating 0 values. This method does not deal with such problems

#Dealing with repeating/missing values
#imputing values

set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

#comparison of quantiles between imputed values and true values 
quantile(capAve - capAveTruth)

#only missing values
quantile((capAve - capAveTruth)[selectNA])

#only non missing values
quantile((capAve - capAveTruth)[!selectNA])


