library(datasets)
library(ggplot2)
library(caret)
library(e1071)
library(rattle)

data(iris)
names(iris)

#Looking at the distribution of the three species in the dataset
table(iris$Species)

#splitting the data into training and test data sets
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

#plotting the data with the widths in the axes
#quick plot
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

#creating the decision tree
modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)

fancyRpartPlot(modFit$finalModel)

#using the model we built in the training data set to predict values in the testing data set
predict(modFit,newdata = testing)
