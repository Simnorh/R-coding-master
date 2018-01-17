#10 fold cross validation
# 1. train on 1-9 test 10
# 2. train on 2-10 test 1
# 3. train on 1 + 3-10 test 2
#and so on
ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

set.seed(123)

#He imports library MASS for his iris dataset
#and caret for cross validation

#library(MASS, quietly=TRUE)

methods(sigma)
install.packages("pbkrtest", dependencies = TRUE)
install.packages("car",dependencies=TRUE)
install.packages("lme4", dependencies = TRUE)
install.packages("caret",dependencies=TRUE)

library(car)
library(lme4)
library(caret)
#Create train and test dataset, target variable ??

library(caTools)

#Storing datasetx into dataframe named dataframe, allows for reusing of coding for every dataset, exchange name of datasetx
DataFrame <- datasetx

#Help datasetx to know more about dataset
help("datasetx")

#check structure of data
str(DataFrame)

#check dimensions of data
dim(DataFrame)

#first 3 row
head(DataFrame,3)

#summary of Data
summary(DataFrame)

#check number of unique values
apply(DataFrame,2,function(x) length(unique(x)))

#check dataset again
str(DataFrame)



ind = createDataPartition(riv$elv, p = 9/10, list = FALSE)
TrainDF <- riv[ind,]
TestDF <- riv[-ind,]

#We will be using the caret package for crossvalidation.Function

ControlParameters <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)
method("cv") #cross validation method
number = 10 #number of cross validations
classProbs =TRUE #model will save predictions for each class


#Lets choose the model parameters. Here we are choosing mtry of Random
#and choosing 3 values, you can choose another model also and its parameters in the function expand.grid 
#which will create a grid of all combinations of parameters

parameterGrid <- expand.grid(mtry=c(2,3,4)) #check out video on this part again

#We will put the above parameter in the model below in trControl arguement
#and now fit the model using train function
#to know more about train function run ?train in console

modelRandom <- train(variable~.,
                     data = TrainDF,
                     method = "lm",
                     trControl = ControlParameters,
                     tuneGrid = parameterGrid)
                     #preProcess = c('center', 'scale')"can be removed")
#testvariable~. means we want to predict testvariable and ~. means by way of all other variables
#method="rf" means random forest model
#preProcess centers and scales data
#to know which models or methods are available other than random forest names(getModelInfo())

names(getModelInfo)
modelRandom


predictions <- predict(modelRandom, TestDF)

#check confusion matrix

t <- table(predictions = predictions, actual=TestDF$variable)
t


# 1. resample data.
# 2. dele opp datasettet i 10 biter
# 3. se hvordan estimering av 100 års flom endrer seg ved bruk av forskjellige treningssett
# pålitelighets kriterie

setwd("D:/Master/Thesis/Data")

ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

testriver <- ams_data[ams_data$station == 2.1,]

qqplot(testriver$daily_ams_dates, testriver$daily_ams.1)

resampled_data <- testriver[sample(nrow(testriver)),]


#Dataset is read in
#column "station" has been created to identify rivers based on 1 variable instead of two
#creates testriver dataframe consisting of all rows containing data from a given station (any way to do this for all stations simultaneously?)
#Data of testriver dataframe is resampled randomly

#try installing tidyverse
#install.packages("tydiverse", dependencies = TRUE)
#Maybe tydiverse will be useful later, now? not so much.

data_folds <- cut(seq(1, nrow(resampled_data)), breaks = 10, labels=FALSE)
#divides dataframe into 10 "equally" sized chunks, NB requires randomized dataset!
       

for(i in 1:10){
  testIndexes <- which (data_folds == i, arr.ind = TRUE)
  testData <- resampled_data[testIndexes, ]
  trainData <- resampled_data[-testIndexes,]
}



library(car)
library(lme4)
library(caret)
#Create train and test dataset, target variable ??

library(caTools)

indices = createDataPartition(testriver$daily_ams.1, p = 8/10, list = FALSE)
TrainDF <- testriver[indices,]
TestDF <- testriver[-indices,]

#We will be using the caret package for crossvalidation.Function

ControlParameters <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)
method("cv") #cross validation method
number = 10 #number of cross validations
classProbs =TRUE #model will save predictions for each class


#Lets choose the model parameters. Here we are choosing mtry of Random
#and choosing 3 values, you can choose another model also and its parameters in the function expand.grid 
#which will create a grid of all combinations of parameters

parameterGrid <- expand.grid(mtry=c(1,2,3,4,5,6)) #check out video on this part again

#We will put the above parameter in the model below in trControl arguement
#and now fit the model using train function
#to know more about train function run ?train in console

modelRandom <- train(daily_ams.1~.,
                     data = TrainDF,
                     method = "lm",
                     trControl = ControlParameters,
                     tuneGrid = parameterGrid,
                     na.action = na.exclude)
#preProcess = c('center', 'scale')"can be removed")
#testvariable~. means we want to predict testvariable and ~. means by way of all other variables
#method="rf" means random forest model
#preProcess centers and scales data
#to know which models or methods are available other than random forest names(getModelInfo())

names(getModelInfo)
modelRandom


predictions <- predict(modelRandom, TestDF)














rmse <- function()
{
  sqrt(mean(error^2))
}




MAE(pred = trainData, obs = testData, na.rm = TRUE)

