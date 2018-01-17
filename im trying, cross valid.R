setwd("D:/Master/Thesis/Data")

ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

testriver <- ams_data[ams_data$station == 2.36,]

resampled_data <- testriver[sample(nrow(testriver)),]

library(caret)

#set.seed determines "where" the following algorithm will start, making the code retestable, change seed nr to change folds
#set.seed(196)
#cv10fold <- createFolds(resampled_data$daily_ams.1, k = 10)
#str(cv10fold)

set.seed(198)
ind = createDataPartition(resampled_data$daily_ams.1, p = 9/10, list=FALSE)
str(ind)
TrainDF <- resampled_data[ind,]
TestDF <- resampled_data[-ind,]

ControlParameters <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

parameterGrid <- expand.grid(mtry=c(1,2,3,4,5)) #check out video on this part again

#We will put the above parameter in the model below in trControl arguement
#and now fit the model using train function
#to know more about train function run ?train in console

modelRandom <- train(daily_ams.1~daily_ams.1+FGP+fine_ams,
                     data = TrainDF,
                     method = "rf",
                     trControl = ControlParameters,
                     tuneGrid = parameterGrid)
modelRandom







predictions <- predict(modelRandom, newdata = TestDF)

#check confusion matrix

t <- table(predictions = predictions, actual=TestDF$variable)
t
