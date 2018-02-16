setwd("D:/Master/Thesis/Data")

ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

testriver <- ams_data[ams_data$station == 2.11,]

resampled_data <- testriver[sample(nrow(testriver)),]

library(caret)

set.seed(2334)
data_folds <- cut(seq(1, nrow(resampled_data)), breaks = 10, labels=FALSE)

set.seed(2000)
trainindex <- createDataPartition(resampled_data$daily_ams.1, p=8/10, list=FALSE)
trainset <- resampled_data[trainindex,]
testset <- resampled_data[-trainindex,]





set.seed(196)
cv10fold <- createFolds(pot_river$year, k = 10)
str(cv10fold)

control1 <- trainControl(method = "cv", number = 10, index = cv10fold)

#grid <- expand.grid(.fL=c(0), .usekernerl = c(FALSE))

model1 <- train(daily_ams.1~fine_ams + FGP + daily_ams.1,
                data = resampled_data,
                method = "lm",
                trControl= control1,
                na.action = na.exclude)

model1

set.seed (928)
traincon <- trainControl(method = "cv", number = 10, index = trainindex)

grid <- expand.grid(.fL=c(0), .usekernerl = c(FALSE))

model2 <- train(daily_ams.1~fine_ams + FGP + daily_ams.1,
                data = trainset,
                method = "rf",
                trControl= traincon,
                na.action = na.exclude,
                ntree = 10)

model2



