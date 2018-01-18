setwd("D:/Master/Thesis/Data")

ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

testriver <- ams_data[ams_data$station == 2.11,]

resampled_data <- testriver[sample(nrow(testriver)),]

library(caret)

set.seed(2334)
data_folds <- cut(seq(1, nrow(resampled_data)), breaks = 10, labels=FALSE)

