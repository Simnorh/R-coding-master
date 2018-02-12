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
#creates testriver dataframe consisting of all rows containing data from a given station 
#(any way to do this for all stations simultaneously?)
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


hist(resampled_data$daily_ams.1)


install.packages("devtools")
library(devtools)
install_github("NVE/fitdistrib")
install.packages('shiny')
library(shiny)
install.packages("glogis")
install.packages("tibble")
install.packages("rlang")
install_github("NVE/FlomKart_ShinyApp")
install_github("NVE/FlomKart")
install.packages("plotrix")
install_github("Simnorh/FlomKart")
install.packages('addtable2plot')
install.packages("reshape2")
install.packages ("haven")
install.packages("tidyverse")
library(tidyverse)
library (reshape2)
library(FlomKartShinyApp)
library(fitdistrib)
library(nsRFA)
library(FlomKart)
library(evd)
library(plotrix)



fittedtest <- f.GEV(x = resampled_data$daily_ams.1,
                    xi = param_estimate$estimate, 
                    alfa =param_estimate$estimate,
                    k = param_estimate$estimate)

A2_GOFlaio(resampled_data$daily_ams.1, dist="GEV")

setwd("D:/Master/Thesis/Data")

ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

#resampled_data <- ams_data[sample(nrow(ams_data)),]
#group_by (resampled_data, station)
testriver <- ams_data[ams_data$station == 2.25,]

set.seed(2661)
resampled_data <- testriver[sample(nrow(testriver)),]


param_estimate <- gev_Lmom (resampled_data$daily_ams.1)
#plot( resampled_data$daily_ams_dates, resampled_data$daily_ams.1, pch= 16, cex= 0.2)

Lmoments(resampled_data$daily_ams.1)

FlomKartShinyApp::plot4server(resampled_data$daily_ams.1, param = param_estimate$estimate, distr=3)
histo4param_values(distr = 3, param = param_estimate$estimate, method = "gev_Lmom")

goodnessoffittest <- gof_ad(resampled_data$daily_ams.1, 
       param = param_estimate$estimate, 
       distr = "gev",
       test.stat=TRUE,
       p.value=FALSE)

gof_test = data.frame(CS = NA, KS = NA, AD = goodnessoffittest)

plot_all (resampled_data$daily_ams.1,
          GOF.list = gof_test,
          param = param_estimate,
          distr = "gev",
         method = "ad")

plot_qq(resampled_data$daily_ams.1,
        param = param_estimate,
        distr = "gev")

param_estimate <- as.data.frame(param_estimate)

plot_rlevel(resampled_data$daily_ams.1,
            param = param_estimate,
            distr = "gev")

gum_param <- gev_Lmom(resampled_data$daily_ams.1)
FlomKartShinyApp::plot4server(resampled_data$daily_ams.1, param = param_estimate$estimate, distr=3)

gum_param <- as.data.frame(gum_param)

goodnessoffittest <- gof_ad(resampled_data$daily_ams.1, 
                            param = param_estimate$estimate, 
                            distr = "gev",
                            test.stat=TRUE,
                            p.value=FALSE)

param_estimate <- as.data.frame(param_estimate)

gof_kolm <- gof_ks(resampled_data$daily_ams.1,
                   param_estimate$estimate,
                   distr = "gev",
                   test.stat = TRUE,
                   p.value = FALSE)
gof_kolm

plot_ecdf(resampled_data$daily_ams.1,
          param_estimate,
          distr = "gev")
#plot density needs data as spesific type $
#param cant have spesified column

gp_Lmom <- function(dat) {
  
  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= 1) {
    
    dat.Lmom <- Lmoments(dat)
    
    
    fail_safe <- failwith(NULL, par.genpar)
    fitted.param <- fail_safe(dat.Lmom[1], dat.Lmom[2], dat.Lmom[4])
    
    if (is.null(fitted.param) == TRUE) {
      print("Warning: the function par.GEV failed in gev_Lmom")
      invisible(param)
    } else {
      #fitted.param <- as.numeric(par.GEV(dat.mom[1], dat.mom[2], dat.mom[4]))
      # Creating the returning list
      param$estimate <- c(fitted.param$xi, fitted.param$alfa, -fitted.param$k)
      # Standard error is not yet implemented
      invisible(param)
    }
  } else {
    print(paste("Warning: this station has less than ", 1," years of data. Use another method!",
                collapse = "", sep = ""))
    invisible(param)
  }
}


gofad <- function(dat, param, distr = "distr", test.stat = TRUE , p.value = FALSE) {
  
  AD <- NA
  fail_safe <- failwith(NA, goftest::ad.test)
  
  if (distr == 'gumbel') {
    temp <- fail_safe(dat, "pgumbel", param[1], param[2])
  }
  if (distr == 'gamma') {
    temp <- fail_safe(dat, "pgamma", param[1], rate = param[2])
  }
  if (distr == 'gev') {
    temp <- fail_safe(dat, "pgev", param[1], param[2], param[3])
  }
  if (distr == 'gl') {
    temp <- fail_safe(dat, "F.genlogis", param[1], param[2], param[3])
  }
  if (distr == 'gp') {
    temp <- fail_safe(dat, "F.genpar", param[1], param[2], param[3])
  }
  if (distr == 'pearson') {
    temp <- fail_safe(dat, "F.gamma", param[1], param[2], param[3])
  }
  
  if (p.value == TRUE && is.list(temp) == TRUE) {
    AD <- temp$p.value
  } else if (test.stat == TRUE && is.list(temp) == TRUE && is.numeric(temp$statistic) == TRUE && !is.infinite(temp$statistic) == TRUE) {
    AD <- temp$statistic
  }
 if (is.na(AD)) {print("Warning: gof_ad has failed with distr...")}
  invisible(AD)
}

plotdensity  <- function(dat, GOF.list, param, distr = "distr") {
  
  xmax <- max(dat)*1.2
  x <- seq(0, xmax, xmax / 100)
  
  ymax <- max(density(dat)$y)*1.2
  
  # Plotting input dat, this is common to all distributions
  hist(dat, xlab = "Flood discharge (m3/s)",ylab = "Probability density",freq = FALSE,
       breaks = seq(0, xmax, xmax / 15), col = "gray", main = NULL, xlim = c(0, xmax), ylim = c(0, ymax))
  par(new = TRUE)
  
  # Distribution specific y vector
  if(distr == 'gumbel')   y <- dgumbel(x, param$estimate[1], param$estimate[2])
  if(distr == 'gamma')    y <- dgamma(x, param$estimate[1], param$estimate[2])
  if(distr == 'gev')      y <- evd::dgev(x, param$estimate[1], param$estimate[2], param$estimate[3])  # I should have done that for most functions coming from packages...
  if(distr == 'gl')       y <- f.genlogis(x, param$estimate[1], param$estimate[2], param$estimate[3])
  if(distr == 'gp')       y <- f.genpar(x, param$estimate[1], param$estimate[2], param$estimate[3])
  if(distr == 'pearson')  y <- f.gamma(x, param$estimate[1], param$estimate[2], param$estimate[3])
  
  plot(x, y, xlim = c(0, xmax), ylim = c(0, ymax), type = "l", lwd = 2, col = "black", xlab = "", ylab = "")
  par(new = TRUE)
  plot(density(dat), main = "Density distribution and data histogramm",
       xlim = c(0, xmax), ylim = c(0, ymax), lty = 1, lwd = 3, col = "blue", xlab = "", ylab = "")
  
  legend("topright", inset = .05, c("Model","Empirical" ), col = c("black","blue"),lty = c(1, 1),lwd=c(2, 3),
         merge = TRUE, bg = "gray90")
}

plotrlevel <- function(dat, param, distr = "distr") {
  
  # Common to all distributions
  xmin <- min(dat)
  xmax <- max(dat)*1.5
  y <- seq(xmin, xmax, length = 100)
  empq <- sort(dat)
  
  # The x vector is distribution specific
  if(distr == 'gumbel') {
    x <- 1 / (1 - pgumbel(y, param$estimate[1], param$estimate[2]))
    # empT <- 1/(1-(seq(1:length(empq))-0.44)/(length(empq))+0.12) # Gringorten, optimized for the gumbel distribution
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr == 'gamma') {
    x <- 1 / (1 - pgamma(y, param$estimate[1], param$estimate[2]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr == 'gev')  {
    x <- 1 / (1 - evd::pgev(y, param$estimate[1], param$estimate[2], param$estimate[3]))
    # empT <- 1/(1-(seq(1:length(empq))-0.44)/(length(empq))+0.12) # Gringorten, optimized for the gumbel distribution
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr == 'gl')   {
    x <- 1 / (1 - F.genlogis(y, param$estimate[1], param$estimate[2], param$estimate[3]))
    # empT <- 1/(1-(seq(1:length(empq))-0.35)/(length(empq)))  # APL
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr=='gp') {
    x <- 1/(1-F.genpar(y, param$estimate[1], param$estimate[2], param$estimate[3]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr=='pearson') {
    x <- 1/(1-F.gamma(y, param$estimate[1], param$estimate[2], param$estimate[3]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  
  # xaxt="n" is to not plot the x axis ticks, as I specify them later
  plot(log(log(x)), y, xlim = c(0, log(log(1000))), xaxt = "n", ylim = c(0, xmax),
       main = "Return levels", xlab = "Return period (years)", ylab = "Flood discharge (m3/s)",type = "l",lwd = 2)
  tix <- c(5, 10, 20, 50, 100, 200, 500)
  axis(1, at = log(log(tix)), labels = tix)
  
  # plot empirical dat points
  points(log(log(empT)), empq, pch = 16, col = "blue")
  grid(nx = 7, ny = 10, lwd = 2) # grid only in y-direction
  
}

plotecdf  <- function(dat, param, distr = "distr") {
  
  xmax <- max(dat)*1.2
  x <- seq(0, xmax, xmax / 100)
  
  # Distribution specific y vector
  if(distr == 'gumbel') y <- pgumbel(x, param$estimate[1], param$estimate[2])
  if(distr == 'gamma')  y <- pgamma(x, param$estimate[1], param$estimate[2])
  if(distr == 'gev')    y <- evd::pgev(x, param$estimate[1], param$estimate[2], param$estimate[3])
  if(distr == 'gl')     y <- F.genlogis(x, param$estimate[1], param$estimate[2], param$estimate[3])
  if(distr == 'gp')     y <- F.genpar(x, param$estimate[1], param$estimate[2], param$estimate[3])
  if(distr == 'pearson') y <- F.gamma(x, param$estimate[1], param$estimate[2], param$estimate[3])
  
  
  plot(ecdf(dat), main = "ECDF", xlim = c(0, xmax), ylim = c(0, 1),
       xlab = "", ylab = "", lty = 21, col = "blue")
  par(new = TRUE)
  plot(x, y, xlim = c(0, xmax), ylim = c(0, 1),
       type = "l",lwd = 2, col = "black", xlab = "Flood discharge (m3/s)", ylab = "Cumulative probability")
}

plotqq  <- function(dat, param, distr = "distr") {
  
  # Compute plotting position
  # pvalues <-(seq(1:length(dat))-0.35)/length(dat) # APL
  p.values <- (seq(1:length(dat)) - 0.5) / length(dat)   # Hazen, a traditional choice
  y <- sort(dat)
  
  if(distr == 'gamma')  x <- sort(rgamma(p.values, param$estimate[1], param$estimate[2]))
  if(distr == 'gumbel') {
    # pvalues <- (seq(1:length(dat))-0.44)/(length(dat)+0.12) # Gringorten, optimized for the gumbel distribution
    x <- sort(rgumbel(p.values, param$estimate[1], param$estimate[2]))
  }
  if(distr == 'gev') {
    # pvalues <- (seq(1:length(dat))-0.44)/(length(dat)+0.12) # Gringorten, optimized for the gumbel distribution
    x <- sort(evd::rgev(p.values, param$estimate[1], param$estimate[2], param$estimate[3]))
  }
  if(distr == 'gl')     x <- invF.genlogis(p.values, param$estimate[1], param$estimate[2], param$estimate[3])
  
  if(distr == 'gp')     x <- invF.genpar(p.values, param$estimate[1], param$estimate[2], param$estimate[3])
  if(distr == 'pearson') x <- sort(rand.gamma(p.values, param$estimate[1], param$estimate[2], param$estimate[3]))
  
  plot(x, y, ylab = "Empirical flood dischare (m3/s)", xlab = "Modelled flood dischare (m3/s)",
       main = "Quantile-Quantile Plot",pch = 16, col = "blue")
  
  abline(0, 1, lwd = 2, col = "black")
}

plotall  <- function(dat, GOF.list, param, distr = "distr", method = "method") {
  
  
  windows()
  par(mfrow = c(2, 2))
  plotdensity(dat, GOF.list, param, as.character(distr))
  plotrlevel(dat, param, as.character(distr))
  
  # Add a table with the goodness of fit estimations
  nbs <- matrix(round(c(GOF.list$CS, GOF.list$KS, GOF.list$AD), 2), ncol = 3)
  rownames(nbs) <- c("Goodness of fit")
  colnames(nbs) <- c("CS", "KS", "AD")
  addtable2plot(0, 0, nbs, bty = "o", bg = "lightgray", display.rownames = TRUE, xpad = 0, ypad = 0)
  
  # Add a table with the fitting results
  if(distr == 'gumbel' | distr == 'gamma')  {
    nbs <- matrix(round(c(param$estimate[1], param$estimate[2], param$se[1], param$se[2]), 2), ncol = 2)
    rownames(nbs) <- c("Location", "Scale")
  } else {
    nbs <- matrix(round(c(param$estimate[1], param$estimate[2], param$estimate[3], param$se[1], param$se[2], param$se[3]), 2), ncol = 2)
    rownames(nbs) <- c("Location", "Scale", "Shape")
  }
  colnames(nbs) <- c("Estimate", "Std Err")
  xmax <- max(dat)
  addtable2plot(0, xmax, nbs, bty = "o", bg = "lightgray", display.rownames = TRUE, xpad = 0, ypad = 0) #150,570
  
  plotecdf(dat, param, as.character(distr))
  text(0, 1, paste("Distrib=", as.character(distr), "/ Method=", as.character(method)), cex = 1.2, adj = 0)
  plotqq(dat, param, as.character(distr))
}

ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

#resampled_data <- ams_data[sample(nrow(ams_data)),]
#group_by (resampled_data, station)
testriver <- ams_data[ams_data$station == 2.11,]

set.seed(2661)
resampled_data <- testriver[sample(nrow(testriver)),]


parest <- gp_Lmom(resampled_data$daily_ams.1)
parest <- as.data.frame(parest)
param_estimate <- gev_Lmom (resampled_data$daily_ams.1)



gofadtest <- gofad(resampled_data$daily_ams.1,
                   parest$estimate,
                   distr = "gp",
                   test.stat=TRUE,
                   p.value=FALSE)

goftest <- data.frame(CS = NA, KS = NA, AD = gofadtest)

plotall (resampled_data$daily_ams.1,
          GOF.list = goftest,
          param = parest,
          distr = "gp",
          method = "ad")


#Lese data:
mtt<-read.table('error_station.txt')

#beregne l-moment
ll <- Lmoments(pot_river[,5])

#Beregne parametre i GP-fordelingen
parameters <- par.genpar(ll[1],ll[2],ll[4])
parametersdf <- as.data.frame(parameters)
F_genpartest <- F.genpar(pot_river$flood.1, parameters$xi, parameters$alfa, parameters$k)
F_genpartestdf <- as.data.frame(F_genpartest)

#Kjøre AD-test
ad.test(mtt[,5], null = "F.genpar", parameters$xi, parameters$alfa, parameters$k)
