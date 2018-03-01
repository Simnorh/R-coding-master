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

install.packages("roxygen2")
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
install.packages("stringr")
library(stringr)


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

BS4NC_tryout <- function(dat, threshold, param, distr = "distr") {
  
  BS <- array(NA, dim = 6)
  modelled.prob <- array(NA, dim = 6)
  empirical <- array(NA, dim = 6)
  Pu <- array(NA, dim = 6)
  
  #   m <- max(dat.full) * 0.8
  #   threshold <- seq(m / 6, m, m / 6)
  
  if(distr == 'exp') {
    modelled.prob <- F.exp(threshold, param[1], param[2])
  }
  if(distr == 'gumbel') {
    modelled.prob <- pgumbel(threshold, param[1], param[2])  # Could be protected with "failwith"
  }
  if(distr == 'gamma') {
    modelled.prob <- pgamma(threshold, param[1], rate = param[2])
  }
  if(distr == 'gev') {
    modelled.prob <- evd::pgev(threshold, param[1], param[2], param[3])
  }
  if(distr == 'gl') {
    modelled.prob <- F.genlogis(threshold, param[1], param[2], param[3])
  }
  if(distr == 'pearson') {
    modelled.prob <- F.gamma(threshold, param[1], param[2], param[3])
  }
  if(distr == 'gp') {
    modelled.prob <- F.genpar(threshold, param[1], param[2], param[3])
  }
  
  for (z in 1:6) {
    binary_vector <- as.integer(dat >= threshold[z]) # / length(dat) # * 30 otherwise, very small values
    Pu[z] <- (1 - modelled.prob[z])
    BS[z] <- mean( (Pu[z] - binary_vector)^2 )
  }
  invisible(BS)
  
}

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

pot_data <-read.table("pot_and_fgp.txt",header=T,sep="\ ")
ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")
pot_data$station <- paste(pot_data$regine, pot_data$main, sep=".")

ams_river <- ams_data[ams_data$station == "2.32", ]
pot_river <- pot_data[pot_data$station == "2.32", ]

pot_years <- str_split_fixed(pot_data$date, "-", 3)
cnames_years <- c("year", "month", "date")
colnames(pot_years) <- cnames_years
pot_years <- subset(pot_years, select =c(year))
pot_data <- cbind(pot_data, pot_years)

porivergb <- dplyr::group_by(pot_river, year)
set.seed(1233)
potriversample <- sample(pot_river$year, size = length(pot_river$threshold), replace = FALSE)
potriversampledf <- as.data.frame(potriversample)
potriversampledf <- dplyr::distinct(potriversampledf)

n <- 10
split(pot_river$year, sample(1:n, pot_river, nrow(pot_river), replace = FALSE, prob = NULL))

set.seed(5121)
sam_pot_river <- pot_river[sample(nrow(pot_river)),]
set.seed(9871)
sam_ams_river <- ams_river[sample(nrow(ams_river)),]

kfold = 10
#dimriv <- dim(potriversampledf)[1]/kfold
dimrivams <- dim(sam_ams_river)[1]/kfold
#gs <- ceiling(dimriv)
gs_ams <- floor(dimrivams)
gs_ams

goftestcv = NULL
cv_pvalue = NULL

#for (i in 1:10){
  i1 = (i-1)*gs + 1
  i2 = i*gs
  testdf <- sam_pot_river[sam_pot_river[,7] %in% potriversample[i1:i2], 5]
  traindf<- sam_pot_river[!(sam_pot_river[,7] %in% potriversample[i1:i2]), 5] 
  paramsgp <- gp_Lmom(traindf)
  goftestcv[i] <- gofad(testdf, paramsgp$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gofad(testdf, paramsgp$estimate, distr = "gp", test.stat=FALSE, p.value=TRUE)
  cv_average <- mean(cv_pvalue)
}
 
for (i in 1:kfold){
  i1 = (i-1)*gs_ams + 1
  i2 = i*gs_ams
  testdf <- sam_ams_river[(i1:i2), 5]
  traindf<- sam_ams_river[-(i1:i2), 5] 
  paramsgp <- gumbel_Lmom(traindf)
  goftestcv[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gumbel", test.stat=FALSE, p.value=TRUE)
  cv_pvalue_av <- mean(cv_pvalue)
} #gumbel

bn <- as.vector(pot_river$year)
bsize <- as.numeric(table(bn))
bsizevec <- as.data.frame(bsize)
ub <- unique(bn)
blockdf <- cbind(ub, bsizevec)
set.seed(145)
rbn <- blockdf[sample(nrow(blockdf)),]
gsize <- floor(length(bn)/kfold)
bcum = cumsum(rbn$bsize)


for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
  paramsgp <- gp_Lmom(traindf)
  goftestcv[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gp", test.stat=FALSE, p.value=TRUE)
  cv_pvalue_av <- mean(cv_pvalue)
} #gp








bsf <- BS4NC(ams_river$daily_ams.1, threshold = return_gum, param = gumpar_ams$estimate, distr = "gum")
bsf
return_p<- list(Periods = c(5, 10, 20))
return_exp = NULL
return_gum = NULL
return_gp = NULL
return_gev = NULL

for (i in 1:3){
return_gum[i] = gumpar_ams$estimate[1] + gumpar_ams$estimate[2]*(log(1-(1/return_p$Periods[i])))
  
return_gp[i] = (gppar_pot$estimate[2]/gppar_pot$estimate[3])*(1-(-log(1-(1/return_p$Periods[i])/testplotframe$`Floods per year`))^gppar_pot$estimate[3])+pot_river$threshold[2]

return_gev[i] = gevpar_ams$estimate[1] + gevpar_ams$estimate[2]/gevpar_ams$estimate[3]*(1-(-log(1-(1/return_p$Periods[i])))^gevpar_ams$estimate[3])

return_exp[i] = exppar_pot$estimate[1]- exppar_pot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[i]))))
}
return_gum
return_gp
return_gev
return_exp

quantdf <- list(Quantiles = c(5, 10, 20))
quantdf <- data.frame(matrix(ncol = 1, c(0.8, 0.9, 0.95)))
quant_name <- c("Quantiles")
colnames(quantdf) <- quant_name

#cummulative gp
quantlist = NULL
for(i in 1:3){
    quantlist[i] = exp(-(1-gppar_pot$estimate[3]*
                           ((quantdf$Quantiles[i]-(gppar_pot$estimate[2]/gppar_pot$estimate[3])*
                               (1-(1/((testplotframe$`Floods per year`)^gppar_pot$estimate[3]))))/
                              (gppar_pot$estimate[2]/((testplotframe$`Floods per year`)^gppar_pot$estimate[3]))))
                       ^(1/(gppar_pot$estimate[3])))
}
quantlistgp = NULL
quantlistexp = NULL
for(i in 1:3){
  quantlistgp[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-gppar_pot$estimate[3]*((return_gp[i]-gppar_pot$estimate[1])/gppar_pot$estimate[2]))^(1/gppar_pot$estimate[3]))))
  quantlistexp[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp[i]-exppar_pot$estimate[1])/exppar_pot$estimate[2])))))
  #quantlistgpaa[i] = exp(-(testplotframe$`Floods per year`)*(1- gppar_pot$estimate[3]*(return_gp[i]/(gppar_pot$estimate[2])))^(1/gppar_pot$estimate[3]))
  #quantlistexp[i] = exp(-(testplotframe$`Floods per year`)*(1- (1-exp(-((return_exp[i] - exppar_pot$estimate[1])/exppar_pot$estimate[2])))))
}
quantlistgp
quantlistexp
  #cummulative exp
quantlistexp = NULL
for(i in 1:3){
  quantlistexp[i] = exp(-(-((quantdf$Quantiles[i]-exppar_pot$estimate[1])/exppar_pot$estimate)))
}
#
##
###
####
#####Quantile and brier score attempt
####
###
##
#
set.seed(5121)
sam_pot_river <- pot_river[sample(nrow(pot_river)),]
set.seed(9871)
sam_ams_river <- ams_river[sample(nrow(ams_river)),]

kfold = 10
#dimriv <- dim(potriversampledf)[1]/kfold
dimrivams <- dim(sam_ams_river)[1]/kfold
#gs <- ceiling(dimriv)
gs_ams <- floor(dimrivams)
gs_ams

goftestcv = NULL
cv_pvalue = NULL

bn <- as.vector(pot_river$year)
bsize <- as.numeric(table(bn))
bsizevec <- as.data.frame(bsize)
ub <- unique(bn)
blockdf <- cbind(ub, bsizevec)
set.seed(145)
rbn <- blockdf[sample(nrow(blockdf)),]
gsize <- floor(length(bn)/kfold)
bcum = cumsum(rbn$bsize)

return_gp_5 = NULL
return_gp_10= NULL
return_gp_20 = NULL
quantlistgp_5 = NULL
quantlistgp_10 = NULL
quantlistgp_20 = NULL
bsgp_5 = NULL
bsgp_10 = NULL
bsgp_20 = NULL
qsgp_5 = NULL
qsgp_10 = NULL
qsgp_20 = NULL
return_exp_5 = NULL
return_exp_10= NULL
return_exp_20 = NULL
quantlistexp_5 = NULL
quantlistexp_10 = NULL
quantlistexp_20 = NULL
bsexp_5 = NULL
bsexp_10 = NULL
bsexp_20 = NULL
qsexp_5 = NULL
qsexp_10 = NULL
qsexp_20 = NULL
return_gev_5 = NULL
return_gev_10= NULL
return_gev_20 = NULL
bsgev_5 = NULL
bsgev_10 = NULL
bsgev_20 = NULL
qsgev_5 = NULL
qsgev_10 = NULL
qsgev_20 = NULL

for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
  paramsgp <- gp_Lmom(traindf)
  goftestcv[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gp", test.stat=FALSE, p.value=TRUE)
  cv_pvalue_av <- mean(cv_pvalue)
  
  return_gp_5[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[1])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
  quantlistgp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_5-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
  
  return_gp_10[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[2])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
  quantlistgp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_10-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
  
  return_gp_20[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[3])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
  quantlistgp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_20-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
  
  returngp5_av <- mean(return_gp_5)
  returngp10_av <- mean(return_gp_10)
  returngp20_av <- mean(return_gp_20)
  quantlistgp5_av <- mean(quantlistgp_5)
  quantlistgp10_av <- mean(quantlistgp_10)
  quantlistgp20_av <- mean(quantlistgp_20)
  
  bsgp_5[i] <- BS4NC_tryout(testdf, threshold = return_gp_5[i], param = paramsgp$estimate, distr = "gp")
  bsgp_10[i] <- BS4NC_tryout(testdf, threshold = return_gp_10[i], param = paramsgp$estimate, distr = "gp")
  bsgp_20[i] <- BS4NC_tryout(testdf, threshold = return_gp_20[i], param = paramsgp$estimate, distr = "gp")
  
  qsgp_5[i] <- QS4NC(testdf, r.levels = return_gp_5[i], r.periods = return_p$Periods[1])
  qsgp_10[i] <- QS4NC(testdf, r.levels = return_gp_10[i], r.periods = return_p$Periods[2])
  qsgp_20[i] <- QS4NC(testdf, r.levels = return_gp_20[i], r.periods = return_p$Periods[3])
  
  qsgp5_av <- mean(qsgp_5)
  qsgp10_av <- mean(qsgp_10)
  qsgp20_av <- mean(qsgp_20)
  bsgp5_av <- mean(bsgp_5)
  bsgp10_av <- mean(bsgp_10)
  bsgp20_av <- mean(bsgp_20)
}#gp
bsgp5_av
bsgp10_av
bsgp20_av
qsgp5_av
qsgp10_av
qsgp20_av

for (i in 1:kfold){
  i1 = (i-1)*gs_ams + 1
  i2 = i*gs_ams
  testdf <- sam_ams_river[(i1:i2), 5]
  traindf<- sam_ams_river[-(i1:i2), 5] 
  paramsgev <- gev_Lmom(traindf)
  goftestcv[i] <- gof_ad(testdf, paramsgev$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gof_ad(testdf, paramsgev$estimate, distr = "gev", test.stat=FALSE, p.value=TRUE)
  cv_pvalue_av <- mean(cv_pvalue)

  return_gev_5[i] = paramsgev$estimate[1] + paramsgev$estimate[2]/paramsgev$estimate[3]*(1-(-log(1-(1/return_p$Periods[1])))^paramsgev$estimate[3])
  #quantlistgp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_5-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
  
  return_gev_10[i] = paramsgev$estimate[1] + paramsgev$estimate[2]/paramsgev$estimate[3]*(1-(-log(1-(1/return_p$Periods[2])))^paramsgev$estimate[3])
  #quantlistgp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_10-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
  
  return_gev_20[i] = paramsgev$estimate[1] + paramsgev$estimate[2]/paramsgev$estimate[3]*(1-(-log(1-(1/return_p$Periods[3])))^paramsgev$estimate[3])
  #quantlistgp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_20-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
  
  returngev5_av <- mean(return_gev_5)
  returngev10_av <- mean(return_gev_10)
  returngev20_av <- mean(return_gev_20)
  #quantlistgp5_av <- mean(quantlistgp_5)
  #quantlistgp10_av <- mean(quantlistgp_10)
  #quantlistgp20_av <- mean(quantlistgp_20)
  
  bsgev_5[i] <- BS4NC_tryout(testdf, threshold = return_gev_5[i], param = paramsgev$estimate, distr = "gev")
  bsgev_10[i] <- BS4NC_tryout(testdf, threshold = return_gev_10[i], param = paramsgev$estimate, distr = "gev")
  bsgev_20[i] <- BS4NC_tryout(testdf, threshold = return_gev_20[i], param = paramsgev$estimate, distr = "gev")
  
  qsgev_5[i] <- QS4NC(testdf, r.levels = return_gev_5[i], r.periods = return_p$Periods[1])
  qsgev_10[i] <- QS4NC(testdf, r.levels = return_gev_10[i], r.periods = return_p$Periods[2])
  qsgev_20[i] <- QS4NC(testdf, r.levels = return_gev_20[i], r.periods = return_p$Periods[3])
  
  qsgev5_av <- mean(qsgev_5)
  qsgev10_av <- mean(qsgev_10)
  qsgev20_av <- mean(qsgev_20)
  bsgev5_av <- mean(bsgev_5)
  bsgev10_av <- mean(bsgev_10)
  bsgev20_av <- mean(bsgev_20)
}#gev
bsgev5_av
bsgev10_av
bsgev20_av
qsgev5_av
qsgev10_av
qsgev20_av

for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
  paramsexp <- exp_Lmom(traindf)
  goftestcv[i] <- gof_ad(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gof_ad(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
  cv_pvalue_av <- mean(cv_pvalue)
  
  return_exp_5[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[1]))))
  quantlistexp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp_5[i]-paramsexp$estimate[1])/paramsexp$estimate[2])))))
  
  return_exp_10[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[2])))) 
  quantlistexp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp_10[i]-paramsexp$estimate[1])/paramsexp$estimate[2])))))
  
  return_exp_20[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[3]))))
  quantlistexp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp_20[i]-paramsexp$estimate[1])/paramsexp$estimate[2])))))
  
  returnexp5_av <- mean(return_exp_5)
  returnexp10_av <- mean(return_exp_10)
  returnexp20_av <- mean(return_exp_20)
  quantlistexp5_av <- mean(quantlistexp_5)
  quantlistexp10_av <- mean(quantlistexp_10)
  quantlistexp20_av <- mean(quantlistexp_20)
  
  bsexp_5[i] <- BS4NC_tryout(testdf, threshold = return_exp_5[i], param = paramsexp$estimate, distr = "exp")
  bsexp_10[i] <- BS4NC_tryout(testdf, threshold = return_exp_10[i], param = paramsexp$estimate, distr = "exp")
  bsexp_20[i] <- BS4NC_tryout(testdf, threshold = return_exp_20[i], param = paramsexp$estimate, distr = "exp")
  
  qsexp_5[i] <- QS4NC(testdf, r.levels = return_exp_5[i], r.periods = return_p$Periods[1])
  qsexp_10[i] <- QS4NC(testdf, r.levels = return_exp_10[i], r.periods = return_p$Periods[2])
  qsexp_20[i] <- QS4NC(testdf, r.levels = return_exp_20[i], r.periods = return_p$Periods[3])
  
  qsexp5_av <- mean(qsexp_5)
  qsexp10_av <- mean(qsexp_10)
  qsexp20_av <- mean(qsexp_20)
  bsexp5_av <- mean(bsexp_5)
  bsexp10_av <- mean(bsexp_10)
  bsexp20_av <- mean(bsexp_20)
}#exp
bsexp5_av
bsexp10_av
bsexp20_av
qsexp5_av
qsexp10_av
qsexp20_av
