gp_Lmom <- function(dat, threshold = NA) {
  
  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= 1) {
    if (is.na(threshold)){
      
      
      dat.Lmom <- Lmoments(dat)
      
      fail_safe <- failwith(NULL, par.genpar)
      fitted.param <- fail_safe(dat.Lmom[1], dat.Lmom[2], dat.Lmom[4])
      
      if (is.null(fitted.param) == TRUE) {
        print("Warning: the function par.genpar failed in gp_Lmom")
        invisible(param)
      } else {
        # Creating the returning list
        param$estimate <- c(fitted.param$xi, fitted.param$alfa, fitted.param$k)
        # Standard error is not yet implemented
        invisible(param)
      }
    }else {
      #put as a failsafe, the parameter estimation above sometimes gave location parameter lower than threshold
      dat2<-dat-threshold
      dat.Lmom <- Lmoments(dat2)
      t2 = dat.Lmom[2] / dat.Lmom[1]
      param$estimate[1] <- threshold
      param$estimate[3] <- 2-1/t2
      param$estimate[2] <- dat.Lmom[1]*(1/t2-1)
      invisible(param)
    }
  }
  
  else {
    print(paste("Warning: this station has less than ", 1," years of data. Use another method!",
                collapse = "", sep = ""))
    invisible(param)
  }
}

gofad <- function(dat, param, distr = "distr", test.stat = TRUE , p.value = FALSE) {
  
  AD <- NA 
  fail_safe <- failwith(NA, goftest::ad.test)
  
  if (distr == 'exp') {
    dat <- dat[dat > param[1]]
    temp <- fail_safe(dat, "F.exp", param[1], param[2])
  }
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
    dat <- dat[dat > param[1]]
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

pot_data <-read.table("pot_and_fgp.txt",header=T,sep="\ ")
ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")


pot_years <- str_split_fixed(pot_data$date, "-", 3)
cnames_years <- c("year", "month", "date")
colnames(pot_years) <- cnames_years
pot_years <- subset(pot_years, select =c(year))
pot_data <- cbind(pot_data, pot_years)

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")
pot_data$station <- paste(pot_data$regine, pot_data$main, sep=".")

stations <- as.data.frame(ams_data$station)
colnames(stations)[1] <- "Stations"
stations <- stations %>% add_count(Stations)
colnames(stations)[2] <- "years"
stations <- dplyr::distinct(stations)

ams_river <- ams_data[ams_data$station == "2.32", ]
pot_river <- pot_data[pot_data$station == "2.32", ]

testplotframe <- as.data.frame(ams_river$station)
colnames(testplotframe)[1] <- "Stations"
testplotframe <- testplotframe %>% add_count(Stations)
colnames(testplotframe)[2] <- "years"
testplotframe <- dplyr::distinct(testplotframe)
Floods_year <- data.frame(nrow(pot_river)/testplotframe$years)
testplotframe <- cbind(testplotframe, Floods_year)
colnames(testplotframe)[3] <- "Floods per year"

porivergb <- dplyr::group_by(pot_river, year)
set.seed(1233)
potriversample <- sample(pot_river$year, size = length(pot_river$threshold), replace = FALSE)
potriversampledf <- as.data.frame(potriversample)
potriversampledf <- dplyr::distinct(potriversampledf)

set.seed(5121)
sam_pot_river <- pot_river[sample(nrow(pot_river)),]
set.seed(9871)
sam_ams_river <- ams_river[sample(nrow(ams_river)),]

kfold = 10
dimriv <- dim(potriversampledf)[1]/kfold
dimrivams <- dim(sam_ams_river)[1]/kfold
gs <- floor(dimriv)
gs_ams <- floor(dimrivams)

goftestcv = NULL
cv_pvalue = NULL

for (i in 1:kfold){
  i1 = (i-1)*gs + 1
  i2 = i*gs
  testdf <- sam_pot_river[sam_pot_river[,7] %in% potriversample[i1:i2], 5]
  traindf<- sam_pot_river[!(sam_pot_river[,7] %in% potriversample[i1:i2]), 5] 
  paramsgp <- gp_Lmom(traindf, threshold = sam_pot_river$threshold[2])
  goftestcv[i] <- gofad(testdf, paramsgp$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gofad(testdf, paramsgp$estimate, distr = "gp", test.stat=FALSE, p.value=TRUE)
  cv_average_pvalue <- mean(cv_pvalue)
}

for (i in 1:kfold){
  i1 = (i-1)*gs_ams + 1
  i2 = i*gs_ams
  testdf <- sam_ams_river[(i1:i2), 5]
  traindf<- sam_ams_river[-(i1:i2), 5] 
  paramsgp <- gumbel_Lmom(traindf)
  goftestcv[i] <- gofad(testdf, paramsgp$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
  cv_average <- mean(goftestcv)
  cv_pvalue[i] <- gofad(testdf, paramsgp$estimate, distr = "gumbel", test.stat=FALSE, p.value=TRUE)
  cv_pvalue_av <- mean(cv_pvalue)
} 
