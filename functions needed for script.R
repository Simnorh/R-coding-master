gofad <- function(dat, param, distr = "distr", test.stat = TRUE , p.value = FALSE) {
  
  AD <- NA 
  fail_safe <- purrr::possibly(goftest::ad.test, NA)
  
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

gofks <- function(dat, param, distr = "distr", test.stat = TRUE , p.value = FALSE) {
  
  KS <- NA
  fail_safe <- purrr::possibly(stats::ks.test, NA)
  
  if (distr == 'gumbel') {
    temp <- fail_safe(dat, "pgumbel", param[1], param[2])
  }
  if (distr == 'exp') {
    temp <- fail_safe(dat, "F.exp", param[1], param[2])
  }
  if (distr == 'gamma') {
    temp <- fail_safe(dat, "pgamma", param[1], rate = param[2])
  }
  if (distr == 'gp') {
    temp <- fail_safe(dat, "F.genpar", param[1], param[2], param[3])
  }
  if (distr == 'gev') {
    temp <- fail_safe(dat, "pgev", param[1], param[2], param[3])
  }
  if (distr == 'gl') {
    temp <- fail_safe(dat, "F.genlogis", param[1], param[2], param[3])
  }
  if (distr == 'pearson') {
    temp <- fail_safe(dat, "F.gamma", param[1], param[2], param[3])
  }
  
  if (p.value == TRUE && is.list(temp) == TRUE) {
    KS <- temp$p.value
  }
  else if (test.stat == TRUE && is.list(temp) == TRUE && is.numeric(temp$statistic) == TRUE && !is.infinite(temp$statistic) == TRUE) {
    KS <- temp$statistic
  }
  # We could add if (is.na(KS)) {print("Warning: gof_ks has failed with distr...)}
  invisible(KS)
}

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

CoV <- function(dat){
  SD <- sd(dat)
  x <- mean(dat)
  CV = SD/x
  return(CV)
}

exp_Lmom <- function(dat, threshold = NA){
  
  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if(length(dat) >= 1){
    if(is.na(threshold)){
      dat.mom <- Lmoments(dat)
      param$estimate <- invisible(as.numeric(par.exp(dat.mom[1], dat.mom[2])))
      invisible(param)
    } else{
      dat2 <- dat-threshold
      dat.Lmom <- Lmoments(dat2)
      param$estimate[1] <- threshold
      param$estimate[2] <- 2*dat.Lmom[2]
      invisible(param)
    }
  }
  else {
    print(paste("Warning:this station has les than ",1,"years of data, use another method", collapse = "", sep = ""))
  }
}

gp_Lmom1 <- function(dat, threshold = NA) {
  
  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= 1) {
    if (is.na(threshold)){
      
      
      dat.Lmom <- Lmoments(dat)
      
      fail_safe <- failwith(NA, par.genpar)
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

library(FlomKart)
library(fitdistrib)
library(tidyr)
library(stringr)
library(dplyr)
library(nsRFA)
library(evd)
library("leaflet")
install.packages("vioplot")
library(shiny)
library(vioplot)
