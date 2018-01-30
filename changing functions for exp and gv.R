exp_Lmom <- function(dat){
  
  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if(length(dat) >= 1){
    dat.mom <- Lmoments(dat)
    param$estimate <- invisible(as.numeric(par.exp(dat.mom[1], dat.mom[2])))
    invisible(param)
  } else {
    print(paste("Warning:this station has les than ",1,"years of data, use another method", collapse = "", sep = ""))
  }
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

#expLmom <- function(dat) {
  
  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= 1) {
    
    dat.Lmom <- Lmoments(dat)
    
    
    fail_safe <- failwith(NULL, par.exp)
    fitted.param <- fail_safe(dat.Lmom[1], dat.Lmom[2])
    
    if (is.null(fitted.param) == TRUE) {
      print("Warning: the function par.GEV failed in gev_Lmom")
      invisible(param)
    } else {
      #fitted.param <- as.numeric(Lmom.exp(dat.mom[1], dat.mom[2]))
      # Creating the returning list
      param$estimate <- c(fitted.param$xi, fitted.param$alfa)
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
  
  if (distr == 'exp') {
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

gof_ks <- function(dat, param, distr = "distr", test.stat = TRUE , p.value = FALSE) {
  
  KS <- NA
  fail_safe <- failwith(NA, stats::ks.test)
  
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

plotdensity  <- function(dat, GOF.list, param, distr = "distr") {
  
  xmax <- max(dat)*1.2
  x <- seq(0, xmax, xmax / 100)
  
  ymax <- max(density(dat)$y)*1.2
  
  # Plotting input dat, this is common to all distributions
  hist(dat, xlab = "Flood discharge (m3/s)",ylab = "Probability density",freq = FALSE,
       breaks = seq(0, xmax, xmax / 15), col = "gray", main = NULL, xlim = c(0, xmax), ylim = c(0, ymax))
  par(new = TRUE)
  
  # Distribution specific y vector
  if(distr == 'exp')   y <- f.exp(x, param$estimate[1], param$estimate[2])
  if(distr == 'gumbel')   y <- dgumbel(x, param$estimate[1], param$estimate[2])
  if(distr == 'gamma')    y <- dgamma(x, param$estimate[1], param$estimate[2])
  if(distr == 'gev')      y <- evd::dgev(x, param$estimate[1], param$estimate[2], param$estimate[3]) 
  # I should have done the above for most functions coming from packages...
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
  if(distr == 'exp') {
    x <- 1 / (1 - F.exp(y, param$estimate[1], param$estimate[2]))
    # empT <- 1/(1-(seq(1:length(empq))-0.44)/(length(empq))+0.12) # Gringorten, optimized for the gumbel distribution
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
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
  if(distr == 'exp')    y <- F.exp(x, param$estimate[1], param$estimate[2])
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
  
  if(distr == 'exp')  x <- sort(rand.exp(p.values, param$estimate[1], param$estimate[2]))
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
  if(distr == 'gumbel'| distr == 'gamma')  {
    nbs <- matrix(round(c(param$estimate[1], param$estimate[2], param$se[1], param$se[2]), 2), ncol = 2)
    rownames(nbs) <- c("Location", "Scale")
  }else if(distr == 'exp')  {
    nbs <- matrix(round(c(param$estimate[1], param$estimate[2], param$se[1], param$se[2]), 2), ncol = 2)
    rownames(nbs) <- c("Location", "Scale")
  }  
  else {
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

plot_rlevel <- function (dat, param, distr.index = 1){
  xmin <- min(dat)
  xmax <- max(dat) * 1.5
  y <- seq(xmin, xmax, length = 100)
  empq <- sort(dat)
  if (distr.index == 1) {
    x <- 1/(1 - pgumbel(y, param[1], param[2]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  if (distr.index == 2) {
    x <- 1/(1 - F.exp(y, param[1], param[2]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  if (distr.index == 3) {
    x <- 1/(1 - pgamma(y, param[1], param[2]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  if (distr.index == 4) {
    x <- 1/(1 - evd::pgev(y, param[1], param[2], param[3]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  if (distr.index == 5) {
    x <- 1/(1 - F.genpar(y, param[1], param[2], param[3]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  if (distr.index == 6) {
    x <- 1/(1 - F.genlogis(y, param[1], param[2], param[3]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  if (distr.index == 7) {
    x <- 1/(1 - nsRFA::F.gamma(y, param[1], param[2], param[3]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.5)/(length(empq)))
  }
  plot(log(log(x)), y, xlim = c(0, log(log(1000))), xaxt = "n", 
       ylim = c(0, xmax), main = "Return levels", xlab = "Return period (years)", 
       ylab = "Flood discharge (m3/s)", type = "l", lwd = 2)
  tix <- c(5, 10, 20, 50, 100, 200, 500)
  axis(1, at = log(log(tix)), labels = tix)
  points(log(log(empT)), empq, pch = 16, col = "blue")
  grid(nx = 7, ny = 10, lwd = 2)
}
stations <- as.data.frame(ams_data$station)
colnames(stations)[1] <- "Stations"
stations <- stations %>% add_count(Stations)
colnames(stations)[2] <- "years"
stations <- dplyr::distinct(stations)

testplotframe <- as.data.frame(testriver$station)
colnames(testplotframe)[1] <- "Stations"
testplotframe <- testplotframe %>% add_count(Stations)
colnames(testplotframe)[2] <- "years"
testplotframe <- dplyr::distinct(testplotframe)

ad_values_gev <- data.frame(matrix(ncol = 3, nrow = 0))
cnames <- c("Stations", "ADscore", "Years")
colnames(ad_values_gev) <- cnames

pot_data <-read.table("pot_and_fgp.txt",header=T,sep="\ ")
ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")

#resampled_data <- ams_data[sample(nrow(ams_data)),]
#group_by (resampled_data, station)
testriver <- ams_data[ams_data$station == "2.607", ]

testplotframe <- as.data.frame(testriver$station)
colnames(testplotframe)[1] <- "Stations"
testplotframe <- testplotframe %>% add_count(Stations)
colnames(testplotframe)[2] <- "years"
testplotframe <- dplyr::distinct(testplotframe)

set.seed(2661)
resampled_data <- testriver[sample(nrow(testriver)),]
#parest <- gp_Lmom(resampled_data$flood.1)
parest2 <- gev_Lmom(testriver$daily_ams.1)
#parest <- as.data.frame(parest)
#param_estimate <- gev_Lmom (resampled_data$daily_ams.1)

gofadtest <- gofad(testriver$daily_ams.1,
                   parest2$estimate,
                   distr = "gev",
                   test.stat=TRUE,
                   p.value=FALSE)
#edit(goftest::ad.test)
gofkstest <- gof_ks(testriver$daily_ams.1,
                    parest2$estimate,
                    distr = "gev",
                    test.stat = TRUE,
                    p.value = FALSE)
#gofcstest <- gof_cs(resampled_data$flood.1,
 #                   parest$estimate,
  #                  distr = "gp")

goftest <- data.frame(CS = NA, KS = gofkstest, AD = gofadtest)

#plotall (testriver$daily_ams.1,
#         GOF.list = goftest,
#         param = parest2,
#         distr = "gev",
#         method = "ad")
#plot_rlevel(testriver$flood.1, parest2$estimate, distr.index = 5)#useless, already in pot all function

newRow <- data.frame(Station = testplotframe$Stations, ADscore = gofadtest,Years = testplotframe$years)
ad_values_gev <- rbind(newRow, ad_values_gev)

#plot(ad_values_gev$Years, ad_values_gev$ADscore)
