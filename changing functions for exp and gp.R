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

gofad <- function(dat, param, distr = "distr", test.stat = TRUE , p.value = FALSE) {
  
  AD <- NA 
  fail_safe <- purrr::possibly(goftest::ad.test, NA)
  
  if (distr == 'exp') {
    temp <- fail_safe(dat, "F.exp", param[2])
  }
  if (distr == 'exp2') {
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
  if (distr == 'gp2') {
    temp <- fail_safe(dat, "F.genpar", param[2], param[3])
  }
  if (distr == 'gp3') {
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
    temp <- fail_safe(dat, "F.exp", param[2])
  }
  if (distr == 'exp2') {
    temp <- fail_safe(dat, "F.exp", param[1], param[2])
  }
  if (distr == 'gamma') {
    temp <- fail_safe(dat, "pgamma", param[1], rate = param[2])
  }
  if (distr == 'gp2') {
    temp <- fail_safe(dat, "F.genpar", param[2], param[3])
  }
  if (distr == 'gp3') {
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

#testplotframe <- as.data.frame(testriver$station)
#colnames(testplotframe)[1] <- "Stations"
#testplotframe <- testplotframe %>% add_count(Stations)
#colnames(testplotframe)[2] <- "years"
#testplotframe <- dplyr::distinct(testplotframe)

#ad_values_gev <- data.frame(matrix(ncol = 3, nrow = 0))
#cnames <- c("Stations", "ADscore", "Years")
#colnames(ad_values_gev) <- cnames

pot_data <-read.table("pot_and_fgp.txt",header=T,sep="\ ")
ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")

pot_data$station <- paste(pot_data$regine, pot_data$main, sep=".")

#resampled_data <- ams_data[sample(nrow(ams_data)),]
#group_by (resampled_data, station)
testriver <- pot_data[pot_data$station == "2.11",]

#testplotframe <- as.data.frame(testriver$station)
#colnames(testplotframe)[1] <- "Stations"
#testplotframe <- testplotframe %>% add_count(Stations)
#colnames(testplotframe)[2] <- "years"
#testplotframe <- dplyr::distinct(testplotframe)

set.seed(2661)
resampled_data <- testriver[sample(nrow(testriver)),]
parest <- gp_Lmom(testriver$flood.1, testriver$threshold[2])
#parest2 <- gev_Lmom(testriver$daily_ams.1)
parestdf <- as.data.frame(parest)
#param_estimate <- gev_Lmom (resampled_data$daily_ams.1)
F_genpartest <- F.exp(testriver$flood.1, parestdf$estimate[1], parestdf$estimate[2])
F_genpartestdf <- as.data.frame(F_genpartest)
gofadtest <- gofad(testriver$flood.1,
                   parest$estimate,
                   distr = "exp",
                   test.stat=TRUE,
                   p.value=FALSE)
#edit(goftest::ad.test)
gofkstest <- gof_ks(testriver$flood.1,
                    parest$estimate,
                    distr = "exp",
                    test.stat = TRUE,
                    p.value = FALSE)
#gofcstest <- gof_cs(resampled_data$flood.1,
 #                   parest$estimate,
  #                  distr = "gp")

goftest <- data.frame(CS = NA, KS = gofkstest, AD = gofadtest)

plotall (testriver$flood.1,
         GOF.list = goftest,
         param = parest,
         distr = "exp",
         method = "ad")
#plot_rlevel(testriver$flood.1, parest2$estimate, distr.index = 5)#useless, already in pot all function

#newRow <- data.frame(Station = testplotframe$Stations, ADscore = gofadtest,Years = testplotframe$years)
#ad_values_gev <- rbind(newRow, ad_values_gev)
write.table (testriver, file = "error_station.txt", sep = "\ ")

#plot(ad_values_gev$Years, ad_values_gev$ADscore)

roundtry <- function(x, base){
  base*round(x/base)
}#works good rounds 0.75-1.25 to 1 and so on

gum_values[,"chunk"] <- NA
for(i in 1:length(gum_values$Floods.per.year)){
  gum_values$chunk[i] <- roundtry(gum_values$Floods.per.year[i], base = 0.5)
}

chunkmeangum <- NULL
chunks <- as.vector(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5))
for(i in 1:length(chunks)){
  chunktestgum <- gum_values[gum_values[,18] %in% chunks[i], 13]
  chunkmeangum[i] <- mean(chunktestgum)
}
chunkgum <- data.frame(chunkmeangum, chunks)
####
gev_values[,"chunk"] <- NA
for(i in 1:length(gev_values$Floods.per.year)){
  gev_values$chunk[i] <- roundtry(gev_values$Floods.per.year[i], base = 0.5)
}

chunkmeangev <- NULL
for(i in 1:length(chunks)){
  chunktestgev <- gev_values[gev_values[,18] %in% chunks[i], 13]
  chunkmeangev[i] <- mean(chunktestgev)
}
chunkgev <- data.frame(chunkmeangev, chunks)
####
gp_values[,"chunk"] <- NA
for(i in 1:length(gp_values$Floods.per.year)){
  gp_values$chunk[i] <- roundtry(gp_values$Floods.per.year[i], base = 0.5)
}

chunkmeangp <- NULL
for(i in 1:length(chunks)){
  chunktestgp <- gp_values[gp_values[,18] %in% chunks[i], 13]
  chunkmeangp[i] <- mean(chunktestgp)
}
chunkgp <- data.frame(chunkmeangp, chunks)
####
exp_values[,"chunk"] <- NA
for(i in 1:length(exp_values$Floods.per.year)){
  exp_values$chunk[i] <- roundtry(exp_values$Floods.per.year[i], base = 0.5)
}

chunkmeanexp <- NULL
for(i in 1:length(chunks)){
  chunktestexp <- exp_values[exp_values[,18] %in% chunks[i], 13]
  chunkmeanexp[i] <- mean(chunktestexp)
}
chunkexp <- data.frame(chunkmeanexp, chunks)

ggplot()+
  geom_smooth(data=chunkgev, aes(x=chunkgev$chunks, y= chunkgev$chunkmean), col="darkred", se=F)+
  geom_smooth(data=chunkgum, aes(x=chunkgum$chunks, y= chunkgum$chunkmean), col="yellow", se=F)+
  geom_smooth(data=chunkexp, aes(x=chunkexp$chunks, y= chunkexp$chunkmean), col="pink", se=F)+
  geom_smooth(data=chunkgp, aes(x=chunkgp$chunks, y= chunkgp$chunkmean), col="darkblue", se=F)
  



felt_data <-read.table("felt_data.txt",header=T,sep=";")
FGP <- data.frame(felt_data$STASJON_NR, felt_data$QD_fgp)
colnames(FGP) <- c("Station","FGP")
write.table (FGP, file = "FGP_values.txt", sep = "\ ")
FGP_up <- read.table("FGP_values.txt", header=T, sep="\ ")
FGP_up$Station <- as.character(FGP_up$Station)

stations <- stations[order(stations$Stations, stations$years), , drop=F]
FGP_up <- FGP_up[order(FGP_up$Station, FGP_up$FGP), , drop=F]
stations[, "FGP"] <- NA
stations$FGP <- FGP_up$FGP

gev_valuesz=gev_values
gev_valuesz$Station <- as.character(gev_valuesz$Station)
gev_valuesx <- gev_valuesz[order(gev_valuesz$Station, gev_valuesz$Years, gev_valuesz$ADscore_pot, gev_valuesz$KSscore_pot,
                                gev_valuesz$ADscore_ams, gev_valuesz$KSscore_ams,gev_valuesz$Floods.per.year,
                                gev_valuesz$CV.AD.score, gev_valuesz$CV.AD.pvalue,gev_valuesz$CV.KS.score,
                                gev_valuesz$CV.KS.pvalue, gev_valuesz$Quantile.5.year, gev_valuesz$Quantile.10.year,
                                gev_valuesz$Quantile.20.year, gev_valuesz$Brier.5.year, gev_valuesz$Brier.10.year,
                                gev_valuesz$Brier.20.year, gev_valuesz$chunk), , drop=F]
stations30x <- stations[!(stations$years < 30),]
gev_valuesx[, "FGP"] <- NA
gev_valuesx$FGP <- stations30x$FGP


.leaflet-container {
  background-color:rgba(255,0,0,0.0);
}

mapresults <- function (group.index) 
{
  print("in norway_map4groups function")
  print(group.index)
  #group.name <- station$name[group.index]
  group.nve_nb <- stationxtrue$nve_nb[group.index]
  group.long <- stationxtrue$long[group.index]
  group.lat <- stationxtrue$lat[group.index]
  group.FPY <- stationxtrue$FPY[group.index]
  my.colors <- c("red", "blue", "black", "cyan")
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c("GP", "EXP", "GEV", "GUM")
    color <- my.colors[x2plot]
    invisible(color)
  }
  
  map <- leaflet() %>% setView(13, 64, zoom = 4) %>% addWMSTiles("https://openwms.statkart.no/skwms1/wms.norges_grunnkart?", 
                                                                          layers = "Norges_grunnkart", options = WMSTileOptions(format = "image/png", 
                                                                          transparent = TRUE), tileOptions(tms = TRUE), attribution = "Kartverket")
  addCircleMarkers(map, data = stationxtrue, lng = ~long, lat = ~lat, 
                   popup = paste("Number:", stationx$nve_nb, "Floods per year:", stationxtrue$FPY, 
                                 sep = " "), radius = 3, color = ~my.color.func(stationxtrue$QSTrue, 
                                                                                my.colors), stroke = FALSE, fillOpacity = 1) %>% 
    addMarkers(group.long, group.lat, popup = paste("Number:", group.nve_nb, 
                                                    "Floods per year:", group.FPY, sep = " ")) %>% 
    addLegend(position = "bottomright", colors = my.colors, 
              labels = c("EXP", "GEV", "GP", "GUM"), title = "Best scoring distribution", 
              opacity = 1)
}

mapresults(400)


###################################################################

fgp_plot2 <- data.frame(station = fgp_plot$station, FGP = fgp_plot$EXP.FGP, method_EXP.brier.pot = fgp_plot$EXP.brier.pot,
                        method_EXP.brier.ams = fgp_plot$EXP.brier.ams,
                        method_GP.brier.pot = fgp_plot$GP.brier.pot,
                        method_GP.brier.ams = fgp_plot$GP.brier.ams,
                        method_GEV.ams = fgp_plot$GEV.ams,
                        method_GUM.ams = fgp_plot$GUM.ams)

cols <- c("EXP.brier.pot"="red", "GP.brier.pot"="black", "GEV.ams" ="blue", "GUM.ams" = "cyan", "EXP.brier.ams" ="red", "GP.brier.ams"="black")
#linet <- c("POT/POT or AMS/AMS"= "solid", "POT/AMS" = "dashed")
linet <- c("EXP.brier.pot"="solid", "GP.brier.pot"="solid", "GEV.ams" ="solid", "GUM.ams" = "solid", "EXP.brier.ams" ="dotted", "GP.brier.ams"="dotted")

#fgp_plot3 <- tidyr::separate(fgp_plot2, into= c("Type", "Variable"), sep = "_")
fgp_plot3 <- tidyr::gather(fgp_plot2, key = Tmp, value =`Brier score`, -FGP,-station) %>%
  tidyr::separate(Tmp, into= c("Type", "Variable" ), sep = "_")

s <- ggplot()+
  scale_color_manual(values=cols)+
  scale_linetype_manual(values =linet)+
  geom_smooth(data=fgp_plot3, aes(x=FGP, y=`Brier score`, col=Variable, lty = Variable), se=F, method = "loess")+
  theme(legend.position = "bottom")
s
returnlevspgum5 = NULL
returnlevspexp5 = NULL
returnlevspgp5 = NULL
returnlevspgev5 = NULL
returnlevspgum10 = NULL
returnlevspexp10 = NULL
returnlevspgp10 = NULL
returnlevspgev10 = NULL
returnlevspgum20 = NULL
returnlevspexp20 = NULL
returnlevspgp20 = NULL
returnlevspgev20 = NULL
returnlevspgum50 = NULL
returnlevspexp50 = NULL
returnlevspgp50 = NULL
returnlevspgev50 = NULL
returnlevspgum100 = NULL
returnlevspexp100 = NULL
returnlevspgp100 = NULL
returnlevspgev100 = NULL
returnlevspgum200 = NULL
returnlevspexp200 = NULL
returnlevspgp200 = NULL
returnlevspgev200 = NULL


for(i in 1:length(returnlevs$X5.GP)){
  returnlevspgum5[i] <- returnlevs$X5.GUM[i]/returnlevs$X5.GP[i]
  returnlevspgum10[i] <- returnlevs$X10.GUM[i]/returnlevs$X10.GP[i]
  returnlevspgum20[i] <- returnlevs$X20.GUM[i]/returnlevs$X20.GP[i]
  returnlevspgum50[i] <- returnlevs$X50.GUM[i]/returnlevs$X50.GP[i]
  returnlevspgum100[i] <- returnlevs$X100.GUM[i]/returnlevs$X100.GP[i]
  returnlevspgum200[i] <- returnlevs$X200.GUM[i]/returnlevs$X200.GP[i]
  
  returnlevspexp5[i] <- returnlevs$X5.EXP[i]/returnlevs$X5.GP[i]
  returnlevspexp10[i] <- returnlevs$X10.EXP[i]/returnlevs$X10.GP[i]
  returnlevspexp20[i] <- returnlevs$X20.EXP[i]/returnlevs$X20.GP[i]
  returnlevspexp50[i] <- returnlevs$X50.EXP[i]/returnlevs$X50.GP[i]
  returnlevspexp100[i] <- returnlevs$X100.EXP[i]/returnlevs$X100.GP[i]
  returnlevspexp200[i] <- returnlevs$X200.EXP[i]/returnlevs$X200.GP[i]
  
  returnlevspgev5[i] <- returnlevs$X5.GEV[i]/returnlevs$X5.GP[i]
  returnlevspgev10[i] <- returnlevs$X10.GEV[i]/returnlevs$X10.GP[i]
  returnlevspgev20[i] <- returnlevs$X20.GEV[i]/returnlevs$X20.GP[i]
  returnlevspgev50[i] <- returnlevs$X50.GEV[i]/returnlevs$X50.GP[i]
  returnlevspgev100[i] <- returnlevs$X100.GEV[i]/returnlevs$X100.GP[i]
  returnlevspgev200[i] <- returnlevs$X200.GEV[i]/returnlevs$X200.GP[i]
  
  returnlevspgp5[i] <- returnlevs$X5.GP[i]/returnlevs$X5.GP[i]
  returnlevspgp10[i] <- returnlevs$X10.GP[i]/returnlevs$X10.GP[i]
  returnlevspgp20[i] <- returnlevs$X20.GP[i]/returnlevs$X20.GP[i]
  returnlevspgp50[i] <- returnlevs$X50.GP[i]/returnlevs$X50.GP[i]
  returnlevspgp100[i] <- returnlevs$X100.GP[i]/returnlevs$X100.GP[i]
  returnlevspgp200[i] <- returnlevs$X200.GP[i]/returnlevs$X200.GP[i]
  
}
rl_rs <- data.frame(matrix(ncol = 24, nrow= 0))
rl_rs <- data.frame("gum5" = returnlevspgum5, "gum10" = returnlevspgum10, "gum20" = returnlevspgum20,
                           "gum50" = returnlevspgum50, "gum100" = returnlevspgum100, "gum200" = returnlevspgum200,
                           "gev5" = returnlevspgev5, "gev10" = returnlevspgev10, "gev20" = returnlevspgev20,
                           "gev50" = returnlevspgev50, "gev100" = returnlevspgev100, "gev200" = returnlevspgev200,
                           "gp5" = returnlevspgp5, "gp10" = returnlevspgp10, "gp20" = returnlevspgp20,
                           "gp50" = returnlevspgp50, "gp100" = returnlevspgp100, "gp200" = returnlevspgp200,
                           "exp5" = returnlevspexp5, "exp10" = returnlevspexp10, "exp20" = returnlevspexp20,
                           "exp50" = returnlevspexp50, "exp100" = returnlevspexp100, "exp200" = returnlevspexp200)
boxplot (rl_rs$gum5, rl_rs$gev5, rl_rs$gp5, rl_rs$exp5,
         rl_rs$gum10, rl_rs$gev10, rl_rs$gp10, rl_rs$exp10,
         rl_rs$gum20, rl_rs$gev20, rl_rs$gp20, rl_rs$exp20,
         rl_rs$gum50, rl_rs$gev50, rl_rs$gp50, rl_rs$exp50,
         rl_rs$gum100, rl_rs$gev100, rl_rs$gp100, rl_rs$exp100,
         rl_rs$gum200, rl_rs$gev200, rl_rs$gp200, rl_rs$exp200)
boxplot(rl_rs)

mapresults <- function (group.index) 
{
  print("in norway_map4groups function")
  print(group.index)
  #group.name <- station$name[group.index]
  group.nve_nb <- stationxtrue$nve_nb[group.index]
  group.long <- stationxtrue$long[group.index]
  group.lat <- stationxtrue$lat[group.index]
  group.FPY <- stationxtrue$FPY[group.index]
  my.colors <- c(A, B, C, D, E)
  plotty <- stationxtrue[-which(is.na(stationxtrue$FGP)),]
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0.2, 0.4, 0.6, 0.8, 1)
    color <- my.colors[trunc(x2plot/0.2)+1]
    invisible(color)
  }
  
  map <- leaflet() %>% setView(13, 64, zoom = 4) %>% addWMSTiles("https://openwms.statkart.no/skwms1/wms.norges_grunnkart?", 
                                                                 layers = "Norges_grunnkart", options = WMSTileOptions(format = "image/png", 
                                                                                                                       transparent = TRUE), tileOptions(tms = TRUE), attribution = "Kartverket")
  addCircleMarkers(map, data = plotty, lng = ~long, lat = ~lat, 
                   popup = paste("Number:", plotty$nve_nb, "FGP:", plotty$FGP, 
                                 sep = " "), radius = 3, color = ~my.color.func(plotty$FGP, 
                                                                                my.colors), stroke = FALSE, fillOpacity = 1) %>% 
    addMarkers(group.long, group.lat, popup = paste("Number:", group.nve_nb, 
                                                    "Floods per year:", group.FPY, sep = " ")) %>% 
    addLegend(position = "bottomright", colors = my.colors, 
              labels = c(0.2, 0.4, 0.6, 0.8, 1), title = "Flood generating process", 
              opacity = 1)
}
mapresults(500)
