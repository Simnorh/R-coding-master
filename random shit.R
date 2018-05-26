setwd("D:/Master/Thesis/Data")
for (h in 1:1){
  return_l <- data.frame(matrix(ncol = 12, nrow = 0))
  cnames_RL <- c("5 GP", "5 EXP", "5 GEV","5 GUM","10 GP", "10 EXP", "10 GEV","10 GUM","20 GP", "20 EXP", "20 GEV","20 GUM")
  colnames(return_l) <- cnames_RL  
  
  gev_gofvalues <- data.frame(matrix(ncol = 18, nrow = 0))
  cnames_gev <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year", "FGP")
  colnames(gev_gofvalues) <- cnames_gev
  
  gp_gofvalues <- data.frame(matrix(ncol = 24, nrow = 0))
  cnames_gp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year", "FGP", "Quantile 5 year ams", "Quantile 10 year ams", "Quantile 20 year ams", "Brier 5 year ams", "Brier 10 year ams", "Brier 20 year ams")
  colnames(gp_gofvalues) <- cnames_gp
  
  exp_gofvalues <- data.frame(matrix(ncol = 24, nrow = 0))
  cnames_exp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year", "FGP", "Quantile 5 year ams", "Quantile 10 year ams", "Quantile 20 year ams", "Brier 5 year ams", "Brier 10 year ams", "Brier 20 year ams")
  colnames(exp_gofvalues) <- cnames_exp
  
  gumbel_gofvalues <- data.frame(matrix(ncol = 18, nrow = 0))
  cnames_gum <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year", "FGP")
  colnames(gumbel_gofvalues) <- cnames_gum
  
  pot_data <-read.table("pot_and_fgp.txt",header=T,sep="\ ")
  ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")
  
  ams_years <- str_split_fixed(ams_data$daily_ams_dates, "-", 3)
  cnames_years <- c("year", "month", "date")
  colnames(ams_years) <- cnames_years
  ams_years <- subset(ams_years, select =c(year))
  ams_data <- cbind(ams_data, ams_years)
  
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
  
  stations <- stations[order(stations$Stations, stations$years), , drop=F]
  felt_data <-read.table("felt_data.txt",header=T,sep=";")
  FGP <- data.frame(felt_data$STASJON_NR, felt_data$QD_fgp, felt_data$QD_median)
  colnames(FGP) <- c("Station","FGP", "Average yearly flood")
  #write.table(FGP, file = "FGP_values.txt", sep = "\ ", dec = ".") #Before you do this you have to enter the text file manually and replace all ".0" with nothing
  FGP_up <- read.table("FGP_values.txt", header=T, sep="\ ", dec="." )
  FGP_up$Station <- as.character(FGP_up$Station)
  FGP_up <- FGP_up[order(FGP_up$Station, FGP_up$FGP, FGP_up$Average.yearly.flood), , drop=F]
  stations[, "FGP"] <- NA
  stations[, "Average yearly flood"] <- NA
  stations$FGP <- FGP_up$FGP
  stations$`Average yearly flood` <- FGP_up$Average.yearly.flood
  stations30 <- stations[!(stations$years < 30),]
}
for (s in 1:length(stations30$Stations)){
  ams_river <- ams_data[ams_data[,9] %in% stations30$Stations[s], ]
  pot_river <- pot_data[pot_data[,8] %in% stations30$Stations[s], ]
  
  if(length(pot_river$FGP) == 0){
    next
  } else {
    #ams_river <- ams_data[ams_data$station == "2.11", ]
    #pot_river <- pot_data[pot_data$station == "2.11", ]
    for(l in 1:1){
      testplotframe <- as.data.frame(ams_river$station)
      colnames(testplotframe)[1] <- "Station"
      testplotframe <- testplotframe %>% add_count(Station)
      colnames(testplotframe)[2] <- "years"
      testplotframe <- dplyr::distinct(testplotframe)
      Floods_year <- data.frame(nrow(pot_river)/testplotframe$years)
      testplotframe <- cbind(testplotframe, Floods_year)
      colnames(testplotframe)[3] <- "Floods per year"
      FGP_station <- stations30[stations30$Stations %in% pot_river$station[1], 3]
      testplotframe <- cbind(testplotframe, FGP_station)
      colnames(testplotframe)[4] <- "FGP"
      AV_flood_station <- stations30[stations30$Stations %in% pot_river$station[1], 4]
      testplotframe <- cbind(testplotframe, AV_flood_station)
      colnames(testplotframe)[5] <- "Average yearly flood"
    }
    
    for(t in 1:1){
      gevpar_pot <- gev_Lmom(pot_river$flood.1)
      gevpar_ams <- gev_Lmom(ams_river$daily_ams.1)
      
      gppar_pot <- gp_Lmom(pot_river$flood.1, threshold = NA)
      gppar_ams <- gp_Lmom(ams_river$daily_ams.1)
      
      gumpar_pot <- gumbel_Lmom(pot_river$flood.1)
      gumpar_ams <- gumbel_Lmom(ams_river$daily_ams.1)
      
      exppar_pot <- exp_Lmom(pot_river$flood.1 , threshold = NA)
      exppar_ams <- exp_Lmom(ams_river$daily_ams.1, threshold = NA)
      
      #param_estimate <- gev_Lmom (resampled_data$daily_ams.1)
      
      AD_gevams <- gof_ad(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
      AD_gevpot <- gof_ad(pot_river$flood.1, gevpar_pot$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
      KS_gevams <- gof_ks(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "gev", test.stat = TRUE, p.value = FALSE)
      KS_gevpot <- gof_ks(pot_river$flood.1, gevpar_pot$estimate, distr = "gev", test.stat = TRUE, p.value = FALSE)
      
      AD_gpams <- gof_ad(ams_river$daily_ams.1, gppar_ams$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
      AD_gppot <- gof_ad(pot_river$flood.1, gppar_pot$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
      KS_gpams <- gof_ks(ams_river$daily_ams.1, gppar_ams$estimate, distr = "gp", test.stat = TRUE, p.value = FALSE)
      KS_gppot <- gof_ks(pot_river$flood.1, gppar_pot$estimate, distr = "gp", test.stat = TRUE, p.value = FALSE)
      
      AD_gumams <- gof_ad(ams_river$daily_ams.1, gumpar_ams$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
      AD_gumpot <- gof_ad(pot_river$flood.1, gumpar_pot$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
      KS_gumams <- gof_ks(ams_river$daily_ams.1, gumpar_ams$estimate, distr = "gumbel", test.stat = TRUE, p.value = FALSE)
      KS_gumpot <- gof_ks(pot_river$flood.1, gumpar_pot$estimate, distr = "gumbel", test.stat = TRUE, p.value = FALSE)
      
      AD_expams <- gof_ad(ams_river$daily_ams.1, exppar_ams$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
      AD_exppot <- gof_ad(pot_river$flood.1, exppar_pot$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
      KS_expams <- gof_ks(ams_river$daily_ams.1, exppar_ams$estimate, distr = "exp", test.stat = TRUE, p.value = FALSE)
      KS_exppot <- gof_ks(pot_river$flood.1, exppar_pot$estimate, distr = "exp", test.stat = TRUE, p.value = FALSE)
    }
    for(u in 1:1){
      AD_values_gev_pot <- data.frame(CS = NA, KS = KS_gevpot, AD = AD_gevpot)
      AD_values_gev_ams <- data.frame(CS = NA, KS = KS_gevams, AD = AD_gevams)
      
      AD_values_gp_pot <- data.frame(CS = NA, KS = KS_gppot, AD = AD_gppot)
      AD_values_gp_ams <- data.frame(CS = NA, KS = KS_gpams, AD = AD_gpams)
      
      AD_values_gum_pot <- data.frame(CS = NA, KS = KS_gumpot, AD = AD_gumpot)
      AD_values_gum_ams <- data.frame(CS = NA, KS = KS_gumams, AD = AD_gumams)
      
      AD_values_exp_pot <- data.frame(CS = NA, KS = KS_exppot, AD = AD_exppot)
      AD_values_exp_ams <- data.frame(CS = NA, KS = KS_expams, AD = AD_expams)
    }    
    #plot_all (pot_river$flood.1,GOF.list = AD_values_gp_pot,param = gppar_pot,distr = "gp", method = "ad")
    for(z in 1:1){
      set.seed(5121)
      sam_pot_river <- pot_river[sample(nrow(pot_river)),]
      set.seed(9871)
      sam_ams_river <- ams_river[sample(nrow(ams_river)),]
      
      return_p<- list(Periods = c(5, 10, 20, 50, 100, 200))
      kfold = 10
      dimrivams <- dim(sam_ams_river)[1]/kfold
      gs_ams <- floor(dimrivams)
      
      bn_ams <- as.vector(ams_river$year)
      bsize_ams <- as.numeric(table(bn_ams))
      bsizevec_ams <- as.data.frame(bsize_ams)
      ub_ams <- unique(bn_ams)
      blockdf_ams <- cbind(ub_ams, bsizevec_ams)
      set.seed(145)
      rbn_ams <- blockdf_ams[sample(nrow(blockdf_ams)),]
      gsize_ams <- floor(length(bn_ams)/kfold)
      bcum_ams = cumsum(rbn_ams$bsize_ams)
      
      bn <- as.vector(pot_river$year)
      bsize <- as.numeric(table(bn))
      bsizevec <- as.data.frame(bsize)
      ub <- unique(bn)
      blockdf <- cbind(ub, bsizevec)
      set.seed(145)
      rbn <- blockdf[sample(nrow(blockdf)),]
      gsize <- floor(length(bn)/kfold)
      bcum = cumsum(rbn$bsize)
    }
    
    for(q in 1:1){
      cv_gp_ad = NULL
      cv_gp_ks = NULL
      cv_gp_pad = NULL
      cv_gp_pks = NULL
      return_gp_5 = NULL
      return_gp_10= NULL
      return_gp_20 = NULL
      return_gp_50 = NULL
      return_gp_100 = NULL
      return_gp_200 = NULL
      quantlistgp_5 = NULL
      quantlistgp_10 = NULL
      quantlistgp_20 = NULL
      bsgp_5 = NULL
      bsgp_10 = NULL
      bsgp_20 = NULL
      qsgp_5 = NULL
      qsgp_10 = NULL
      qsgp_20 = NULL
      cv_exp_ad = NULL
      cv_exp_ks = NULL
      cv_exp_pad = NULL
      cv_exp_pks = NULL
      return_exp_5 = NULL
      return_exp_10= NULL
      return_exp_20 = NULL
      return_exp_50 = NULL
      return_exp_100 = NULL
      return_exp_200 = NULL
      quantlistexp_5 = NULL
      quantlistexp_10 = NULL
      quantlistexp_20 = NULL
      bsexp_5 = NULL
      bsexp_10 = NULL
      bsexp_20 = NULL
      qsexp_5 = NULL
      qsexp_10 = NULL
      qsexp_20 = NULL
      cv_gev_ad = NULL
      cv_gev_ks = NULL
      cv_gev_pad = NULL
      cv_gev_pks = NULL
      return_gev_5 = NULL
      return_gev_10= NULL
      return_gev_20 = NULL
      return_gev_50 = NULL
      return_gev_100 = NULL
      return_gev_200 = NULL
      bsgev_5 = NULL
      bsgev_10 = NULL
      bsgev_20 = NULL
      qsgev_5 = NULL
      qsgev_10 = NULL
      qsgev_20 = NULL
      cv_gumbel_ad = NULL
      cv_gumbel_ks = NULL
      cv_gumbel_pad = NULL
      cv_gumbel_pks = NULL
      return_gum_5 = NULL
      return_gum_10= NULL
      return_gum_20 = NULL
      return_gum_50 = NULL
      return_gum_100 = NULL
      return_gum_200 = NULL
      bsgum_5 = NULL
      bsgum_10 = NULL
      bsgum_20 = NULL
      qsgum_5 = NULL
      qsgum_10 = NULL
      qsgum_20 = NULL
      amsbsgp_5 = NULL
      amsbsgp_10 = NULL
      amsbsgp_20 = NULL
      amsqsgp_5 = NULL
      amsqsgp_10 = NULL
      amsqsgp_20 = NULL
      amsbsexp_5 = NULL
      amsbsexp_10 = NULL
      amsbsexp_20 = NULL
      amsqsexp_5 = NULL
      amsqsexp_10 = NULL
      amsqsexp_20 = NULL
    }
    
    for (i in 1:kfold){
      i1 = which.min(abs((i-1)*gsize+1-bcum))
      i2 = which.min(abs(i*gsize-bcum))
      testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
      traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5]
      traindfyears <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 7]
      paramsgp <- gp_Lmom(traindf, threshold = NA)
      cv_gp_ad[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
      cva_gp_ad <- mean(cv_gp_ad)
      cv_gp_pad[i] <- gof_ad(testdf, paramsgp$estimate, distr = "gp", test.stat=FALSE, p.value=TRUE)
      cvp_gp_ad <- mean(cv_gp_pad)
      
      cv_gp_ks[i] <- gof_ks(testdf, paramsgp$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
      cva_gp_ks <- mean(cv_gp_ks)
      cv_gp_pks[i] <- gof_ks(testdf, paramsgp$estimate, distr = "gp", test.stat=FALSE, p.value=TRUE)
      cvp_gp_ks <- mean(cv_gp_pks)
      
      return_gp_5[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[1])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
      #quantlistgp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_5-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gp_10[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[2])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
      #quantlistgp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_10-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gp_20[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[3])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
      #quantlistgp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_20-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      return_gp_50[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[4])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
      return_gp_100[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[5])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
      return_gp_200[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[6])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
      
      
      returngp5_av <- mean(return_gp_5)
      returngp10_av <- mean(return_gp_10)
      returngp20_av <- mean(return_gp_20)
      returngp50_av <- mean(return_gp_50)
      returngp100_av <- mean(return_gp_100)
      returngp200_av <- mean(return_gp_200)
      quantlistgp5_av <- mean(quantlistgp_5)
      quantlistgp10_av <- mean(quantlistgp_10)
      quantlistgp20_av <- mean(quantlistgp_20)
      
      bsgp_failsafe <- purrr::possibly(BS4NC_tryout, NA)
      
      bsgp_5[i] <- bsgp_failsafe(testdf, threshold = return_gp_5[i], param = paramsgp$estimate, distr = "gp")
      bsgp_10[i] <- bsgp_failsafe(testdf, threshold = return_gp_10[i], param = paramsgp$estimate, distr = "gp")
      bsgp_20[i] <- bsgp_failsafe(testdf, threshold = return_gp_20[i], param = paramsgp$estimate, distr = "gp")
      
      qsgp_failsafe <- purrr::possibly(QS4NC, NA)
      
      qsgp_5[i] <- qsgp_failsafe(testdf, r.levels = return_gp_5[i], r.periods = return_p$Periods[1])
      qsgp_10[i] <- qsgp_failsafe(testdf, r.levels = return_gp_10[i], r.periods = return_p$Periods[2])
      qsgp_20[i] <- qsgp_failsafe(testdf, r.levels = return_gp_20[i], r.periods = return_p$Periods[3])
      
      qsgp5_av <- mean(qsgp_5)
      qsgp10_av <- mean(qsgp_10)
      qsgp20_av <- mean(qsgp_20)
      
      qsgp5_av <- qsgp5_av/testplotframe$`Average yearly flood`
      qsgp10_av <- qsgp10_av/testplotframe$`Average yearly flood`
      qsgp20_av <- qsgp20_av/testplotframe$`Average yearly flood`
      qsgp50_av <- qsgp50_av/testplotframe$`Average yearly flood`
      qsgp100_av <- qsgp100_av/testplotframe$`Average yearly flood`
      qsgp200_av <- qsgp200_av/testplotframe$`Average yearly flood`
      
      bsgp5_av <- mean(bsgp_5)
      bsgp10_av <- mean(bsgp_10)
      bsgp20_av <- mean(bsgp_20)
      
      low_values <- sam_ams_river[sam_ams_river$daily_ams.1 < sam_pot_river$threshold[2], ]
      low_vect <- as.vector(low_values$daily_ams.1)
      testdf_ams <- sam_ams_river[!(sam_ams_river[,8] %in% traindfyears), 5]
      testdf_ams<- append(testdf_ams, low_vect[i])
      testdf_ams <- testdf_ams[!is.na(testdf_ams)]
      
      amsbsgp_5[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_5[i], param = paramsgp$estimate, distr = "gp")
      amsbsgp_10[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_10[i], param = paramsgp$estimate, distr = "gp")
      amsbsgp_20[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_20[i], param = paramsgp$estimate, distr = "gp")
      
      amsqsgp_5[i] <- QS4NC(testdf_ams, r.levels = return_gp_5[i], r.periods = return_p$Periods[1])
      amsqsgp_10[i] <- QS4NC(testdf_ams, r.levels = return_gp_10[i], r.periods = return_p$Periods[2])
      amsqsgp_20[i] <- QS4NC(testdf_ams, r.levels = return_gp_20[i], r.periods = return_p$Periods[3])
      
      amsqsgp5_av  <- mean(amsqsgp_5)
      amsqsgp10_av <- mean(amsqsgp_10)
      amsqsgp20_av <- mean(amsqsgp_20)
      amsbsgp5_av <- mean(amsbsgp_5)
      amsbsgp10_av <- mean(amsbsgp_10)
      amsbsgp20_av <- mean(amsbsgp_20)
    }#gp
    
    for (i in 1:kfold){
      i1 = (i-1)*gs_ams + 1
      i2 = i*gs_ams
      testdf <- sam_ams_river[(i1:i2), 5]
      traindf<- sam_ams_river[-(i1:i2), 5] 
      paramsgev <- gev_Lmom(traindf)
      cv_gev_ad[i] <- gof_ad(testdf, paramsgev$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
      cva_gev_ad <- mean(cv_gev_ad)
      #if(is.na(cva_gev_ad)){
      #  cva_gev_ad <- mean(!is.na(cv_gev_ad))
      #}
      cv_gev_pad[i] <- gof_ad(testdf, paramsgev$estimate, distr = "gev", test.stat=FALSE, p.value=TRUE)
      cvp_gev_ad <- mean(cv_gev_pad)
      
      cv_gev_ks[i] <- gof_ks(testdf, paramsgev$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
      cva_gev_ks <- mean(cv_gev_ks)
      cv_gev_pks[i] <- gof_ks(testdf, paramsgev$estimate, distr = "gev", test.stat=FALSE, p.value=TRUE)
      cvp_gev_ks <- mean(cv_gev_pks)
      
      return_gev_5[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(-paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[1])))^(-paramsgev$estimate[3]))
      #quantlistgp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_5-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gev_10[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(-paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[2])))^(-paramsgev$estimate[3]))
      #quantlistgp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_10-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gev_20[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(-paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[3])))^(-paramsgev$estimate[3]))
      #quantlistgp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_20-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      return_gev_50[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(-paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[4])))^(-paramsgev$estimate[3]))
      return_gev_100[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(-paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[5])))^(-paramsgev$estimate[3]))
      return_gev_200[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(-paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[6])))^(-paramsgev$estimate[3]))
      
      returngev5_av <- mean(return_gev_5)
      returngev10_av <- mean(return_gev_10)
      returngev20_av <- mean(return_gev_20)
      returngev50_av <- mean(return_gev_50)
      returngev100_av <- mean(return_gev_100)
      returngev200_av <- mean(return_gev_200)
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
      
      qsgev5_av <- qsgev5_av/testplotframe$`Average yearly flood`
      qsgev10_av <- qsgev10_av/testplotframe$`Average yearly flood`
      qsgev20_av <- qsgev20_av/testplotframe$`Average yearly flood`
      
      bsgev5_av <- mean(bsgev_5)
      bsgev10_av <- mean(bsgev_10)
      bsgev20_av <- mean(bsgev_20)
    }#gev
    
    for (i in 1:kfold){
      i1 = which.min(abs((i-1)*gsize+1-bcum))
      i2 = which.min(abs(i*gsize-bcum))
      testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
      traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
      traindfyears <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 7]
      paramsexp <- exp_Lmom(traindf)
      cv_exp_ad[i] <- gofad(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
      cva_exp_ad <- mean(cv_exp_ad)
      cv_exp_pad[i] <- gofad(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
      cvp_exp_ad <- mean(cv_exp_pad)
      
      cv_exp_ks[i] <- gofks(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
      cva_exp_ks <- mean(cv_exp_ks)
      cv_exp_pks[i] <- gofks(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
      cvp_exp_ks <- mean(cv_exp_pks)
      
      
      return_exp_5[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[1]))))
      quantlistexp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp_5[i]-paramsexp$estimate[1])/paramsexp$estimate[2])))))
      
      return_exp_10[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[2])))) 
      quantlistexp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp_10[i]-paramsexp$estimate[1])/paramsexp$estimate[2])))))
      
      return_exp_20[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[3]))))
      quantlistexp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-exp(-((return_exp_20[i]-paramsexp$estimate[1])/paramsexp$estimate[2])))))
      return_exp_50[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[4]))))
      return_exp_100[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[5]))))
      return_exp_200[i] = paramsexp$estimate[1]- paramsexp$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/return_p$Periods[6]))))
      
      
      returnexp5_av <- mean(return_exp_5)
      returnexp10_av <- mean(return_exp_10)
      returnexp20_av <- mean(return_exp_20)
      returnexp50_av <- mean(return_exp_50)
      returnexp100_av <- mean(return_exp_100)
      returnexp200_av <- mean(return_exp_200)
      
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
      
      qsexp5_av <- qsexp5_av/testplotframe$`Average yearly flood`
      qsexp10_av <- qsexp10_av/testplotframe$`Average yearly flood`
      qsexp20_av <- qsexp20_av/testplotframe$`Average yearly flood`
      
      bsexp5_av <- mean(bsexp_5)
      bsexp10_av <- mean(bsexp_10)
      bsexp20_av <- mean(bsexp_20)
      
      low_values <- sam_ams_river[sam_ams_river$daily_ams.1 < sam_pot_river$threshold[2], ]
      low_vect <- as.vector(low_values$daily_ams.1)
      testdf_ams <- sam_ams_river[!(sam_ams_river[,8] %in% traindfyears), 5]
      testdf_ams<- append(testdf_ams, low_vect[i])
      testdf_ams <- testdf_ams[!is.na(testdf_ams)]
      
      amsbsexp_5[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_5[i], param = paramsexp$estimate, distr = "exp")
      amsbsexp_10[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_10[i], param = paramsexp$estimate, distr = "exp")
      amsbsexp_20[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_20[i], param = paramsexp$estimate, distr = "exp")
      
      amsqsexp_5[i] <- QS4NC(testdf_ams, r.levels = return_exp_5[i], r.periods = return_p$Periods[1])
      amsqsexp_10[i] <- QS4NC(testdf_ams, r.levels = return_exp_10[i], r.periods = return_p$Periods[2])
      amsqsexp_20[i] <- QS4NC(testdf_ams, r.levels = return_exp_20[i], r.periods = return_p$Periods[3])
      
      amsqsexp5_av <- mean(amsqsexp_5)
      amsqsexp10_av <- mean(amsqsexp_10)
      amsqsexp20_av <- mean(amsqsexp_20)
      amsbsexp5_av <- mean(amsbsexp_5)
      amsbsexp10_av <- mean(amsbsexp_10)
      amsbsexp20_av <- mean(amsbsexp_20)
    }#exp
    
    for (i in 1:kfold){
      i1 = (i-1)*gs_ams + 1
      i2 = i*gs_ams
      testdf <- sam_ams_river[(i1:i2), 5]
      traindf<- sam_ams_river[-(i1:i2), 5] 
      paramsgum <- gumbel_Lmom(traindf)
      cv_gumbel_ad[i] <- gof_ad(testdf, paramsgum$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
      cva_gum_ad <- mean(cv_gumbel_ad)
      cv_gumbel_pad[i] <- gof_ad(testdf, paramsgum$estimate, distr = "gumbel", test.stat=FALSE, p.value=TRUE)
      cvp_gum_ad <- mean(cv_gumbel_pad)
      
      cv_gumbel_ks[i] <- gof_ks(testdf, paramsgum$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
      cva_gum_ks <- mean(cv_gumbel_ks)
      cv_gumbel_pks[i] <- gof_ks(testdf, paramsgum$estimate, distr = "gumbel", test.stat=FALSE, p.value=TRUE)
      cvp_gum_ks <- mean(cv_gumbel_pks)
      
      return_gum_5[i] = paramsgum$estimate[1]- paramsgum$estimate[2]*(log(-log(1-1/return_p$Periods[1])))
      #quantlistgp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_5-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gum_10[i] = paramsgum$estimate[1]- paramsgum$estimate[2]*(log(-log(1-1/return_p$Periods[2])))
      #quantlistgp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_10-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gum_20[i] = paramsgum$estimate[1]- paramsgum$estimate[2]*(log(-log(1-1/return_p$Periods[3])))
      #quantlistgp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_20-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      return_gum_50[i] = paramsgum$estimate[1]- paramsgum$estimate[2]*(log(-log(1-1/return_p$Periods[4])))
      return_gum_100[i] = paramsgum$estimate[1]- paramsgum$estimate[2]*(log(-log(1-1/return_p$Periods[5])))
      return_gum_200[i] = paramsgum$estimate[1]- paramsgum$estimate[2]*(log(-log(1-1/return_p$Periods[6])))
      
      returngum5_av <- mean(return_gum_5)
      returngum10_av <- mean(return_gum_10)
      returngum20_av <- mean(return_gum_20)
      returngum50_av <- mean(return_gum_50)
      returngum100_av <- mean(return_gum_100)
      returngum200_av <- mean(return_gum_200)
      #quantlistgp5_av <- mean(quantlistgp_5)
      #quantlistgp10_av <- mean(quantlistgp_10)
      #quantlistgp20_av <- mean(quantlistgp_20)
      
      bsgum_5[i] <- BS4NC_tryout(testdf, threshold = return_gum_5[i], param = paramsgum$estimate, distr = "gumbel")
      bsgum_10[i] <- BS4NC_tryout(testdf, threshold = return_gum_10[i], param = paramsgum$estimate, distr = "gumbel")
      bsgum_20[i] <- BS4NC_tryout(testdf, threshold = return_gum_20[i], param = paramsgum$estimate, distr = "gumbel")
      
      qsgum_5[i] <- QS4NC(testdf, r.levels = return_gum_5[i], r.periods = return_p$Periods[1])
      qsgum_10[i] <- QS4NC(testdf, r.levels = return_gum_10[i], r.periods = return_p$Periods[2])
      qsgum_20[i] <- QS4NC(testdf, r.levels = return_gum_20[i], r.periods = return_p$Periods[3])
      
      qsgum5_av <- mean(qsgum_5)
      qsgum10_av <- mean(qsgum_10)
      qsgum20_av <- mean(qsgum_20)
      
      qsgum5_av <- qsgum5_av/testplotframe$`Average yearly flood`
      qsgum10_av <- qsgum10_av/testplotframe$`Average yearly flood`
      qsgum20_av <- qsgum20_av/testplotframe$`Average yearly flood`
      
      bsgum5_av <- mean(bsgum_5)
      bsgum10_av <- mean(bsgum_10)
      bsgum20_av <- mean(bsgum_20)
    }#gum
    
    for(y in 1:1){
      newRow_gev <- data.frame("Stations" = testplotframe$Station[1],"Years" = testplotframe$years[1],
                               "ADscore_pot" = AD_gevpot, "KSscore_pot" = KS_gevpot,
                               "ADscore_ams" = AD_gevams, "KSscore_ams" = KS_gevams, 
                               "Floods per year" = testplotframe$`Floods per year`[1], 
                               "CV AD score" = cva_gev_ad, "CV AD pvalue" = cvp_gev_ad, 
                               "CV KS score" = cva_gev_ks, "CV KS pvalue" = cvp_gev_ks,
                               "Quantile 5 year" = qsgev5_av, "Quantile 10 year" = qsgev10_av,
                               "Quantile 20 year"= qsgev20_av, "Brier 5 year"= bsgev5_av, 
                               "Brier 10 year" = bsgev10_av, "Brier 20 year" = bsgev20_av,
                               "FGP" = testplotframe$FGP)
      gev_gofvalues <- rbind(newRow_gev, gev_gofvalues)
      write.table (gev_gofvalues, file = "gev valuestry2.txt", sep = "\ ")
      
      newRow_gp <- data.frame("Stations" = testplotframe$Station[1],"Years" = testplotframe$years[1],
                              "ADscore_pot" = AD_gppot, "KSscore_pot" = KS_gppot, 
                              "ADscore_ams" = AD_gpams, "KSscore_ams" = KS_gpams, 
                              "Floods per year" = testplotframe$`Floods per year`[1],
                              "CV AD score" = cva_gp_ad, "CV AD pvalue" = cvp_gp_ad,
                              "CV KS score" = cva_gp_ks, "CV KS pvalue" = cvp_gp_ks,
                              "Quantile 5 year" = qsgp5_av, "Quantile 10 year" = qsgp10_av, 
                              "Quantile 20 year"= qsgp20_av, "Brier 5 year"= bsgp5_av, 
                              "Brier 10 year" = bsgp10_av, "Brier 20 year" = bsgp20_av,
                              "Quantile 5 year ams" = amsqsgp5_av, "Quantile 10 year ams"= amsqsgp10_av,
                              "Quantile 20 year ams" = amsqsgp20_av, "Brier 5 year ams" = amsbsgp5_av, 
                              "Brier 10 year ams"= amsbsgp10_av, "Brier 20 year ams"= amsbsgp20_av,
                              "FGP" = testplotframe$FGP)
      gp_gofvalues <- rbind(newRow_gp, gp_gofvalues)
      write.table (gp_gofvalues, file = "gp valuestry2.txt", sep = "\ ")
      
      newRow_gum <- data.frame("Stations" = testplotframe$Station[1], "Years" = testplotframe$years[1], 
                               "ADscore_pot" = AD_gumpot, "KSscore_pot" = KS_gumpot, 
                               "ADscore_ams" = AD_gumams, "KSscore_ams" = KS_gumams,
                               "Floods per year" = testplotframe$`Floods per year`[1],
                               "CV AD score" = cva_gum_ad, "CV AD pvalue" = cvp_gum_ad,
                               "CV KS score" = cva_gum_ks, "CV KS pvalue" = cvp_gum_ks,
                               "Quantile 5 year" = qsgum5_av, "Quantile 10 year" = qsgum10_av,
                               "Quantile 20 year"= qsgum20_av, "Brier 5 year"= bsgum5_av,
                               "Brier 10 year" = bsgum10_av, "Brier 20 year" = bsgum20_av,
                               "FGP" = testplotframe$FGP)
      gumbel_gofvalues <- rbind(newRow_gum, gumbel_gofvalues)
      write.table (gumbel_gofvalues, file = "gumbel valuestry2.txt", sep = "\ ")
      
      newRow_exp <- data.frame("Stations" = testplotframe$Station[1], "Years" = testplotframe$years[1],
                               "ADscore_pot" = AD_exppot, "KSscore_pot" = KS_exppot,
                               "ADscore_ams" = AD_expams, "KSscore_ams" = KS_expams, 
                               "Floods per year" = testplotframe$`Floods per year`[1],
                               "CV AD score" = cva_exp_ad, "CV AD pvalue" = cvp_exp_ad,
                               "CV KS score" = cva_exp_ks, "CV KS pvalue" = cvp_exp_ks, 
                               "Quantile 5 year" = qsexp5_av, "Quantile 10 year" = qsexp10_av, 
                               "Quantile 20 year"= qsexp20_av, "Brier 5 year"= bsexp5_av,
                               "Brier 10 year" = bsexp10_av, "Brier 20 year" = bsexp20_av,
                               "Quantile 5 year ams" = amsqsexp5_av, "Quantile 10 year ams"= amsqsexp10_av,
                               "Quantile 20 year ams" = amsqsexp20_av, "Brier 5 year ams" = amsbsexp5_av, 
                               "Brier 10 year ams"= amsbsexp10_av, "Brier 20 year ams"= amsbsexp20_av,
                               "FGP" = testplotframe$FGP)
      exp_gofvalues <- rbind(newRow_exp, exp_gofvalues)
      write.table (exp_gofvalues, file = "exp valuestry2.txt", sep = "\ ")
      
      newRow_return <- data.frame("5 GP" = returngp5_av, "5 EXP"= returnexp5_av, "5 GEV"= returngev5_av,
                                  "5 GUM"= returngum5_av,"10 GP"= returngp10_av, "10 EXP"= returnexp10_av,
                                  "10 GEV"= returngev10_av,"10 GUM"= returngum10_av,"20 GP"= returngp20_av,
                                  "20 EXP"= returnexp20_av, "20 GEV"= returngev20_av,"20 GUM"= returngum20_av,
                                  "50 GP"= returngp50_av, "50 EXP"= returnexp50_av, "50 GEV"= returngev50_av,
                                  "50 GUM"= returngum50_av, "100 GP"= returngp100_av,
                                  "100 EXP"= returnexp100_av, "100 GEV"= returngev100_av,"100 GUM"= returngum100_av,
                                  "200 GP"= returngp200_av,
                                  "200 EXP"= returnexp200_av, "200 GEV"= returngev200_av,"200 GUM"= returngum200_av)
      return_l <- rbind(newRow_return, return_l)
      write.table (return_l, file = "Return level averagestry2.txt", sep = "\ ")
    }
  }
}


Stability_test <- function(amsdata = NA, potdata = NA, FPY = NA, threshold= NA){
  return5_gumboo = NA
  return10_gumboo = NA
  return20_gumboo = NA
  return50_gumboo = NA
  return100_gumboo = NA
  return200_gumboo = NA
  
  return5_gevboo = NA
  return10_gevboo = NA
  return20_gevboo = NA
  return50_gevboo = NA
  return100_gevboo = NA
  return200_gevboo = NA
  
  return5_expboo = NA
  return10_expboo = NA
  return20_expboo = NA
  return50_expboo = NA
  return100_expboo = NA
  return200_expboo = NA
  
  return5_gpboo = NA
  return10_gpboo = NA
  return20_gpboo = NA
  return50_gpboo = NA
  return100_gpboo = NA
  return200_gpboo = NA
  
  dimrivboot <- dim(amsdata)/kfold
  gs_amsboot <- floor(dimrivboot)
  
  dimrivbootpot <- dim(potdata)/kfold
  gs_potboot <- floor(dimrivbootpot)
  
  for (i in 1:1000){
    sam_bootams <- sample(amsdata, replace = TRUE)
    i1 = 1
    i2 = 1*gs_amsboot
    testdf <- sam_bootams[(i1:i2)]
    traindf<- sam_bootams[-(i1:i2)]
    paramgumboot <- gumbel_Lmom(traindf)
    paramgevboot <- gev_Lmom(traindf)
    
    return5_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/5)))
    return10_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/10)))
    return20_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/20)))
    return50_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/50)))
    return100_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/100)))
    return200_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/200)))
    
    return5_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/5)))^(-paramgevboot$estimate[3]))
    return10_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/10)))^(-paramgevboot$estimate[3]))
    return20_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/20)))^(-paramgevboot$estimate[3]))
    return50_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/50)))^(-paramgevboot$estimate[3]))
    return100_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/100)))^(-paramgevboot$estimate[3]))
    return200_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/200)))^(-paramgevboot$estimate[3]))
  }
  
  for(i in 1:1000){
    sam_bootpot <- sample(potdata, replace = TRUE)
    i1 = 1
    i2 = 1*gs_potboot
    testdf <- sam_bootpot[(i1:i2)]
    traindf<- sam_bootpot[-(i1:i2)]
    paramexpboot <- exp_Lmom(traindf)
    paramgpboot <- gp_Lmom(traindf)
    
    return5_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/FPY)*log(1-(1/5))))
    return10_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/FPY)*log(1-(1/10))))
    return20_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/FPY)*log(1-(1/20))))
    return50_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/FPY)*log(1-(1/50))))
    return100_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/FPY)*log(1-(1/100))))
    return200_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/FPY)*log(1-(1/200))))
    
    return5_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/5)/FPY))^paramgpboot$estimate[3])+threshold
    return10_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/10)/FPY))^paramgpboot$estimate[3])+threshold
    return20_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/20)/FPY))^paramgpboot$estimate[3])+threshold
    return50_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/50)/FPY))^paramgpboot$estimate[3])+threshold
    return100_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/100)/FPY))^paramgpboot$estimate[3])+threshold
    return200_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/200)/FPY))^paramgpboot$estimate[3])+threshold
  }
  
  newRow_CV <- data.frame("5 GUM" = CV5gum,"5 GEV" = CV5gev,"5 EXP" = CV5exp,"5 GP" = CV5gp,
                          "10 GUM" = CV10gum,"10 GEV" = CV10gev,"10 EXP" = CV10exp,"10 GP" = CV10gp,
                          "20 GUM" = CV20gum,"20 GEV" = CV20gev,"20 EXP" = CV20exp,"20 GP" = CV20gp,
                          "50 GUM" = CV50gum,"50 GEV" = CV50gev,"50 EXP" = CV50exp,"50 GP" = CV50gp,
                          "100 GUM" = CV100gum,"100 GEV" = CV100gev,"100 EXP" = CV100exp,
                          "100 GP" = CV100gp,"200 GUM" = CV200gum,"200 GEV" = CV200gev,
                          "200 EXP" = CV200exp,"200 GP" = CV200gp)
  return(newRow_stabilities)
}

Stability_test(amsdata = ams_river$daily_ams.1, potdata = pot_river$flood.1, 
               FPY = testplotframe$`Floods per year`, threshold = pot_river$threshold[2])



for (i in 1:1000){
  sam_bootams <- sample(amsdata, replace = TRUE)
  i1 = 1
  i2 = 1*gs_ams
  testdf <- sam_bootams[(i1:i2)]
  traindf<- sam_bootams[-(i1:i2)]
  paramgumboot <- gumbel_Lmom(traindf)
  return5_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/return_p$Periods[1])))
}
plot(return5_gumboo)
return5_gumboo
hist(return5_gumboo, breaks = 10)
stability <- shapiro.test(return5_gumboo)
stability <- stability$p.value
stability
standd <- sd(return5_gumboo)
standd
#bootstrap works

for (i in 1:1000){
  sam_bootams <- sample(sam_ams_river$daily_ams.1, replace = TRUE)
  i1 = 1
  i2 = 1*gs_amsboot
  testdf <- sam_bootams[(i1:i2)]
  traindf<- sam_bootams[-(i1:i2)]
  paramgumboot <- gumbel_Lmom(traindf)
  paramgevboot <- gev_Lmom(traindf)
  
  return5_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/5)))
  return10_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/10)))
  return20_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/20)))
  return50_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/50)))
  return100_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/100)))
  return200_gumboo[i] = paramgumboot$estimate[1]- paramgumboot$estimate[2]*(log(-log(1-1/200)))
  
  return5_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/5)))^(-paramgevboot$estimate[3]))
  return10_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/10)))^(-paramgevboot$estimate[3]))
  return20_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/20)))^(-paramgevboot$estimate[3]))
  return50_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/50)))^(-paramgevboot$estimate[3]))
  return100_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/100)))^(-paramgevboot$estimate[3]))
  return200_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/200)))^(-paramgevboot$estimate[3]))
}
Sr5gum <- shapiro.test(return5_gumboo) 
Sr5gum <- Sr5gum$p.value
Sr10gum <- shapiro.test(return10_gumboo) 
Sr10gum <- Sr10gum$p.value
Sr20gum <- shapiro.test(return20_gumboo) 
Sr20gum <- Sr20gum$p.value
Sr50gum <- shapiro.test(return50_gumboo) 
Sr50gum <- Sr50gum$p.value
Sr100gum <- shapiro.test(return100_gumboo) 
Sr100gum <- Sr100gum$p.value
Sr200gum <- shapiro.test(return200_gumboo) 
Sr200gum <- Sr200gum$p.value

Sr5gev <- shapiro.test(return5_gevboo) 
Sr5gev <- Sr5gev$p.value
Sr10gev <- shapiro.test(return10_gevboo) 
Sr10gev <- Sr10gev$p.value
Sr20gev <- shapiro.test(return20_gevboo) 
Sr20gev <- Sr20gev$p.value
Sr50gev <- shapiro.test(return50_gevboo) 
Sr50gev <- Sr50gev$p.value
Sr100gev <- shapiro.test(return100_gevboo) 
Sr100gev <- Sr100gev$p.value
Sr200gev <- shapiro.test(return200_gevboo) 
Sr200gev <- Sr200gev$p.value

for(i in 1:1000){
  sam_bootpot <- sample(pot_river$flood.1, replace = TRUE)
  i1 = 1
  i2 = 1*gs_potboot
  testdf <- sam_bootpot[(i1:i2)]
  traindf<- sam_bootpot[-(i1:i2)]
  paramexpboot <- exp_Lmom(traindf)
  paramgpboot <- gp_Lmom(traindf)
  
  return5_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/5))))
  return10_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/10))))
  return20_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/20))))
  return50_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/50))))
  return100_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/100))))
  return200_expboo[i] = paramexpboot$estimate[1]- paramexpboot$estimate[2]*(log(-(1/testplotframe$`Floods per year`)*log(1-(1/200))))
  
  return5_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/5)/testplotframe$`Floods per year`))^paramgpboot$estimate[3])+pot_river$threshold[2]
  return10_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/10)/testplotframe$`Floods per year`))^paramgpboot$estimate[3])+pot_river$threshold[2]
  return20_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/20)/testplotframe$`Floods per year`))^paramgpboot$estimate[3])+pot_river$threshold[2]
  return50_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/50)/testplotframe$`Floods per year`))^paramgpboot$estimate[3])+pot_river$threshold[2]
  return100_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/100)/testplotframe$`Floods per year`))^paramgpboot$estimate[3])+pot_river$threshold[2]
  return200_gpboo[i] = (paramgpboot$estimate[2]/paramgpboot$estimate[3])*(1-(-log(1-(1/200)/testplotframe$`Floods per year`))^paramgpboot$estimate[3])+pot_river$threshold[2]
}
Sr5exp <- shapiro.test(return5_expboo) 
Sr5exp <- Sr5exp$p.value
Sr10exp <- shapiro.test(return10_expboo) 
Sr10exp <- Sr10exp$p.value
Sr20exp <- shapiro.test(return20_expboo) 
Sr20exp <- Sr20exp$p.value
Sr50exp <- shapiro.test(return50_expboo) 
Sr50exp <- Sr50exp$p.value
Sr100exp <- shapiro.test(return100_expboo) 
Sr100exp <- Sr100exp$p.value
Sr200exp <- shapiro.test(return200_expboo) 
Sr200exp <- Sr200exp$p.value

Sr5gp <- shapiro.test(return5_gpboo) 
Sr5gp <- Sr5gp$p.value
Sr10gp <- shapiro.test(return10_gpboo) 
Sr10gp <- Sr10gp$p.value
Sr20gp <- shapiro.test(return20_gpboo) 
Sr20gp <- Sr20gp$p.value
Sr50gp <- shapiro.test(return50_gpboo) 
Sr50gp <- Sr50gp$p.value
Sr100gp <- shapiro.test(return100_gpboo) 
Sr100gp <- Sr100gp$p.value
Sr200gp <- shapiro.test(return200_gpboo) 
Sr200gp <- Sr200gp$p.value

CoV <- function(dat){
  SD <- sd(dat)
  x <- mean(dat)
  CV = SD/x
  return(CV)
}
CV <- CoV(return100_gevboo)
CV
