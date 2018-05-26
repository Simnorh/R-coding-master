setwd("D:/Master/Thesis/Data") #set working directory to wherever you want and this script will create textfiles with the results in said
for (h in 1:1){
station <- data.frame(matrix(ncol = 12, nrow= 0))
cnames_station <- c("nve_nb, long", "lat", "size", "FGP", "FPY", "KS", "AD", "BS", "QS", "BS2", "QS2")
colnames(station) <- cnames_station
  
return_l <- data.frame(matrix(ncol = 25, nrow = 0))
cnames_RL <- c("Station","5 GP", "5 EXP", "5 GEV","5 GUM","10 GP", "10 EXP", "10 GEV","10 GUM","20 GP", "20 EXP", "20 GEV","20 GUM","50 GP", "50 EXP", "50 GEV","50 GUM","100 GP", "100 EXP", "100 GEV","100 GUM","200 GP", "200 EXP", "200 GEV","200 GUM")
colnames(return_l) <- cnames_RL 

CoefficientVariance <- data.frame(matrix(ncol = 25, nrow = 0))
cnames_CV <- c("Floods per year", "5 GP", "5 EXP", "5 GEV","5 GUM","10 GP", "10 EXP", "10 GEV","10 GUM","20 GP", "20 EXP", "20 GEV","20 GUM","50 GP", "50 EXP", "50 GEV","50 GUM","100 GP", "100 EXP", "100 GEV","100 GUM","200 GP", "200 EXP", "200 GEV","200 GUM")
colnames(CoefficientVariance) <- cnames_CV 
  
gev_gofvalues <- data.frame(matrix(ncol = 25, nrow = 0))
cnames_gev <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue",  "Quantile 5 year","Quantile 10 year","Quantile 20 year","Quantile 50 year","Quantile 100 year","Quantile 200 year", "Brier 5 year", "Brier 10 year", "Brier 20 year","Brier 50 year","Brier 100 year","Brier 200 year", "FGP")
colnames(gev_gofvalues) <- cnames_gev

gp_gofvalues <- data.frame(matrix(ncol = 37, nrow = 0))
cnames_gp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year","Quantile 50 year","Quantile 100 year","Quantile 200 year", "Brier 5 year", "Brier 10 year", "Brier 20 year","Brier 50 year","Brier 100 year","Brier 200 year", "FGP", "Quantile 5 year ams", "Quantile 10 year ams", "Quantile 20 year ams", "Quantile 50 year ams", "Quantile 100 year ams", "Quantile 200 year ams", "Brier 5 year ams", "Brier 10 year ams", "Brier 20 year ams", "Brier 50 year ams", "Brier 100 year ams", "Brier 200 year ams")
colnames(gp_gofvalues) <- cnames_gp

exp_gofvalues <- data.frame(matrix(ncol = 37, nrow = 0))
cnames_exp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year","Quantile 50 year","Quantile 100 year","Quantile 200 year", "Brier 5 year", "Brier 10 year", "Brier 20 year","Brier 50 year","Brier 100 year","Brier 200 year", "FGP", "Quantile 5 year ams", "Quantile 10 year ams", "Quantile 20 year ams", "Quantile 50 year ams", "Quantile 100 year ams", "Quantile 200 year ams", "Brier 5 year ams", "Brier 10 year ams", "Brier 20 year ams", "Brier 50 year ams", "Brier 100 year ams", "Brier 200 year ams")
colnames(exp_gofvalues) <- cnames_exp

gumbel_gofvalues <- data.frame(matrix(ncol = 25, nrow = 0))
cnames_gum <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue",  "Quantile 5 year","Quantile 10 year","Quantile 20 year","Quantile 50 year","Quantile 100 year","Quantile 200 year", "Brier 5 year", "Brier 10 year", "Brier 20 year","Brier 50 year","Brier 100 year","Brier 200 year", "FGP")
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
#write.table(FGP, file = "FGP_values.txt", sep = "\ ")
#FGP_up <- read.table("FGP_values2.txt", header=T, sep="\ ")
#FGP_up$Station <- as.character(FGP_up$Station)

#stations <- stations[order(stations$Stations, stations$years), , drop=F]
felt_data <-read.table("felt_data.txt",header=T,sep=";")
FGP <- data.frame(felt_data$STASJON_NR, felt_data$QD_fgp, felt_data$QD_median, felt_data$latitude, felt_data$longitude, felt_data$AREAL_UTM33)
colnames(FGP) <- c("Station","FGP", "Average yearly flood", "lat", "long", "size")
#write.table(FGP, file = "FGP_values2.txt", sep = "\ ", dec = ".") #Before you do this you have to enter the text file manually and replace all ".0" with nothing
FGP_up <- read.table("FGP_values2.txt", header=T, sep="\ ", dec="." )
FGP_up$Station <- as.character(FGP_up$Station)
FGP_up <- FGP_up[order(FGP_up$Station, FGP_up$FGP, FGP_up$Average.yearly.flood, FGP_up$lat, FGP_up$long, FGP_up$size), , drop=F]
stations[, "FGP"] <- NA
stations[, "Average yearly flood"] <- NA
stations[, "lat"] <- NA
stations[, "long"] <- NA
stations[, "size"] <- NA
stations$FGP <- FGP_up$FGP
stations$lat <- FGP_up$lat
stations$long <- FGP_up$long
stations$`Average yearly flood` <- FGP_up$Average.yearly.flood
stations$size <- FGP_up$size
stations30 <- stations[!(stations$years < 30),]
}
for (s in 1:length(stations30$Stations)){
  ams_river <- ams_data[ams_data[,9] %in% stations30$Stations[s], ]
  pot_river <- pot_data[pot_data[,8] %in% stations30$Stations[s], ]
  
  ams_riv <- (by(ams_river$daily_ams.1, ams_river$year, max))
  ams_river <- ams_river[ams_river[,5] %in% ams_riv, ]
  ams_river <- ams_river[!duplicated(ams_river$year),]
  
  if(length(pot_river$FGP) == 0){
    next
  } else if(length(ams_river$year) < 30){
    next
  } 
  else {
    
    #ams_river <- ams_data[ams_data$station == "91.2", ]
    #pot_river <- pot_data[pot_data$station == "86.10", ]
    station_values <- data.frame(stations30$Stations[s],stations30$long[s],stations30$lat[s], stations30$size[s], stations30$FGP[s])
    colnames(station_values) <- c("Stations", "long", "lat", "size", "FGP")
    
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

for(z in 1:1){
#set.seed(2561)
set.seed(1871)
sam_pot_river <- pot_river[sample(nrow(pot_river)),]
set.seed(1923)
#set.seed(2635)
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
#set.seed(2145)
rbn_ams <- blockdf_ams[sample(nrow(blockdf_ams)),]
gsize_ams <- floor(length(bn_ams)/kfold)
bcum_ams = cumsum(rbn_ams$bsize_ams)

bn <- as.vector(pot_river$year)
bsize <- as.numeric(table(bn))
bsizevec <- as.data.frame(bsize)
ub <- unique(bn)
blockdf <- cbind(ub, bsizevec)
#set.seed(45)
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
  bsgp_50 = NULL
  bsgp_100 = NULL
  bsgp_200 = NULL
  qsgp_5 = NULL
  qsgp_10 = NULL
  qsgp_20 = NULL
  qsgp_50 = NULL
  qsgp_100 = NULL
  qsgp_200 = NULL
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
  bsexp_50 = NULL
  bsexp_100 = NULL
  bsexp_200 = NULL
  qsexp_5 = NULL
  qsexp_10 = NULL
  qsexp_20 = NULL
  qsexp_50 = NULL
  qsexp_100 = NULL
  qsexp_200 = NULL
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
  bsgev_50 = NULL
  bsgev_100 = NULL
  bsgev_200 = NULL
  qsgev_5 = NULL
  qsgev_10 = NULL
  qsgev_20 = NULL
  qsgev_50 = NULL
  qsgev_100 = NULL
  qsgev_200 = NULL
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
  bsgum_50 = NULL
  bsgum_100 = NULL
  bsgum_200 = NULL
  qsgum_5 = NULL
  qsgum_10 = NULL
  qsgum_20 = NULL
  qsgum_50 = NULL
  qsgum_100 = NULL
  qsgum_200 = NULL
  amsbsgp_5 = NULL
  amsbsgp_10 = NULL
  amsbsgp_20 = NULL
  amsbsgp_50 = NULL
  amsbsgp_100 = NULL
  amsbsgp_200 = NULL
  amsqsgp_5 = NULL
  amsqsgp_10 = NULL
  amsqsgp_20 = NULL
  amsqsgp_50 = NULL
  amsqsgp_100 = NULL
  amsqsgp_200 = NULL
  amsbsexp_5 = NULL
  amsbsexp_10 = NULL
  amsbsexp_20 = NULL
  amsbsexp_50 = NULL
  amsbsexp_100 = NULL
  amsbsexp_200 = NULL
  amsqsexp_5 = NULL
  amsqsexp_10 = NULL
  amsqsexp_20 = NULL
  amsqsexp_50 = NULL
  amsqsexp_100 = NULL
  amsqsexp_200 = NULL
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
  
  dimrivboot <- dim(ams_river)[1]/kfold
  gs_amsboot <- floor(dimrivboot)
  
  dimrivbootpot <- dim(pot_river)[1]/kfold
  gs_potboot <- floor(dimrivbootpot)
}
    
for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5]
  traindfyears <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 7]
  paramsgp <- gp_Lmom(traindf)#, threshold = sam_pot_river$threshold[2]
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
  return_gp_20[i] = (paramsgp$estimate[2]/paramsgp$estimate[3])*(1-(-log(1-(1/return_p$Periods[3])/testplotframe$`Floods per year`))^paramsgp$estimate[3])+pot_river$threshold[2]
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
  bsgp_50[i] <- bsgp_failsafe(testdf, threshold = return_gp_50[i], param = paramsgp$estimate, distr = "gp")
  bsgp_100[i] <- bsgp_failsafe(testdf, threshold = return_gp_100[i], param = paramsgp$estimate, distr = "gp")
  bsgp_200[i] <- bsgp_failsafe(testdf, threshold = return_gp_200[i], param = paramsgp$estimate, distr = "gp")
  
  qsgp_failsafe <- purrr::possibly(QS4NC, NA)
  
  qsgp_5[i] <- qsgp_failsafe(testdf, r.levels = return_gp_5[i], r.periods = return_p$Periods[1])
  qsgp_10[i] <- qsgp_failsafe(testdf, r.levels = return_gp_10[i], r.periods = return_p$Periods[2])
  qsgp_20[i] <- qsgp_failsafe(testdf, r.levels = return_gp_20[i], r.periods = return_p$Periods[3])
  qsgp_50[i] <- qsgp_failsafe(testdf, r.levels = return_gp_50[i], r.periods = return_p$Periods[4])
  qsgp_100[i] <- qsgp_failsafe(testdf, r.levels = return_gp_100[i], r.periods = return_p$Periods[5])
  qsgp_200[i] <- qsgp_failsafe(testdf, r.levels = return_gp_200[i], r.periods = return_p$Periods[6])
  
  qsgp5_av <- mean(qsgp_5)
  qsgp10_av <- mean(qsgp_10)
  qsgp20_av <- mean(qsgp_20)
  qsgp50_av <- mean(qsgp_50)
  qsgp100_av <- mean(qsgp_100)
  qsgp200_av <- mean(qsgp_200)
  bsgp5_av <- mean(bsgp_5)
  bsgp10_av <- mean(bsgp_10)
  bsgp20_av <- mean(bsgp_20)
  bsgp50_av <- mean(bsgp_50)
  bsgp100_av <- mean(bsgp_100)
  bsgp200_av <- mean(bsgp_200)
  
  qsgp5_av <- qsgp5_av/testplotframe$`Average yearly flood`
  qsgp10_av <- qsgp10_av/testplotframe$`Average yearly flood`
  qsgp20_av <- qsgp20_av/testplotframe$`Average yearly flood`
  qsgp50_av <- qsgp50_av/testplotframe$`Average yearly flood`
  qsgp100_av <- qsgp100_av/testplotframe$`Average yearly flood`
  qsgp200_av <- qsgp200_av/testplotframe$`Average yearly flood`
  
  low_values <- sam_ams_river[sam_ams_river$daily_ams.1 < sam_pot_river$threshold[2], ]
  low_vect <- as.vector(low_values$daily_ams.1)
  testdf_ams <- sam_ams_river[!(sam_ams_river[,8] %in% traindfyears), 5]
  testdf_ams<- append(testdf_ams, low_vect[i])
  testdf_ams <- testdf_ams[!is.na(testdf_ams)]
  
  amsbsgp_5[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_5[i], param = paramsgp$estimate, distr = "gp")
  amsbsgp_10[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_10[i], param = paramsgp$estimate, distr = "gp")
  amsbsgp_20[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_20[i], param = paramsgp$estimate, distr = "gp")
  amsbsgp_50[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_50[i], param = paramsgp$estimate, distr = "gp")
  amsbsgp_100[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_100[i], param = paramsgp$estimate, distr = "gp")
  amsbsgp_200[i] <- BS4NC_tryout(testdf_ams, threshold = return_gp_200[i], param = paramsgp$estimate, distr = "gp")
  
  amsqsgp_5[i] <- QS4NC(testdf_ams, r.levels = return_gp_5[i], r.periods = return_p$Periods[1])
  amsqsgp_10[i] <- QS4NC(testdf_ams, r.levels = return_gp_10[i], r.periods = return_p$Periods[2])
  amsqsgp_20[i] <- QS4NC(testdf_ams, r.levels = return_gp_20[i], r.periods = return_p$Periods[3])
  amsqsgp_50[i] <- QS4NC(testdf_ams, r.levels = return_gp_50[i], r.periods = return_p$Periods[4])
  amsqsgp_100[i] <- QS4NC(testdf_ams, r.levels = return_gp_100[i], r.periods = return_p$Periods[5])
  amsqsgp_200[i] <- QS4NC(testdf_ams, r.levels = return_gp_200[i], r.periods = return_p$Periods[6])
  
  amsqsgp5_av  <- mean(amsqsgp_5)/testplotframe$`Average yearly flood`
  amsqsgp10_av <- mean(amsqsgp_10)/testplotframe$`Average yearly flood`
  amsqsgp20_av <- mean(amsqsgp_20)/testplotframe$`Average yearly flood`
  amsqsgp50_av <- mean(amsqsgp_50)/testplotframe$`Average yearly flood`
  amsqsgp100_av <- mean(amsqsgp_100)/testplotframe$`Average yearly flood`
  amsqsgp200_av <- mean(amsqsgp_200)/testplotframe$`Average yearly flood`
  amsbsgp5_av <- mean(amsbsgp_5)
  amsbsgp10_av <- mean(amsbsgp_10)
  amsbsgp20_av <- mean(amsbsgp_20)
  amsbsgp50_av <- mean(amsbsgp_50)
  amsbsgp100_av <- mean(amsbsgp_100)
  amsbsgp200_av <- mean(amsbsgp_200)
}#gp

for (i in 1:kfold){
  i1 = (i-1)*gs_ams + 1
  i2 = i*gs_ams
  testdf <- sam_ams_river[(i1:i2), 5]
  traindf<- sam_ams_river[-(i1:i2), 5] 
  paramsgev <- gev_Lmom(traindf)
  cv_gev_ad[i] <- gof_ad(testdf, paramsgev$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
  cva_gev_ad <- mean(cv_gev_ad)
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
  bsgev_50[i] <- BS4NC_tryout(testdf, threshold = return_gev_50[i], param = paramsgev$estimate, distr = "gev")
  bsgev_100[i] <- BS4NC_tryout(testdf, threshold = return_gev_100[i], param = paramsgev$estimate, distr = "gev")
  bsgev_200[i] <- BS4NC_tryout(testdf, threshold = return_gev_200[i], param = paramsgev$estimate, distr = "gev")
  
  qsgev_5[i] <- QS4NC(testdf, r.levels = return_gev_5[i], r.periods = return_p$Periods[1])
  qsgev_10[i] <- QS4NC(testdf, r.levels = return_gev_10[i], r.periods = return_p$Periods[2])
  qsgev_20[i] <- QS4NC(testdf, r.levels = return_gev_20[i], r.periods = return_p$Periods[3])
  qsgev_50[i] <- QS4NC(testdf, r.levels = return_gev_50[i], r.periods = return_p$Periods[4])
  qsgev_100[i] <- QS4NC(testdf, r.levels = return_gev_100[i], r.periods = return_p$Periods[5])
  qsgev_200[i] <- QS4NC(testdf, r.levels = return_gev_200[i], r.periods = return_p$Periods[6])
  
  qsgev5_av <- mean(qsgev_5)
  qsgev10_av <- mean(qsgev_10)
  qsgev20_av <- mean(qsgev_20)
  qsgev50_av <- mean(qsgev_50)
  qsgev100_av <- mean(qsgev_100)
  qsgev200_av <- mean(qsgev_200)
  
  bsgev5_av <- mean(bsgev_5)
  bsgev10_av <- mean(bsgev_10)
  bsgev20_av <- mean(bsgev_20)
  bsgev50_av <- mean(bsgev_50)
  bsgev100_av <- mean(bsgev_100)
  bsgev200_av <- mean(bsgev_200)
  
  
  qsgev5_av <- qsgev5_av/testplotframe$`Average yearly flood`
  qsgev10_av <- qsgev10_av/testplotframe$`Average yearly flood`
  qsgev20_av <- qsgev20_av/testplotframe$`Average yearly flood`
  qsgev50_av <- qsgev50_av/testplotframe$`Average yearly flood`
  qsgev100_av <- qsgev100_av/testplotframe$`Average yearly flood`
  qsgev200_av <- qsgev200_av/testplotframe$`Average yearly flood`
}#gev

for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
  traindfyears <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 7]
  paramsexp <- exp_Lmom(traindf)#, threshold = sam_pot_river$threshold[2]
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
  bsexp_50[i] <- BS4NC_tryout(testdf, threshold = return_exp_50[i], param = paramsexp$estimate, distr = "exp")
  bsexp_100[i] <- BS4NC_tryout(testdf, threshold = return_exp_100[i], param = paramsexp$estimate, distr = "exp")
  bsexp_200[i] <- BS4NC_tryout(testdf, threshold = return_exp_200[i], param = paramsexp$estimate, distr = "exp")
  

  qsexp_5[i] <- QS4NC(testdf, r.levels = return_exp_5[i], r.periods = return_p$Periods[1])
  qsexp_10[i] <- QS4NC(testdf, r.levels = return_exp_10[i], r.periods = return_p$Periods[2])
  qsexp_20[i] <- QS4NC(testdf, r.levels = return_exp_20[i], r.periods = return_p$Periods[3])
  qsexp_50[i] <- QS4NC(testdf, r.levels = return_exp_50[i], r.periods = return_p$Periods[4])
  qsexp_100[i] <- QS4NC(testdf, r.levels = return_exp_100[i], r.periods = return_p$Periods[5])
  qsexp_200[i] <- QS4NC(testdf, r.levels = return_exp_200[i], r.periods = return_p$Periods[6])
  
  
  qsexp5_av <- mean(qsexp_5)
  qsexp10_av <- mean(qsexp_10)
  qsexp20_av <- mean(qsexp_20)
  qsexp50_av <- mean(qsexp_50)
  qsexp100_av <- mean(qsexp_100)
  qsexp200_av <- mean(qsexp_200)
  
  bsexp5_av <- mean(bsexp_5)
  bsexp10_av <- mean(bsexp_10)
  bsexp20_av <- mean(bsexp_20)
  bsexp50_av <- mean(bsexp_50)
  bsexp100_av <- mean(bsexp_100)
  bsexp200_av <- mean(bsexp_200)
  
  
  qsexp5_av <- qsexp5_av/testplotframe$`Average yearly flood`
  qsexp10_av <- qsexp10_av/testplotframe$`Average yearly flood`
  qsexp20_av <- qsexp20_av/testplotframe$`Average yearly flood`
  qsexp50_av <- qsexp50_av/testplotframe$`Average yearly flood`
  qsexp100_av <- qsexp100_av/testplotframe$`Average yearly flood`
  qsexp200_av <- qsexp200_av/testplotframe$`Average yearly flood`
  
  low_values <- sam_ams_river[sam_ams_river$daily_ams.1 < sam_pot_river$threshold[2], ]
  low_vect <- as.vector(low_values$daily_ams.1)
  testdf_ams <- sam_ams_river[!(sam_ams_river[,8] %in% traindfyears), 5]
  testdf_ams<- append(testdf_ams, low_vect[i])
  testdf_ams <- testdf_ams[!is.na(testdf_ams)]
  
  amsbsexp_5[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_5[i], param = paramsexp$estimate, distr = "exp")
  amsbsexp_10[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_10[i], param = paramsexp$estimate, distr = "exp")
  amsbsexp_20[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_20[i], param = paramsexp$estimate, distr = "exp")
  amsbsexp_50[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_50[i], param = paramsexp$estimate, distr = "exp")
  amsbsexp_100[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_100[i], param = paramsexp$estimate, distr = "exp")
  amsbsexp_200[i] <- BS4NC_tryout(testdf_ams, threshold = return_exp_200[i], param = paramsexp$estimate, distr = "exp")
  
    
  amsqsexp_5[i] <- QS4NC(testdf_ams, r.levels = return_exp_5[i], r.periods = return_p$Periods[1])
  amsqsexp_10[i] <- QS4NC(testdf_ams, r.levels = return_exp_10[i], r.periods = return_p$Periods[2])
  amsqsexp_20[i] <- QS4NC(testdf_ams, r.levels = return_exp_20[i], r.periods = return_p$Periods[3])
  amsqsexp_50[i] <- QS4NC(testdf_ams, r.levels = return_exp_50[i], r.periods = return_p$Periods[4])
  amsqsexp_100[i] <- QS4NC(testdf_ams, r.levels = return_exp_100[i], r.periods = return_p$Periods[5])
  amsqsexp_200[i] <- QS4NC(testdf_ams, r.levels = return_exp_200[i], r.periods = return_p$Periods[6])
  
  
  amsqsexp5_av <- mean(amsqsexp_5)/testplotframe$`Average yearly flood`
  amsqsexp10_av <- mean(amsqsexp_10)/testplotframe$`Average yearly flood`
  amsqsexp20_av <- mean(amsqsexp_20)/testplotframe$`Average yearly flood`
  amsqsexp50_av <- mean(amsqsexp_50)/testplotframe$`Average yearly flood`
  amsqsexp100_av <- mean(amsqsexp_100)/testplotframe$`Average yearly flood`
  amsqsexp200_av <- mean(amsqsexp_200)/testplotframe$`Average yearly flood`
  
  amsbsexp5_av <- mean(amsbsexp_5)
  amsbsexp10_av <- mean(amsbsexp_10)
  amsbsexp20_av <- mean(amsbsexp_20)
  amsbsexp50_av <- mean(amsbsexp_50)
  amsbsexp100_av <- mean(amsbsexp_100)
  amsbsexp200_av <- mean(amsbsexp_200)
  
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
  bsgum_50[i] <- BS4NC_tryout(testdf, threshold = return_gum_50[i], param = paramsgum$estimate, distr = "gumbel")
  bsgum_100[i] <- BS4NC_tryout(testdf, threshold = return_gum_100[i], param = paramsgum$estimate, distr = "gumbel")
  bsgum_200[i] <- BS4NC_tryout(testdf, threshold = return_gum_200[i], param = paramsgum$estimate, distr = "gumbel")
  
  
  qsgum_5[i] <- QS4NC(testdf, r.levels = return_gum_5[i], r.periods = return_p$Periods[1])
  qsgum_10[i] <- QS4NC(testdf, r.levels = return_gum_10[i], r.periods = return_p$Periods[2])
  qsgum_20[i] <- QS4NC(testdf, r.levels = return_gum_20[i], r.periods = return_p$Periods[3])
  qsgum_50[i] <- QS4NC(testdf, r.levels = return_gum_50[i], r.periods = return_p$Periods[4])
  qsgum_100[i] <- QS4NC(testdf, r.levels = return_gum_100[i], r.periods = return_p$Periods[5])
  qsgum_200[i] <- QS4NC(testdf, r.levels = return_gum_200[i], r.periods = return_p$Periods[6])
  
  
  qsgum5_av <- mean(qsgum_5)
  qsgum10_av <- mean(qsgum_10)
  qsgum20_av <- mean(qsgum_20)
  qsgum50_av <- mean(qsgum_50)
  qsgum100_av <- mean(qsgum_100)
  qsgum200_av <- mean(qsgum_200)
  
  bsgum5_av <- mean(bsgum_5)
  bsgum10_av <- mean(bsgum_10)
  bsgum20_av <- mean(bsgum_20)
  bsgum50_av <- mean(bsgum_50)
  bsgum100_av <- mean(bsgum_100)
  bsgum200_av <- mean(bsgum_200)
    
  qsgum5_av <- qsgum5_av/testplotframe$`Average yearly flood`
  qsgum10_av <- qsgum10_av/testplotframe$`Average yearly flood`
  qsgum20_av <- qsgum20_av/testplotframe$`Average yearly flood`
  qsgum50_av <- qsgum50_av/testplotframe$`Average yearly flood`
  qsgum100_av <- qsgum100_av/testplotframe$`Average yearly flood`
  qsgum200_av <- qsgum200_av/testplotframe$`Average yearly flood`
  
}#gum
    
for (i in 1:1000){
      sam_bootpot <- sample(pot_river$flood.1, replace = TRUE)
      #i1 = 1
      #i2 = 1*gs_potboot
      #testdf <- sam_bootpot[(i1:i2)]
      #traindf<- sam_bootpot[-(i1:i2)]
      paramexpboot <- exp_Lmom(sam_bootpot)
      paramgpboot <- gp_Lmom(sam_bootpot)
      
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
    
for (i in 1:1000){
      sam_bootams <- sample(ams_river$daily_ams.1, replace = TRUE)
      #i1 = 1
      #i2 = 1*gs_amsboot
      #testdf <- sam_bootams[(i1:i2)]
      #traindf<- sam_bootams[-(i1:i2)]
      paramgumboot <- gumbel_Lmom(sam_bootams)
      paramgevboot <- gev_Lmom(sam_bootams)
      
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

for (i in 1:1){
  CV5gum <- CoV(return5_gumboo)
  CV10gum <- CoV(return10_gumboo)
  CV20gum <- CoV(return20_gumboo)
  CV50gum <- CoV(return50_gumboo)
  CV100gum <- CoV(return100_gumboo)
  CV200gum <- CoV(return200_gumboo)
  
  CV5gev <- CoV(return5_gevboo)
  CV10gev <- CoV(return10_gevboo)
  CV20gev <- CoV(return20_gevboo)
  CV50gev <- CoV(return50_gevboo)
  CV100gev <- CoV(return100_gevboo)
  CV200gev <- CoV(return200_gevboo)
  
  CV5gp <- CoV(return5_gpboo)
  CV10gp <- CoV(return10_gpboo)
  CV20gp <- CoV(return20_gpboo)
  CV50gp <- CoV(return50_gpboo)
  CV100gp <- CoV(return100_gpboo)
  CV200gp <- CoV(return200_gpboo)
  
  CV5exp <- CoV(return5_expboo)
  CV10exp <- CoV(return10_expboo)
  CV20exp <- CoV(return20_expboo)
  CV50exp <- CoV(return50_expboo)
  CV100exp <- CoV(return100_expboo)
  CV200exp <- CoV(return200_expboo)
  
}
        
for(y in 1:1){
KS <- data.frame("GEV" = cva_gev_ks, "GUM" = cva_gum_ks, "EXP" = cva_exp_ks, "GP" = cva_gp_ks)
KSmin <- data.frame(matrix(ncol = 1, nrow = 1))
KSmin <- colnames(KS)[apply(KS,1,which.min)]
KSmin <- data.frame(KSmin)

BS <- data.frame("GEV" = bsgev20_av, "GUM" = bsgum20_av, "EXP" = bsexp20_av, "GP" = bsgp20_av, "EXP AMS" = amsbsexp20_av, "GP AMS" = amsbsgp20_av)
BSmin <- data.frame(matrix(ncol = 1, nrow = 1))
BSmin <- colnames(BS)[apply(BS,1,which.min)]
BSmin <- data.frame(BSmin)

QS <- data.frame("GEV" = qsgev20_av, "GUM" = qsgum20_av, "EXP" = qsexp20_av, "GP" = qsgp20_av, "EXP AMS" = amsqsexp20_av, "GP AMS" = amsqsgp20_av)
QSmin <- data.frame(matrix(ncol = 1, nrow = 1))
QSmin <- colnames(QS)[apply(QS,1,which.min)]
QSmin <- data.frame(QSmin)

BS2 <- data.frame("GEV" = bsgev20_av, "GUM" = bsgum20_av, "EXP AMS" = amsbsexp20_av, "GP AMS" = amsbsgp20_av)
BSmin2 <- data.frame(matrix(ncol = 1, nrow = 1))
BSmin2 <- colnames(BS2)[apply(BS2,1,which.min)]
BSmin2 <- data.frame(BSmin2)

QS2 <- data.frame("GEV" = qsgev20_av, "GUM" = qsgum20_av, "EXP AMS" = amsqsexp20_av, "GP AMS" = amsqsgp20_av)
QSmin2 <- data.frame(matrix(ncol = 1, nrow = 1))
QSmin2 <- colnames(QS2)[apply(QS2,1,which.min)]
QSmin2 <- data.frame(QSmin2)

AD <- data.frame("GEV" = cva_gev_ad, "GUM" = cva_gum_ad, "EXP" = cva_exp_ad, "GP" = cva_gp_ad)
ADmin <- data.frame(matrix(ncol = 1, nrow = 1))
ADmin <- colnames(AD)[apply(AD,1,which.min)]
ADmin <- data.frame(ADmin)
  
amsFGP <- mean(ams_river$FGP, na.rm=T)  
potFGP <- mean(pot_river$FGP, na.rm=T)

newrow_station <-  data.frame("nve_nb" = station_values$Stations, "long" = station_values$long, "lat" = station_values$lat,
                              "size" = station_values$size,
                              "FGP" = potFGP, "FPY" = testplotframe$`Floods per year`[1],
                              "KS" = KSmin$KS, "AD" = ADmin$AD, "BS"=BSmin$BSmin, "QS"=QSmin$QSmin, "BS2"=BSmin2$BSmin2, "QS2"=QSmin2$QSmin2) 
station <- rbind(newrow_station, station)
write.table (station, file = "200station values50 final parex.txt", sep = "\ ")




newRow_gev <- data.frame("Stations" = testplotframe$Station[1],"Years" = testplotframe$years[1],
                         "ADscore_pot" = AD_gevpot, "KSscore_pot" = KS_gevpot,
                         "ADscore_ams" = AD_gevams, "KSscore_ams" = KS_gevams, 
                         "Floods per year" = testplotframe$`Floods per year`[1], 
                         "CV AD score" = cva_gev_ad, "CV AD pvalue" = cvp_gev_ad, 
                         "CV KS score" = cva_gev_ks, "CV KS pvalue" = cvp_gev_ks,
                         "Quantile 5 year" = qsgev5_av, "Quantile 10 year" = qsgev10_av,
                         "Quantile 20 year"= qsgev20_av,"Quantile 50 year"= qsgev50_av,
                         "Quantile 100 year"= qsgev100_av,"Quantile 200 year"= qsgev200_av,
                         "Brier 5 year"= bsgev5_av, 
                         "Brier 10 year" = bsgev10_av, "Brier 20 year" = bsgev20_av, "Brier 50 year" = bsgev50_av
                         , "Brier 100 year" = bsgev100_av , "Brier 200 year" = bsgev200_av,
                         "FGP" = amsFGP, "size" = station_values$size)
gev_gofvalues <- rbind(newRow_gev, gev_gofvalues)
write.table (gev_gofvalues, file = "200gev values50 final parex.txt", sep = "\ ")

newRow_gp <- data.frame("Stations" = testplotframe$Station[1],"Years" = testplotframe$years[1],
                        "ADscore_pot" = AD_gppot, "KSscore_pot" = KS_gppot, 
                        "ADscore_ams" = AD_gpams, "KSscore_ams" = KS_gpams, 
                        "Floods per year" = testplotframe$`Floods per year`[1],
                        "CV AD score" = cva_gp_ad, "CV AD pvalue" = cvp_gp_ad,
                        "CV KS score" = cva_gp_ks, "CV KS pvalue" = cvp_gp_ks,
                        "Quantile 5 year" = qsgp5_av, "Quantile 10 year" = qsgp10_av, 
                        "Quantile 20 year"= qsgp20_av,"Quantile 50 year"= qsgp50_av,
                        "Quantile 100 year"= qsgp100_av,
                        "Quantile 200 year"= qsgp200_av,"Brier 5 year"= bsgp5_av, 
                        "Brier 10 year" = bsgp10_av, "Brier 20 year" = bsgp20_av,
                        "Brier 50 year" = bsgp50_av,"Brier 100 year" = bsgp100_av,
                        "Brier 200 year" = bsgp200_av,
                        "Quantile 5 year ams" = amsqsgp5_av, "Quantile 10 year ams"= amsqsgp10_av,
                        "Quantile 20 year ams" = amsqsgp20_av,"Quantile 50 year ams" = amsqsgp50_av,
                        "Quantile 100 year ams" = amsqsgp100_av,
                        "Quantile 200 year ams" = amsqsgp200_av,"Brier 5 year ams" = amsbsgp5_av, 
                        "Brier 10 year ams"= amsbsgp10_av, 
                        "Brier 20 year ams"= amsbsgp20_av,"Brier 50 year ams"= amsbsgp50_av,
                        "Brier 100 year ams"= amsbsgp100_av,"Brier 200 year ams"= amsbsgp200_av,
                        "FGP" = potFGP, "size" = station_values$size)
gp_gofvalues <- rbind(newRow_gp, gp_gofvalues)
write.table (gp_gofvalues, file = "200gp values50 final parex.txt", sep = "\ ")

newRow_gum <- data.frame("Stations" = testplotframe$Station[1], "Years" = testplotframe$years[1], 
                         "ADscore_pot" = AD_gumpot, "KSscore_pot" = KS_gumpot, 
                         "ADscore_ams" = AD_gumams, "KSscore_ams" = KS_gumams,
                         "Floods per year" = testplotframe$`Floods per year`[1],
                         "CV AD score" = cva_gum_ad, "CV AD pvalue" = cvp_gum_ad,
                         "CV KS score" = cva_gum_ks, "CV KS pvalue" = cvp_gum_ks,
                         "Quantile 5 year" = qsgum5_av, "Quantile 10 year" = qsgum10_av,
                         "Quantile 20 year"= qsgum20_av,"Quantile 50 year"= qsgum50_av,
                         "Quantile 100 year"= qsgum100_av,"Quantile 200 year"= qsgum200_av,
                         "Brier 5 year"= bsgum5_av,
                         "Brier 10 year" = bsgum10_av, "Brier 20 year" = bsgum20_av,
                         "Brier 50 year" = bsgum50_av,"Brier 100 year" = bsgum100_av,
                         "Brier 200 year" = bsgum200_av,
                         "FGP" = amsFGP, "size" = station_values$size)
gumbel_gofvalues <- rbind(newRow_gum, gumbel_gofvalues)
write.table (gumbel_gofvalues, file = "200gumbel values50 final parex.txt", sep = "\ ")

newRow_exp <- data.frame("Stations" = testplotframe$Station[1], "Years" = testplotframe$years[1],
                         "ADscore_pot" = AD_exppot, "KSscore_pot" = KS_exppot,
                         "ADscore_ams" = AD_expams, "KSscore_ams" = KS_expams, 
                         "Floods per year" = testplotframe$`Floods per year`[1],
                         "CV AD score" = cva_exp_ad, "CV AD pvalue" = cvp_exp_ad,
                         "CV KS score" = cva_exp_ks, "CV KS pvalue" = cvp_exp_ks, 
                         "Quantile 5 year" = qsexp5_av, "Quantile 10 year" = qsexp10_av, 
                         "Quantile 20 year"= qsexp20_av,"Quantile 50 year"= qsexp50_av,
                         "Quantile 100 year"= qsexp100_av,"Quantile 200 year"= qsexp200_av,
                         "Brier 5 year"= bsexp5_av,
                         "Brier 10 year" = bsexp10_av, "Brier 20 year" = bsexp20_av,
                         "Brier 50 year" = bsexp50_av, "Brier 100 year" = bsexp100_av,
                         "Brier 200 year" = bsexp200_av,
                         "Quantile 5 year ams" = amsqsexp5_av, "Quantile 10 year ams"= amsqsexp10_av,
                         "Quantile 20 year ams" = amsqsexp20_av,"Quantile 50 year ams" = amsqsexp50_av,
                         "Quantile 100 year ams" = amsqsexp100_av,"Quantile 200 year ams" = amsqsexp200_av,
                         "Brier 5 year ams" = amsbsexp5_av, 
                         "Brier 10 year ams"= amsbsexp10_av, "Brier 20 year ams"= amsbsexp20_av,
                         "Brier 50 year ams"= amsbsexp50_av,"Brier 100 year ams"= amsbsexp100_av,
                         "Brier 200 year ams"= amsbsexp200_av,
                         "FGP" = potFGP, "size" = station_values$size)
exp_gofvalues <- rbind(newRow_exp, exp_gofvalues)
write.table (exp_gofvalues, file = "200exp values50 final parex.txt", sep = "\ ")

newRow_return <- data.frame("Station" = testplotframe$Station[1],"5 GP" = returngp5_av, "5 EXP"= returnexp5_av, "5 GEV"= returngev5_av,
                            "5 GUM"= returngum5_av,"10 GP"= returngp10_av, "10 EXP"= returnexp10_av,
                            "10 GEV"= returngev10_av,"10 GUM"= returngum10_av,"20 GP"= returngp20_av,
                            "20 EXP"= returnexp20_av, "20 GEV"= returngev20_av,"20 GUM"= returngum20_av,
                            "50 GP"= returngp50_av, "50 EXP"= returnexp50_av, "50 GEV"= returngev50_av,
                            "50 GUM"= returngum50_av, "100 GP"= returngp100_av,
                            "100 EXP"= returnexp100_av, "100 GEV"= returngev100_av,"100 GUM"= returngum100_av,
                            "200 GP"= returngp200_av,
                            "200 EXP"= returnexp200_av, "200 GEV"= returngev200_av,"200 GUM"= returngum200_av)
return_l <- rbind(newRow_return, return_l)
write.table (return_l, file = "200Return level averages50 final parex.txt", sep = "\ ")

newRow_CV <- data.frame("Floods per year" = testplotframe$`Floods per year`[1],"5 GUM" = CV5gum,"5 GEV" = CV5gev,"5 EXP" = CV5exp,"5 GP" = CV5gp,
                        "10 GUM" = CV10gum,"10 GEV" = CV10gev,"10 EXP" = CV10exp,"10 GP" = CV10gp,
                        "20 GUM" = CV20gum,"20 GEV" = CV20gev,"20 EXP" = CV20exp,"20 GP" = CV20gp,
                        "50 GUM" = CV50gum,"50 GEV" = CV50gev,"50 EXP" = CV50exp,"50 GP" = CV50gp,
                        "100 GUM" = CV100gum,"100 GEV" = CV100gev,"100 EXP" = CV100exp,
                        "100 GP" = CV100gp,"200 GUM" = CV200gum,"200 GEV" = CV200gev,
                        "200 EXP" = CV200exp,"200 GP" = CV200gp)
CoefficientVariance <- rbind(newRow_CV, CoefficientVariance)
write.table (CoefficientVariance, file = "200Coefficient of variation stability test50 final parex.txt", sep = "\ ")

}
  }
}
   
