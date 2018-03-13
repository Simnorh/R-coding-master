setwd("D:/Master/Thesis/Data")
for (h in 1:1){
gev_gofvalues <- data.frame(matrix(ncol = 17, nrow = 0))
cnames_gev <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year")
colnames(gev_gofvalues) <- cnames_gev

gp_gofvalues <- data.frame(matrix(ncol = 17, nrow = 0))
cnames_gp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year")
colnames(gp_gofvalues) <- cnames_gp

exp_gofvalues <- data.frame(matrix(ncol = 17, nrow = 0))
cnames_exp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year")
colnames(exp_gofvalues) <- cnames_exp

gumbel_gofvalues <- data.frame(matrix(ncol = 17, nrow = 0))
cnames_gum <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year", "CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year")
colnames(gumbel_gofvalues) <- cnames_gum

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
stations30 <- stations[!(stations$years < 30),]
}
#for (s in 1:length(stations30))
#ams_river <- ams_data[ams_data[,8] %in% stations30$Stations[s], ]
#pot_river <- pot_data[pot_data[,8] %in% stations30$Stations[s], ]
ams_river <- ams_data[ams_data$station == "311.16", ]
pot_river <- pot_data[pot_data$station == "311.16", ]

for(l in 1:1){
testplotframe <- as.data.frame(ams_river$station)
colnames(testplotframe)[1] <- "Stations"
testplotframe <- testplotframe %>% add_count(Stations)
colnames(testplotframe)[2] <- "years"
testplotframe <- dplyr::distinct(testplotframe)
Floods_year <- data.frame(nrow(pot_river)/testplotframe$years)
testplotframe <- cbind(testplotframe, Floods_year)
colnames(testplotframe)[3] <- "Floods per year"
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

if(is.na(AD_gppot) || is.na(KS_gppot)){
  gppar_pot <- gp_Lmom(pot_river$flood.1, threshold = pot_river$threshold[2])
  gppar_ams <- gp_Lmom(ams_river$daily_ams.1)
  
  AD_gpams <- gof_ad(ams_river$daily_ams.1, gppar_ams$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
  AD_gppot <- gof_ad(pot_river$flood.1, gppar_pot$estimate, distr = "gp", test.stat=TRUE, p.value=FALSE)
  KS_gpams <- gof_ks(ams_river$daily_ams.1, gppar_ams$estimate, distr = "gp", test.stat = TRUE, p.value = FALSE)
  KS_gppot <- gof_ks(pot_river$flood.1, gppar_pot$estimate, distr = "gp", test.stat = TRUE, p.value = FALSE)
}

if(is.na(AD_exppot) || is.na(KS_exppot)){
  exppar_pot <- exp_Lmom(pot_river$flood.1 , threshold = pot_river$threshold[2])
  exppar_ams <- exp_Lmom(ams_river$daily_ams.1, threshold = NA)
  
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

return_p<- list(Periods = c(5, 10, 20))
kfold = 10
dimrivams <- dim(sam_ams_river)[1]/kfold
gs_ams <- floor(dimrivams)

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
  bsgum_5 = NULL
  bsgum_10 = NULL
  bsgum_20 = NULL
  qsgum_5 = NULL
  qsgum_10 = NULL
  qsgum_20 = NULL
  
}

for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
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

if(is.na(cva_gp_ad) || is.na(cvp_gp_ad) || is.na(cva_gp_ks) || is.na(cvp_gp_ad)){
  for (i in 1:kfold){
    i1 = which.min(abs((i-1)*gsize+1-bcum))
    i2 = which.min(abs(i*gsize-bcum))
    testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
    traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
    paramsgp <- gp_Lmom(traindf, threshold = pot_river$threshold[2])
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
}

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

for (i in 1:kfold){
  i1 = which.min(abs((i-1)*gsize+1-bcum))
  i2 = which.min(abs(i*gsize-bcum))
  testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
  traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
  paramsexp <- exp_Lmom(traindf)
  cv_exp_ad[i] <- gof_ad(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
  cva_exp_ad <- mean(cv_exp_ad)
  cv_exp_pad[i] <- gof_ad(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
  cvp_exp_ad <- mean(cv_exp_pad)
  
  cv_exp_ks[i] <- gof_ks(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
  cva_exp_ks <- mean(cv_exp_ks)
  cv_exp_pks[i] <- gof_ks(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
  cvp_exp_ks <- mean(cv_exp_pks)
  
  
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

if(is.na(cva_exp_ad) || is.na(cvp_exp_ad) || is.na(cva_exp_ks) || is.na(cvp_exp_ad)){
  for (i in 1:kfold){
    i1 = which.min(abs((i-1)*gsize+1-bcum))
    i2 = which.min(abs(i*gsize-bcum))
    testdf <- sam_pot_river[sam_pot_river[,7] %in% rbn$ub[i1:i2], 5]
    traindf <- sam_pot_river[!(sam_pot_river[,7] %in% rbn$ub[i1:i2]), 5] 
    paramsexp <- exp_Lmom(traindf, threshold = pot_river$threshold[2])
    cv_exp_ad[i] <- gof_ad(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
    cva_exp_ad <- mean(cv_exp_ad)
    cv_exp_pad[i] <- gof_ad(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
    cvp_exp_ad <- mean(cv_exp_pad)
    
    cv_exp_ks[i] <- gof_ks(testdf, paramsexp$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
    cva_exp_ks <- mean(cv_exp_ks)
    cv_exp_pks[i] <- gof_ks(testdf, paramsexp$estimate, distr = "exp", test.stat=FALSE, p.value=TRUE)
    cvp_exp_ks <- mean(cv_exp_pks)
    
    
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
}

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
  
  returngum5_av <- mean(return_gum_5)
  returngum10_av <- mean(return_gum_10)
  returngum20_av <- mean(return_gum_20)
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
  bsgum5_av <- mean(bsgum_5)
  bsgum10_av <- mean(bsgum_10)
  bsgum20_av <- mean(bsgum_20)
}#gum

for(y in 1:1){
newRow_gev <- data.frame("Station" = testplotframe$Stations,"Years" = testplotframe$years, "ADscore_pot" = AD_gevpot, "KSscore_pot" = KS_gevpot, "ADscore_ams" = AD_gevams, "KSscore_ams" = KS_gevams, "Floods per year" = testplotframe$`Floods per year`, "CV AD score" = cva_gev_ad, "CV AD pvalue" = cvp_gev_ad, "CV KS score" = cva_gev_ks, "CV KS pvalue" = cvp_gev_ks, "Quantile 5 year" = qsgev5_av, "Quantile 10 year" = qsgev10_av, "Quantile 20 year"= qsgev20_av, "Brier 5 year"= bsgev5_av, "Brier 10 year" = bsgev10_av, "Brier 20 year" = bsgev20_av)
gev_gofvalues <- rbind(newRow_gev, gev_gofvalues)
write.table (gev_gofvalues, file = "gev gofvalues.txt", sep = "\ ")

newRow_gp <- data.frame("Station" = testplotframe$Stations,"Years" = testplotframe$years, "ADscore_pot" = AD_gppot, "KSscore_pot" = KS_gppot, "ADscore_ams" = AD_gpams, "KSscore_ams" = KS_gpams, "Floods per year" = testplotframe$`Floods per year`, "CV AD score" = cva_gp_ad, "CV AD pvalue" = cvp_gp_ad, "CV KS score" = cva_gp_ks, "CV KS pvalue" = cvp_gp_ks, "Quantile 5 year" = qsgp5_av, "Quantile 10 year" = qsgp10_av, "Quantile 20 year"= qsgp20_av, "Brier 5 year"= bsgp5_av, "Brier 10 year" = bsgp10_av, "Brier 20 year" = bsgp20_av)
gp_gofvalues <- rbind(newRow_gp, gp_gofvalues)
write.table (gp_gofvalues, file = "gp gofvalues.txt", sep = "\ ")

newRow_gum <- data.frame("Station" = testplotframe$Stations, "Years" = testplotframe$years, "ADscore_pot" = AD_gumpot, "KSscore_pot" = KS_gumpot, "ADscore_ams" = AD_gumams, "KSscore_ams" = KS_gumams, "Floods per year" = testplotframe$`Floods per year`, "CV AD score" = cva_gum_ad, "CV AD pvalue" = cvp_gum_ad, "CV KS score" = cva_gum_ks, "CV KS pvalue" = cvp_gum_ks, "Quantile 5 year" = qsgum5_av, "Quantile 10 year" = qsgum10_av, "Quantile 20 year"= qsgum20_av, "Brier 5 year"= bsgum5_av, "Brier 10 year" = bsgum10_av, "Brier 20 year" = bsgum20_av)
gumbel_gofvalues <- rbind(newRow_gum, gumbel_gofvalues)
write.table (gumbel_gofvalues, file = "gumbel gofvalues.txt", sep = "\ ")

newRow_exp <- data.frame("Station" = testplotframe$Stations, "Years" = testplotframe$years, "ADscore_pot" = AD_exppot, "KSscore_pot" = KS_exppot, "ADscore_ams" = AD_expams, "KSscore_ams" = KS_expams, "Floods per year" = testplotframe$`Floods per year`, "CV AD score" = cva_exp_ad, "CV AD pvalue" = cvp_exp_ad, "CV KS score" = cva_exp_ks, "CV KS pvalue" = cvp_exp_ks, "Quantile 5 year" = qsexp5_av, "Quantile 10 year" = qsexp10_av, "Quantile 20 year"= qsexp20_av, "Brier 5 year"= bsexp5_av, "Brier 10 year" = bsexp10_av, "Brier 20 year" = bsexp20_av)
exp_gofvalues <- rbind(newRow_exp, exp_gofvalues)
write.table (exp_gofvalues, file = "exp gofvalues.txt", sep = "\ ")
}

