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
