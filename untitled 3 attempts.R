setwd("D:/Master/Thesis/Data")
for (h in 1:1){
  
  gev_gofvaluestry <- data.frame(matrix(ncol = 15, nrow = 0))
  cnames_gev <- c("Stations", "Years", "ADscore_ams","KSscore_ams","CV AD score","CV AD pvalue", "CV KS score", "CV KS pvalue", "Quantile 5 year","Quantile 10 year","Quantile 20 year", "Brier 5 year", "Brier 10 year", "Brier 20 year", "FGP")
  colnames(gev_gofvaluestry) <- cnames_gev

  ams_data <-read.table("ams_and_fgp.txt",header=T,sep="\ ")
  
  ams_years <- str_split_fixed(ams_data$daily_ams_dates, "-", 3)
  cnames_years <- c("year", "month", "date")
  colnames(ams_years) <- cnames_years
  ams_years <- subset(ams_years, select =c(year))
  ams_data <- cbind(ams_data, ams_years)
  
  ams_data$station <- paste(ams_data$regine, ams_data$main, sep=".")
  
  stations <- as.data.frame(ams_data$station)
  colnames(stations)[1] <- "Stations"
  stations <- stations %>% add_count(Stations)
  colnames(stations)[2] <- "years"
  stations <- dplyr::distinct(stations)
  #write.table(FGP, file = "FGP_values.txt", sep = "\ ")
  FGP_up <- read.table("FGP_values.txt", header=T, sep="\ ")
  FGP_up$Station <- as.character(FGP_up$Station)
  
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
    for(l in 1:1){
      testplotframe <- as.data.frame(ams_river$station)
      colnames(testplotframe)[1] <- "Station"
      testplotframe <- testplotframe %>% add_count(Station)
      colnames(testplotframe)[2] <- "years"
      testplotframe <- dplyr::distinct(testplotframe)
      FGP_station <- stations30[stations30$Stations %in% ams_river$station[1], 3]
      testplotframe <- cbind(testplotframe, FGP_station)
      colnames(testplotframe)[3] <- "FGP"
      AV_flood_station <- stations30[stations30$Stations %in% ams_river$station[1], 4]
      testplotframe <- cbind(testplotframe, AV_flood_station)
      colnames(testplotframe)[4] <- "Average yearly flood"
    }
    
    for(t in 1:1){
     
      gevpar_ams <- gev_Lmom(ams_river$daily_ams.1)
      #param_estimate <- gev_Lmom (resampled_data$daily_ams.1)
      
      AD_gevams <- gof_ad(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "gev", test.stat=TRUE, p.value=FALSE)
      KS_gevams <- gof_ks(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "gev", test.stat = TRUE, p.value = FALSE)
    }
    for(u in 1:1){
      AD_values_gev_ams <- data.frame(CS = NA, KS = KS_gevams, AD = AD_gevams)
   
    }    
    #plot_all (pot_river$flood.1,GOF.list = AD_values_gp_pot,param = gppar_pot,distr = "gp", method = "ad")
    for(z in 1:1){
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
      
    }
    
    for(q in 1:1){
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
      
      return5_gevboo = NA
      return10_gevboo = NA
      return20_gevboo = NA
      return50_gevboo = NA
      return100_gevboo = NA
      return200_gevboo = NA
      
      dimrivboot <- dim(ams_river)[1]/kfold
      gs_amsboot <- floor(dimrivboot)
    }
    
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
      
      return_gev_5[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[1])))^(paramsgev$estimate[3]))
      #quantlistgp_5[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_5-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gev_10[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[2])))^(paramsgev$estimate[3]))
      #quantlistgp_10[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_10-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      
      return_gev_20[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[3])))^(paramsgev$estimate[3]))
      #quantlistgp_20[i] = exp(-testplotframe$`Floods per year`*(1-(1-(1-paramsgp$estimate[3]*((return_gp_20-paramsgp$estimate[1])/paramsgp$estimate[2]))^(1/paramsgp$estimate[3]))))
      return_gev_50[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[4])))^(paramsgev$estimate[3]))
      return_gev_100[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[5])))^(paramsgev$estimate[3]))
      return_gev_200[i] = paramsgev$estimate[1] + (paramsgev$estimate[2]/(paramsgev$estimate[3]))*(1-(-log(1-(1/return_p$Periods[6])))^(paramsgev$estimate[3]))
      
      returngev5_av <- mean(return_gev_5)
      returngev10_av <- mean(return_gev_10)
      returngev20_av <- mean(return_gev_20)
      returngev50_av <- mean(return_gev_50)
      returngev100_av <- mean(return_gev_100)
      returngev200_av <- mean(return_gev_200)
    
      
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
      
      qsgev5_av <- qsgev5_av/testplotframe$`Average yearly flood`
      qsgev10_av <- qsgev10_av/testplotframe$`Average yearly flood`
      qsgev20_av <- qsgev20_av/testplotframe$`Average yearly flood`
    }#gev
    
    for (i in 1:1000){
      sam_bootams <- sample(ams_river$daily_ams.1, replace = TRUE)
      #i1 = 1
      #i2 = 1*gs_amsboot
      #testdf <- sam_bootams[(i1:i2)]
      #traindf<- sam_bootams[-(i1:i2)]
      paramgevboot <- gev_Lmom(sam_bootams)
      
      return5_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/5)))^(-paramgevboot$estimate[3]))
      return10_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/10)))^(-paramgevboot$estimate[3]))
      return20_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/20)))^(-paramgevboot$estimate[3]))
      return50_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/50)))^(-paramgevboot$estimate[3]))
      return100_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/100)))^(-paramgevboot$estimate[3]))
      return200_gevboo[i] = paramgevboot$estimate[1] + (paramgevboot$estimate[2]/(-paramgevboot$estimate[3]))*(1-(-log(1-(1/200)))^(-paramgevboot$estimate[3]))
    }
    
    for (i in 1:1){
      
      CV5gev <- CoV(return5_gevboo)
      CV10gev <- CoV(return10_gevboo)
      CV20gev <- CoV(return20_gevboo)
      CV50gev <- CoV(return50_gevboo)
      CV100gev <- CoV(return100_gevboo)
      CV200gev <- CoV(return200_gevboo)
      
    }
    
    for(y in 1:1){
      newRow_gev <- data.frame("Stations" = testplotframe$Station[1],"Years" = testplotframe$years[1],
                               "ADscore_ams" = AD_gevams, "KSscore_ams" = KS_gevams,
                               "CV AD score" = cva_gev_ad, "CV AD pvalue" = cvp_gev_ad, 
                               "CV KS score" = cva_gev_ks, "CV KS pvalue" = cvp_gev_ks,
                               "Quantile 5 year" = qsgev5_av, "Quantile 10 year" = qsgev10_av,
                               "Quantile 20 year"= qsgev20_av, "Brier 5 year"= bsgev5_av, 
                               "Brier 10 year" = bsgev10_av, "Brier 20 year" = bsgev20_av,
                               "FGP" = testplotframe$FGP)
      gev_gofvalues <- rbind(newRow_gev, gev_gofvaluestry)
      write.table (gev_gofvalues, file = "gev valuestry4.txt", sep = "\ ")
    }
}


#########################################################################

fgp_plot <- data.frame("EXP brier pot" = exp_values$Quantile.20.year,"EXP brier ams" = exp_values$Quantile.20.year.ams,"EXP FGP" = exp_values$Floods.per.year,
                       "GP brier pot" = gp_values$Quantile.20.year,"GP brier ams" = gp_values$Quantile.20.year.ams,"GP FGP" = gp_values$Floods.per.year,
                       "GEV ams" = gev_values$Quantile.20.year,"GEV FGP" = gev_values$Floods.per.year,
                       "GUM ams" = gum_values$Quantile.20.year,"GUM FGP" = gum_values$Floods.per.year)

KS_plot<- data.frame("EXP KS" = exp_values$CV.AD.score,"EXP FPY" = exp_values$Floods.per.year,
                     "GP KS" = gp_values$CV.AD.score,"GP FPY" = gp_values$Floods.per.year,
                     "GEV KS" = gev_values$CV.AD.score,"GEV FPY" = gev_values$Floods.per.year,
                     "GUM KS" = gum_values$CV.AD.score,"GUM FPY" = gum_values$Floods.per.year, "station" = exp_values$Stations)
KS_plot <- KS_plot[KS_plot]

`Estimation/validation data`= "POT/POT or AMS/AMS"
gppot ="POT/POT"
gev2 ="AMS/AMS "
gum2 = "AMS/AMS"
pot_ams = "POT/AMS"
pot_ams2 = "POT/AMS"
Legend = "EXP"
gp = "GP"
gev = "GEV"
gum <- "GUM"
expp <- "EXP AMS"
gpp <- "GP AMS"
cols <- c("EXP"="red", "GP"="black", "GEV" ="blue", "GUM" = "cyan", "EXP AMS" ="red", "GP AMS"="black")
linet <- c("POT/POT or AMS/AMS"= 7, "POT/AMS" = 3)

ggplot(data=KS_plot )+
  geom_smooth(aes(x = EXP.FPY, y = EXP.KS, color = Legend, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GP.FPY, y = GP.KS, colour = gp, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GEV.FPY, y = GEV.KS, color = gev, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GUM.FPY, y = GUM.KS, color = gum, lty = `Estimation/validation data`), se=F, method = 'loess')+
  xlab("Floods per year")+
  ylab("KS score")+
  ggtitle("Kolmogorov-Smirnov goodness-of-fit")+
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill = "white"), legend.position = "bottom", legend.box = "vertical")+
  scale_color_manual(values=c(cols))+
  scale_linetype(guide=FALSE)

ggplot(data=fgp_plot)+
  geom_smooth(aes(x = EXP.FGP, y = EXP.brier.pot, color = Legend, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GP.FGP, y = GP.brier.pot, colour = gp, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GEV.FGP, y = GEV.ams, color = gev, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GUM.FGP, y = GUM.ams, color = gum, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = EXP.FGP, y = EXP.brier.ams, color = exp, lty = pot_ams), se=F, method = 'loess')+
  geom_smooth(aes(x = GP.FGP, y = GP.brier.ams, color = gp, lty = pot_ams), se=F, method = 'loess')+
  xlab("Floods per year")+
  ylab("Brier score")+
  ggtitle("Brier score")+
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill = "white"), legend.position = "bottom", legend.box = "vertical")+
  scale_color_manual(values=c(cols))+
  scale_linetype_manual("",values =c(linet))
#######################################################################


hist_plot <- as.data.frame(cbind("GEV"=gev_values$CV.KS.score, "GUM"=gum_values$CV.KS.score, "EXP"=exp_values$CV.KS.score, "GP"=gp_values$CV.KS.score))
hist_KS <- data.frame(matrix(ncol = 1, nrow = length(hist_plot$GEV)))
hist_KS <- colnames(hist_plot)[apply(hist_plot,1,which.min)]
hist_KS <- data.frame(hist_KS)
hist_KS <- hist_KS %>% add_count(hist_KS)
hist_KS <- dplyr::distinct(hist_KS) 
barplot(hist_KS$n, names.arg=hist_KS$hist_KS)
