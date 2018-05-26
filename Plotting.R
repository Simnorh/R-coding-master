setwd("D:/Master/Thesis/Data")
gev_values <-read.table("200gev values50 final parex.txt",header=T,sep="\ ")
exp_values <-read.table("200exp values50 final parex.txt",header=T,sep="\ ")
gp_values <-read.table("200gp values50 final parex.txt",header=T,sep="\ ")
gum_values <-read.table("200gumbel values50 final parex.txt",header=T,sep="\ ")
CoV_values <-read.table("200Coefficient of variation stability test50 final parex.txt",header=T,sep="\ ")
stationx <- read.table("200station values50 final parex.txt", header=T, sep="\ ")
returnlevs <- read.table("200Return level averages50 final parex.txt", header=T, sep="\ ")

stationxtrue <- stationx
stationxtruebs <- gsub(".AMS", "", stationxtrue$BS)
stationxtrueqs <- gsub(".AMS", "", stationxtrue$QS)
stationxtrue <- cbind(stationxtrue, stationxtruebs, stationxtrueqs)
colnames(stationxtrue)[13] <- "BSTrue"
colnames(stationxtrue)[14] <- "QSTrue"
#stationxtrue$FGP <- gp_values$FGP

fgp_plot <- data.frame("EXP brier pot" = exp_values$Quantile.20.year,"EXP brier ams" = exp_values$Quantile.20.year.ams,"EXP FPY" = exp_values$Floods.per.year,
                       "GP brier pot" = gp_values$Quantile.20.year,"GP brier ams" = gp_values$Quantile.20.year.ams,"GUM FPY" = gum_values$Floods.per.year,
                       "GP FPY" = gp_values$Floods.per.year, "GEV ams" = gev_values$Quantile.20.year, "GEV FPY" = gev_values$Floods.per.year,
                       "GUM ams" = gum_values$Quantile.20.year)

#fgp_plot_out <- fgp_plot[!fgp_plot$station == 150.100,]
#fgp_plot_out <- fgp_plot_out[!fgp_plot_out$station ==80.400,]
#fgp_plot_out <- fgp_plot_out[!fgp_plot_out$station ==156.150,]

KS_plot<- data.frame("EXP KS" = exp_values$CV.AD.score,"EXP FPY" = exp_values$Floods.per.year,
                     "GP KS" = gp_values$CV.AD.score,"GP FPY" = gp_values$Floods.per.year,
                     "GEV KS" = gev_values$CV.AD.score,"GEV FPY" = gev_values$Floods.per.year,
                     "GUM KS" = gum_values$CV.AD.score,"GUM FPY" = gum_values$Floods.per.year, "station" = exp_values$Stations)

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
#linet <- c("POT/POT or AMS/AMS"= "solid", "POT/AMS" = "dashed")
linet <- c("EXP"="solid", "GP"="solid", "GEV" ="solid", "GUM" = "solid", "EXP AMS" ="dotted", "GP AMS"="dotted")

ggplot(data=KS_plot)+
  geom_smooth(aes(x = EXP.FPY, y = EXP.KS, color = Legend, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GP.FPY, y = GP.KS, colour = gp, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GEV.FPY, y = GEV.KS, color = gev, lty = `Estimation/validation data`), se=F, method = 'loess')+
  geom_smooth(aes(x = GUM.FPY, y = GUM.KS, color = gum, lty = `Estimation/validation data`), se=F, method = 'loess')+
  xlab(expression(paste("Events per year", " ", "(",lambda, ")")))+
  ylab("AD score")+
  ggtitle("Anderson-Darling goodness-of-fit")+
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill = "white"), legend.position = "bottom", legend.box = "vertical")+
  scale_color_manual(values=c(cols))+
  scale_linetype(guide=FALSE)

ggplot(data=fgp_plot)+
  geom_smooth(aes(x = EXP.FPY, y = EXP.brier.pot, color = Legend, lty = Legend), se=F, method = 'loess')+
  geom_smooth(aes(x = GP.FPY, y = GP.brier.pot, colour = gp, lty = gp), se=F, method = 'loess')+
  geom_smooth(aes(x = GEV.FPY, y = GEV.ams, color = gev, lty = gev), se=F, method = 'loess')+
  geom_smooth(aes(x = GUM.FPY, y = GUM.ams, color = gum, lty = gum), se=F, method = 'loess')+
  geom_smooth(aes(x = EXP.FPY, y = EXP.brier.ams, color = expp, lty = expp), se=F, method = 'loess')+
  geom_smooth(aes(x = GP.FPY, y = GP.brier.ams, color = gpp, lty = gpp), se=F, method = 'loess')+
  xlab(expression(paste("Events per year", " ", "(",lambda, ")")))+
  ylab("Quantile score")+
  ggtitle("Quantile score 20 year return period")+
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill = "white"), legend.position = "bottom", legend.box = "vertical")+
  scale_color_manual(values=cols)+
  scale_linetype_manual(values =linet)
  guides(color=guide_legend(override.aes = list(linetype = linet )))



  mean(gum_values$CV.KS.score, na.rm=T)
 mean(gev_values$CV.KS.score, na.rm=T)
 mean(exp_values$CV.KS.score, na.rm=T)
 mean(gp_values$CV.KS.score, na.rm=T)
 mean(gum_values$Brier.100.year, na.rm=T)
 mean(gum_values$Brier.200.year, na.rm=T)

vioplot(CoV_values$X20.GUM, CoV_values$X20.GEV, CoV_values$X20.EXP, CoV_values$X20.GP,
        col="paleturquoise3", names=c("GUM", "GEV", "EXP", "GP"), lty=0, lwd =1, horizontal = F)
title(main ="CV for 20 year design flood estimates", ylab="CV", xlab = "Distributions")

plot1
  plot1 + scale_color_manual("Legend", values=c("black","blue","darkgreen","purple"))

labels <- c("EXP", "GP", "GEV", "GUM")
legend("bottomright",title = "Legend", labels, col=c("black", "blue", "darkgreen", "purple") )



roundtry <- function(x, base){
  base*round(x/base)
}#works good rounds 0.75-1.25 to 1 and so on
gev_values[,"chunk"] <- NA
for(i in 1:length(gev_values$Floods.per.year)){
  gev_values$chunk[i] <- roundtry(gev_values$Floods.per.year[i], base = 0.5)
}

par(mfrow=c(1,2))
hist_plot <- as.data.frame(cbind("GEV"=gev_values$Brier.20.year, "GUM"=gum_values$Brier.20.year,
                                 "AMS EXP"=exp_values$Brier.20.year.ams, "AMS GP"=gp_values$Brier.20.year.ams))
hist_KS <- data.frame(matrix(ncol = 1, nrow = length(hist_plot$GEV)))
hist_KS <- colnames(hist_plot)[apply(hist_plot,1,which.min)]
hist_KS <- data.frame(hist_KS)
hist_KS <- hist_KS %>% add_count(hist_KS)
hist_KS <- dplyr::distinct(hist_KS) 
hist_KS <- hist_KS[order(hist_KS$hist_KS),,drop=F]
labs <- c("EXP AMS", "GP AMS", "GEV", "GUM")
hist_KS <- cbind(hist_KS, labs)
barplot(hist_KS$n, names.arg=hist_KS$labs, main = "Brier score 20 year return period", sub="B", col="grey", cex.names = 0.8, ylab= "Number of stations", 
        ylim = c(0, 140))

BS2 <- as.data.frame(stationxtrue$QS2)
BS2 <- BS2 %>% add_count(BS2$`stationxtrue$QS2`) 
BS2 <- dplyr::distinct(BS2) 
barplot(BS2$n, names.arg = BS2$`stationxtrue$QS2`)


stationlengths <- as.data.frame(gev_values$chunk)
stationlengths <- stationlengths %>% add_count(stationlengths$`gev_values$chunk`)
stationlengths <- dplyr::distinct(stationlengths)
stationlengths <- stationlengths[order(stationlengths$`gev_values$chunk`),, drop=F]
barplot(stationlengths$n, names.arg=stationlengths$`gev_values$chunk`, main = "Length of POT series relative to AMS series", 
        xlab=expression(paste("Events per year ", "(",lambda, ")")), ylab="Number of stations", ylim = c(0, 60))

