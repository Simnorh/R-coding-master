setwd("D:/Master/Thesis/Data")

gev_gofvalues <- data.frame(matrix(ncol = 7, nrow = 0))
cnames_gev <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year")
colnames(gev_gofvalues) <- cnames_gev

gp_gofvalues <- data.frame(matrix(ncol = 7, nrow = 0))
cnames_gp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year")
colnames(gp_gofvalues) <- cnames_gp

exp_gofvalues <- data.frame(matrix(ncol = 7, nrow = 0))
cnames_exp <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year")
colnames(exp_gofvalues) <- cnames_exp

gumbel_gofvalues <- data.frame(matrix(ncol = 7, nrow = 0))
cnames_gum <- c("Stations", "Years", "ADscore_ams","KSscore_ams","ADscore_pot","KSscore_pot", "Flood per year")
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

ams_river <- ams_data[ams_data$station == "8.2", ]
pot_river <- pot_data[pot_data$station == "8.2", ]

testplotframe <- as.data.frame(ams_river$station)
colnames(testplotframe)[1] <- "Stations"
testplotframe <- testplotframe %>% add_count(Stations)
colnames(testplotframe)[2] <- "years"
testplotframe <- dplyr::distinct(testplotframe)
Floods_year <- data.frame(nrow(pot_river)/testplotframe$years)
testplotframe <- cbind(testplotframe, Floods_year)
colnames(testplotframe)[3] <- "Floods per year"

#resampled_data <- ams_data[sample(nrow(ams_data)),]
#group_by (resampled_data, station)
#ams_river <- ams_data[ams_data$station == "2.611", ]

#testplotframe <- as.data.frame(ams_river$station)
#colnames(testplotframe)[1] <- "Stations"
#testplotframe <- testplotframe %>% add_count(Stations)
#colnames(testplotframe)[2] <- "years"
#testplotframe <- dplyr::distinct(testplotframe)

#set.seed(2661)
#resampled_data <- testriver[sample(nrow(testriver)),]
#parest <- gp_Lmom(resampled_data$flood.1)
#parest2 <- gev_Lmom(testriver$daily_ams.1)
#parest <- as.data.frame(parest)

gevpar_pot <- gev_Lmom(pot_river$flood.1)
gevpar_ams <- gev_Lmom(ams_river$daily_ams.1)

gppar_pot <- gp_Lmom(pot_river$flood.1, threshold = NA)
gppar_ams <- gp_Lmom(ams_river$daily_ams.1, threshold = NA)

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

AD_gumams <- gof_ad(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
AD_gumpot <- gof_ad(pot_river$flood.1, gevpar_pot$estimate, distr = "gumbel", test.stat=TRUE, p.value=FALSE)
KS_gumams <- gof_ks(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "gumbel", test.stat = TRUE, p.value = FALSE)
KS_gumpot <- gof_ks(pot_river$flood.1, gevpar_pot$estimate, distr = "gumbel", test.stat = TRUE, p.value = FALSE)

AD_expams <- gof_ad(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
AD_exppot <- gof_ad(pot_river$flood.1, gevpar_pot$estimate, distr = "exp", test.stat=TRUE, p.value=FALSE)
KS_expams <- gof_ks(ams_river$daily_ams.1, gevpar_ams$estimate, distr = "exp", test.stat = TRUE, p.value = FALSE)
KS_exppot <- gof_ks(pot_river$flood.1, gevpar_pot$estimate, distr = "exp", test.stat = TRUE, p.value = FALSE)

#gofcstest <- gof_cs(resampled_data$flood.1,
#                   parest$estimate,
#                  distr = "gp")

AD_values_gev_pot <- data.frame(CS = NA, KS = KS_gevpot, AD = AD_gevpot)
AD_values_gev_ams <- data.frame(CS = NA, KS = KS_gevams, AD = AD_gevams)

AD_values_gp_pot <- data.frame(CS = NA, KS = KS_gppot, AD = AD_gppot)
AD_values_gp_ams <- data.frame(CS = NA, KS = KS_gpams, AD = AD_gpams)

AD_values_gum_pot <- data.frame(CS = NA, KS = KS_gumpot, AD = AD_gumpot)
AD_values_gum_ams <- data.frame(CS = NA, KS = KS_gumams, AD = AD_gumams)

AD_values_exp_pot <- data.frame(CS = NA, KS = KS_exppot, AD = AD_exppot)
AD_values_exp_ams <- data.frame(CS = NA, KS = KS_expams, AD = AD_expams)
#####Unødvendig?

plot_all (pot_river$flood.1,
         GOF.list = AD_values_gp_pot,
         param = gppar_pot,
         distr = "gp",
         method = "ad")
#plot_rlevel(testriver$flood.1, parest2$estimate, distr.index = 5)#useless, already in pot all function

newRow_gev <- data.frame("Station" = testplotframe$Stations,"Years" = testplotframe$years, "ADscore_pot" = AD_gevpot, "KSscore_pot" = KS_gevpot, "ADscore_ams" = AD_gevams, "KSscore_ams" = KS_gevams, "Floods per year" = testplotframe$`Floods per year`)
gev_gofvalues <- rbind(newRow_gev, gev_gofvalues)
write.table (gev_gofvalues, file = "gev gofvalues.txt", sep = "\ ")

newRow_gp <- data.frame("Station" = testplotframe$Stations,"Years" = testplotframe$years, "ADscore_pot" = AD_gppot, "KSscore_pot" = KS_gppot, "ADscore_ams" = AD_gpams, "KSscore_ams" = KS_gpams, "Floods per year" = testplotframe$`Floods per year`)
gp_gofvalues <- rbind(newRow_gp, gp_gofvalues)
write.table (gp_gofvalues, file = "gp gofvalues.txt", sep = "\ ")

newRow_gum <- data.frame("Station" = testplotframe$Stations, "Years" = testplotframe$years, "ADscore_pot" = AD_gumpot, "KSscore_pot" = KS_gumpot, "ADscore_ams" = AD_gumams, "KSscore_ams" = KS_gumams, "Floods per year" = testplotframe$`Floods per year`)
gumbel_gofvalues <- rbind(newRow_gum, gumbel_gofvalues)
write.table (gumbel_gofvalues, file = "gumbel gofvalues.txt", sep = "\ ")

newRow_exp <- data.frame("Station" = testplotframe$Stations, "Years" = testplotframe$years, "ADscore_pot" = AD_exppot, "KSscore_pot" = KS_exppot, "ADscore_ams" = AD_expams, "KSscore_ams" = KS_expams, "Floods per year" = testplotframe$`Floods per year`)
exp_gofvalues <- rbind(newRow_exp, exp_gofvalues)
write.table (exp_gofvalues, file = "exp gofvalues.txt", sep = "\ ")
#plot(ad_values_gev$Years, ad_values_gev$ADscore)