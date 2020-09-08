library(base)
library(dplyr)
library(lubridate)
library(tibble)

tox <- subset(TOXICITY_Temp, RT!="1/0/1900")
tox1 <- as.data.frame(tox)
tox1$DATE <- mdy(tox1$DATE)
tox1$RT <- mdy(tox1$RT)
#tox1 <- tox1[order(tox1[,1], tox1[,2], tox1[,3])]



dates <- data.frame(MRN = tox$MRN, Laterality = tox$Laterality, date=tox$DATE, RT=tox$RT, Fatigue=tox$Fatigue)
#dates

dates$diff <- as.numeric(dates$date, dates$RT, units=c("days"))

tox_SortMRNLateralityDate <- tox1
tox_SortMRNLateralityDate$diff <- dates$diff

tox_SortMRNLateralityDate <- tox1[with(tox1, order(tox1$MRN, tox1$Laterality, tox1$DATE)),]


#Breast Irradiation Toxicity Item in CTCAE V4.0
TxItem <- c("Fatigue", "Cosmesis", "Radation_Dermatitis", "Hyperpigmentation", "Hypopigmentation", 
            "Breast_Pain", "Lymphedema", "Induration_Fibrosis", "BreastAtrophy_VolumeReduction",
            "FatNecrosis", "Telangiectasia", "BreastInfection", "Seroma")

#toxdt <- as.date(as.character(date$DATE), format = "%yyyy/%mm/%dd")
#RT <- as.date(as.character(dates$RT), format = "%yyyy/%mm/%dd")



