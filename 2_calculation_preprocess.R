library(base)
library(dplyr)
library(lubridate)
library(tibble)

tox <- subset(TOXICITY_Temp, RT!="1/0/1900")
tox <- subset(tox, select = c("MRN", "Laterality","DATE", "RT", "Fatigue", "Breast pain", "Cosmesis",
                              "Radiation Dermatits", "Hyperpigmentation", "Hypopigmentation",
                              "Breast Edema", "Induration_Fibrosis", "Volume Reduction", 
                              "Fat Necrosis", "Telangiectasia", "Breast Infection", 
                              "Seroma", "Lymphedema", "Lymphedema", "RibFracture", "Nipple Deformity"))
tox1 <- as.data.frame(tox)
tox1$DATE <- mdy(tox1$DATE)
tox1$RT <- mdy(tox1$RT)
#tox1 <- tox1[order(tox1[,1], tox1[,2], tox1[,3])]

tox1$diff <- tox1$DATE - tox1$RT

#dates <- data.frame(MRN = tox$MRN, Laterality = tox$Laterality, date=tox$DATE, RT=tox$RT, Fatigue=tox$Fatigue)
#dates

#tox1$diff <- as.numeric(tox1$DATE, tox1$RT, units=c("days"))

#tox_SortMRNLateralityDate <- tox1
#tox_SortMRNLateralityDate$diff <- dates$diff

#tox_SortMRNLateralityDate <- tox1[with(tox1, order(tox1$MRN, tox1$Laterality, tox1$DATE)),]


#category if the individual toxicity entry acute (<=60 days) or chronic (>60 days) from RT start
n <- nrow(tox1)
for (i in 1:n) {
  if (tox1$diff[i]<=180 & tox1$diff[i]>0) {
    #print(dates$diff[i])
    tox1$acute[i] <- as.numeric(1)
    tox1$chronic[i] <- as.numeric(0)
  } 
  else if(tox1$diff[i]>180) {
    #print("I am here")
    tox1$acute[i] <- as.numeric(0)
    tox1$chronic[i] <- as.numeric(1)
    
  }
}

AcuteToxSet <- subset(tox1, acute==1)
na.omit(AcuteToxSet)
nrow(AcuteToxSet)

ChronicToxSet <- subset(tox1, chronic==1)
na.omit(ChronicToxSet)

rm(TOXICITY_Temp, tox)   
