library(dplyr)
library(rlang)
library(base)
library(lubridate)
library(tibble)
require(data.table)

AcuteToxSet <- subset(tox_SortMRNLateralityDate, acute==1)
na.omit(AcuteToxSet)
class(AcuteToxSet)
AcuteToxSet$Fatigue
which.max(AcuteToxSet$Fatigue)


TxItem <- c("Fatigue", "Cosmesis", "Radation_Dermatitis", "Hyperpigmentation", "Hypopigmentation", 
            "Breast_Pain", "Lymphedema", "Induration_Fibrosis", "BreastAtrophy_VolumeReduction",
            "FatNecrosis", "Telangiectasia", "BreastInfection", "Seroma")
NoToxItem <- length(TxItem)
NoToxItem



#Find distinguish patients/laterality entries from tox data
UniPatient <- data.frame(MRN = AcuteToxSet$MRN, Laterality = AcuteToxSet$Laterality)
NEntry <- nrow(unique(UniPatient))
#AcuteToxSet$Dup <- duplicated(UniPatient)
#UniPatient$MaxFatigue <- AcuteToxSet %>% group_by(MRN, Laterality) %>% AcuteToxSet[which.max(AcuteToxSet$Fatigue),"Fatigue"]

#remove the duplicated records by group of MRN/Laterality/DATE
AcuteToxSetNew <- AcuteToxSet %>% distinct(MRN, Laterality, DATE, .keep_all = TRUE)


#Arrange the data by the following orders: asending MRN, ascending laterality
#                                           descending Tox, and ascending DATE
AcuteToxSetFatigue <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Fatigue), DATE)

#AcuteToxFatigue <- AcuteToxSetFatigue %>% group_by(MRN, Laterality) %>%
#    arrange(MRN)  %>%
#    arrange(Laterality) %>%
#    arrange(desc(Fatigue)) %>%
#    arrange(mdy(DATE), .by_group=TRUE) %>%
#    summarise(maxFatigue=first(Fatigue) )

AcuteToxFatigue <- AcuteToxSetFatigue %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, Fatigue) %>%
  top_n(-1, DATE) %>%
  

AcuteToxFatigue<- subset(AcuteToxFatigue, select = c("MRN", "Laterality", "Fatigue", "DATE"))
rename(AcuteToxFatigue, MRN=MRN, Laterality=Laterality, MaxFatigue = Fatigue, MaxFatigueDt = DATE)





















#group toxicity entries for each patient with latarality
for (i in 1:2) {

  if (AcuteToxSet$MRN[i] != AcuteToxSet$MRN[i+1]) {   
    #print("I am here")
    #print(AcuteToxSet$MRN[i])
    finaltox <- rbind(finaltox, data.frame(MRN = AcuteToxSet$MRN[i], 
                                           Laterality =AcuteToxSet$Laterality[i], 
                                           MaxFatigue= AcuteToxSet$Fatigue[i],
                                           MaxFatigueDt = AcuteToxSet$DATE[i]) )
    m=1
  } 
  else if (AcuteToxSet$MRN[i] == AcuteToxSet$MRN[i+1] & AcuteToxSet$Dup == TRUE) {
    m=m+1
    
  }
}
























