library(dplyr)
library(rlang)
library(base)
library(lubridate)
library(tibble)
require(data.table)

#AcuteToxSet <- subset(tox_SortMRNLateralityDate, acute==1)
na.omit(AcuteToxSet)
#class(AcuteToxSet)



TxItem <- c("Fatigue", "Cosmesis", "Radation_Dermatitis", "Hyperpigmentation", "Hypopigmentation", 
            "Breast_Pain", "Lymphedema", "Induration_Fibrosis", "BreastAtrophy_VolumeReduction",
            "FatNecrosis", "Telangiectasia", "BreastInfection", "Seroma")
NoToxItem <- length(TxItem)
NoToxItem



############Find distinguish patients/laterality entries from tox data
UniPatient <- data.frame(MRN = AcuteToxSet$MRN, Laterality = AcuteToxSet$Laterality)
#NEntry <- nrow(unique(UniPatient))


############remove the duplicated records by group of MRN/Laterality/DATE
AcuteToxSetNew <- AcuteToxSet %>% distinct(MRN, Laterality, DATE, .keep_all = TRUE)



#Arrange the data by the following orders: asending MRN, ascending laterality
#                                           descending Tox, and ascending DATE
##############   Fatigue ###################################################################
AcuteToxSetFatigue <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Fatigue), DATE)

AcuteToxFatigue <- AcuteToxSetFatigue %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, Fatigue) %>%
  top_n(-1, DATE)
  
AcuteToxFatigue<- subset(AcuteToxFatigue, select = c("MRN", "Laterality", "Fatigue", "DATE"))
AcuteToxFatigue <- rename(AcuteToxFatigue, MRN=MRN, Laterality=Laterality, MaxAcuteFatigue = Fatigue, MaxAcuteFatigueDt = DATE)

##############   Cosmesis ###################################################################
AcuteToxSetCosmesis <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Cosmesis), DATE)

AcuteToxCosmesis <- AcuteToxSetCosmesis %>% group_by(MRN, Laterality, add=TRUE) %>%
                    top_n(1, Cosmesis) %>%
                    top_n(-1, DATE)
  
AcuteToxCosmesis<- subset(AcuteToxCosmesis, select = c("MRN", "Laterality", "Cosmesis", "DATE"))
AcuteToxCosmesis<- rename(AcuteToxCosmesis, MRN=MRN, Laterality=Laterality, MaxAcuteCosmesis = Cosmesis, MaxAcuteCosmesisDt = DATE)

##############   Radiation Dermatitis ###################################################################
AcuteToxSetRadiationDermatits <- arrange(AcuteToxSetNew, MRN, Laterality, desc(`Radiation Dermatits`), DATE)

AcuteToxSetRadiationDermatits <- AcuteToxSetRadiationDermatits %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Radiation Dermatits") %>%
  top_n(-1, DATE)

AcuteToxRadiationDermatits<- subset(AcuteToxSetRadiationDermatits, select = c("MRN", "Laterality", "Radiation Dermatits", "DATE"))
AcuteToxRadiationDermatits<- rename(AcuteToxRadiationDermatits, MRN=MRN, Laterality=Laterality, MaxAcuteRadiationDermatits = "Radiation Dermatits", MaxAcuteRadiationDermatitsDt = DATE)


##############   Hyperpigmentation   ###################################################################
AcuteToxSetHyperpigmentation   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Hyperpigmentation), DATE)

AcuteToxSetHyperpigmentation <- AcuteToxSetHyperpigmentation %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Hyperpigmentation") %>%
  top_n(-1, DATE)

AcuteToxHyperpigmentation<- subset(AcuteToxSetHyperpigmentation, select = c("MRN", "Laterality", "Hyperpigmentation", "DATE"))
AcuteToxHyperpigmentation<- rename(AcuteToxHyperpigmentation, MRN=MRN, Laterality=Laterality, MaxAcuteHyperpigmentation = Hyperpigmentation, MaxAcuteHyperpigmentationDt = DATE)



##############   Hypopigmentation   ###################################################################
AcuteToxSetHypopigmentation   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Hypopigmentation), DATE)

AcuteToxSetHypopigmentation <- AcuteToxSetHypopigmentation %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Hypopigmentation") %>%
  top_n(-1, DATE)

AcuteToxHypopigmentation<- subset(AcuteToxSetHypopigmentation, select = c("MRN", "Laterality", "Hypopigmentation", "DATE"))
AcuteToxHypopigmentation<- rename(AcuteToxHypopigmentation, MRN=MRN, Laterality=Laterality, MaxAcuteHypopigmentation = Hypopigmentation, MaxAcuteHypopigmentationDt = DATE)


##############   Breast pain   ###################################################################
AcuteToxSetBreastPain   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(`Breast pain`), DATE)

AcuteToxSetBreastPain <- AcuteToxSetBreastPain %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Breast pain") %>%
  top_n(-1, DATE)

AcuteToxBreastPain<- subset(AcuteToxSetBreastPain, select = c("MRN", "Laterality", "Breast pain", "DATE"))
AcuteToxBreastPain<- rename(AcuteToxBreastPain, MRN=MRN, Laterality=Laterality, MaxAcuteBreastPain = "Breast pain", MaxAcuteBreastPainDt = DATE)


##############   Lymphedema   ###################################################################
AcuteToxSetLymphedema   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Lymphedema), DATE)

AcuteToxSetLymphedema <- AcuteToxSetLymphedema %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Lymphedema") %>%
  top_n(-1, DATE)

AcuteToxLymphedema<- subset(AcuteToxSetLymphedema, select = c("MRN", "Laterality", "Lymphedema", "DATE"))
AcuteToxLymphedema<- rename(AcuteToxLymphedema, MRN=MRN, Laterality=Laterality, MaxAcuteLymphedema = "Lymphedema", MaxAcuteLymphedemaDt = DATE)



##############   Induration_Fibrosis   ###################################################################
AcuteToxSetInduration_Fibrosis   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Induration_Fibrosis), DATE)

AcuteToxSetInduration_Fibrosis <- AcuteToxSetInduration_Fibrosis %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Induration_Fibrosis") %>%
  top_n(-1, DATE)

AcuteToxInduration_Fibrosis<- subset(AcuteToxSetInduration_Fibrosis, select = c("MRN", "Laterality", "Induration_Fibrosis", "DATE"))
AcuteToxInduration_Fibrosis<- rename(AcuteToxInduration_Fibrosis, MRN=MRN, Laterality=Laterality, MaxAcuteInduration_Fibrosis = "Induration_Fibrosis", MaxAcuteInduration_FibrosisDt = DATE)


##############   Volume Reduction   ###################################################################
AcuteToxSetVolumeReduction   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(`Volume Reduction`), DATE)

AcuteToxSetVolumeReduction <- AcuteToxSetVolumeReduction %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Volume Reduction") %>%
  top_n(-1, DATE)

AcuteToxVolumeReduction<- subset(AcuteToxSetVolumeReduction, select = c("MRN", "Laterality", "Volume Reduction", "DATE"))
AcuteToxVolumeReduction<- rename(AcuteToxVolumeReduction, MRN=MRN, Laterality=Laterality, MaxAcuteVolumeReduction = "Volume Reduction", MaxAcuteVolumeReductionDt = DATE)


##############   Fat Necrosis   ###################################################################
AcuteToxSetFatNecrosis   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(`Fat Necrosis`), DATE)

AcuteToxSetFatNecrosis <- AcuteToxSetFatNecrosis %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Fat Necrosis") %>%
  top_n(-1, DATE)

AcuteToxFatNecrosis<- subset(AcuteToxSetFatNecrosis, select = c("MRN", "Laterality", "Fat Necrosis", "DATE"))
AcuteToxFatNecrosis<- rename(AcuteToxFatNecrosis, MRN=MRN, Laterality=Laterality, MaxAcuteFatNecrosis = "Fat Necrosis", MaxAcuteFatNecrosisDt = DATE)



##############   Telangiectasia   ###################################################################
AcuteToxSetTelangiectasia   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Telangiectasia), DATE)

AcuteToxSetTelangiectasia <- AcuteToxSetTelangiectasia %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Telangiectasia") %>%
  top_n(-1, DATE)

AcuteToxTelangiectasia<- subset(AcuteToxSetTelangiectasia, select = c("MRN", "Laterality", "Telangiectasia", "DATE"))
AcuteToxTelangiectasia<- rename(AcuteToxTelangiectasia, MRN=MRN, Laterality=Laterality, MaxAcuteTelangiectasia = "Telangiectasia", MaxAcuteTelangiectasiaDt = DATE)


##############   Seroma   ###################################################################
AcuteToxSetSeroma   <- arrange(AcuteToxSetNew, MRN, Laterality, desc(Seroma), DATE)

AcuteToxSetSeroma <- AcuteToxSetSeroma %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Seroma") %>%
  top_n(-1, DATE)

AcuteToxSeroma<- subset(AcuteToxSetSeroma, select = c("MRN", "Laterality", "Seroma", "DATE"))
AcuteToxSeroma<- rename(AcuteToxSeroma, MRN=MRN, Laterality=Laterality, MaxAcuteSeroma = "Seroma", MaxAcuteSeromaDt = DATE)


##############   Breast Infection   ###################################################################
AcuteToxSetBreastInfection  <- arrange(AcuteToxSetNew, MRN, Laterality, desc(`Breast Infection`), DATE)

AcuteToxSetBreastInfection <- AcuteToxSetBreastInfection %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Breast Infection") %>%
  top_n(-1, DATE)

AcuteToxBreastInfection<- subset(AcuteToxSetBreastInfection, select = c("MRN", "Laterality", "Breast Infection", "DATE"))
AcuteToxBreastInfection<- rename(AcuteToxBreastInfection, MRN=MRN, Laterality=Laterality, MaxAcuteBreastInfection = "Breast Infection", MaxAcuteBreastInfectionDt = DATE)





rm(AcuteToxSetFatigue, AcuteToxSetCosmesis, AcuteToxSetRadiationDermatits, AcuteToxSetHyperpigmentation,
   AcuteToxSetHypopigmentation, AcuteToxSetLymphedema, AcuteToxSetInduration_Fibrosis, 
   AcuteToxSetVolumeReduction, AcuteToxSetFatNecrosis, AcuteToxSetTelangiectasia, 
   AcuteToxSetBreastInfection, AcuteToxSetSeroma)


merge(x,y) {
  merge(x, y, by="MRN", all=TRUE)
}

ToxTable <- list(AcuteToxFatigue,AcuteToxCosmesis,AcuteToxRadiationDermatits,
              AcuteToxHyperpigmentation,AcuteToxHypopigmentation,
              AcuteToxLymphedema,AcuteToxInduration_Fibrosis, 
              AcuteToxVolumeReduction,AcuteToxFatNecrosis, AcuteToxTelangiectasia, 
              AcuteToxBreastInfection,AcuteToxSeroma)

length(ToxTable)
FinalAcuteMaxTox<- UniPatient
for (m in 1: length(ToxTable)){
  
  FinalAcuteMaxTox <- merge(FinalAcuteMaxTox, ToxTable[m], by=c("MRN","Laterality"), all=TRUE)
}

FinalAcuteMaxTox <- merge(UniPatient, AcuteToxFatigue, by=c("MRN","Laterality"), all=TRUE)
FinalAcuteMaxTox <- merge(FinalAcuteMaxTox, AcuteToxCosmesis, by="MRN", all=TRUE)
FinalAcuteMaxTox <- merge(UniPatient, ToxTable[1], by="MRN", all=TRUE)
