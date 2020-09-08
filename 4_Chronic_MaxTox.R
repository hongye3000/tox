library(dplyr)
library(rlang)
library(base)
library(lubridate)
library(tibble)
require(data.table)

ChronicToxSet <- subset(tox1, chronic==1)
na.omit(ChronicToxSet)
#class(ChronicToxSet)



TxItem <- c("Fatigue", "Cosmesis", "Radation_Dermatitis", "Hyperpigmentation", "Hypopigmentation", 
            "Breast_Pain", "Lymphedema", "Induration_Fibrosis", "BreastAtrophy_VolumeReduction",
            "FatNecrosis", "Telangiectasia", "BreastInfection", "Seroma")
NoToxItem <- length(TxItem)
NoToxItem



############Find distinguish patients/laterality entries from tox data
UniPatient <- data.frame(MRN = ChronicToxSet$MRN, Laterality = ChronicToxSet$Laterality)
#NEntry <- nrow(unique(UniPatient))


############remove the duplicated records by group of MRN/Laterality/DATE
ChronicToxSetNew <- ChronicToxSet %>% distinct(MRN, Laterality, DATE, .keep_all = TRUE)



#Arrange the data by the following orders: asending MRN, ascending laterality
#                                           descending Tox, and ascending DATE
##############   Fatigue ###################################################################
ChronicToxSetFatigue <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Fatigue), DATE)

ChronicToxFatigue <- ChronicToxSetFatigue %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, Fatigue) %>%
  top_n(-1, DATE)
  
ChronicToxFatigue<- subset(ChronicToxFatigue, select = c("MRN", "Laterality", "Fatigue", "DATE"))
ChronicToxFatigue <- rename(ChronicToxFatigue, MRN=MRN, Laterality=Laterality, MaxChronicFatigue = Fatigue, MaxChronicFatigueDt = DATE)

##############   Cosmesis ###################################################################
ChronicToxSetCosmesis <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Cosmesis), DATE)

ChronicToxCosmesis <- ChronicToxSetCosmesis %>% group_by(MRN, Laterality, add=TRUE) %>%
                    top_n(1, Cosmesis) %>%
                    top_n(-1, DATE)
  
ChronicToxCosmesis<- subset(ChronicToxCosmesis, select = c("MRN", "Laterality", "Cosmesis", "DATE"))
ChronicToxCosmesis<- rename(ChronicToxCosmesis, MRN=MRN, Laterality=Laterality, MaxChronicCosmesis = Cosmesis, MaxChronicCosmesisDt = DATE)

##############   Radiation Dermatitis ###################################################################
ChronicToxSetRadiationDermatits <- arrange(ChronicToxSetNew, MRN, Laterality, desc(`Radiation Dermatits`), DATE)

ChronicToxSetRadiationDermatits <- ChronicToxSetRadiationDermatits %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Radiation Dermatits") %>%
  top_n(-1, DATE)

ChronicToxRadiationDermatits<- subset(ChronicToxSetRadiationDermatits, select = c("MRN", "Laterality", "Radiation Dermatits", "DATE"))
ChronicToxRadiationDermatits<- rename(ChronicToxRadiationDermatits, MRN=MRN, Laterality=Laterality, MaxChronicRadiationDermatits = "Radiation Dermatits", MaxChronicRadiationDermatitsDt = DATE)


##############   Hyperpigmentation   ###################################################################
ChronicToxSetHyperpigmentation   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Hyperpigmentation), DATE)

ChronicToxSetHyperpigmentation <- ChronicToxSetHyperpigmentation %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Hyperpigmentation") %>%
  top_n(-1, DATE)

ChronicToxHyperpigmentation<- subset(ChronicToxSetHyperpigmentation, select = c("MRN", "Laterality", "Hyperpigmentation", "DATE"))
ChronicToxHyperpigmentation<- rename(ChronicToxHyperpigmentation, MRN=MRN, Laterality=Laterality, MaxChronicHyperpigmentation = Hyperpigmentation, MaxChronicHyperpigmentationDt = DATE)



##############   Hypopigmentation   ###################################################################
ChronicToxSetHypopigmentation   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Hypopigmentation), DATE)

ChronicToxSetHypopigmentation <- ChronicToxSetHypopigmentation %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Hypopigmentation") %>%
  top_n(-1, DATE)

ChronicToxHypopigmentation<- subset(ChronicToxSetHypopigmentation, select = c("MRN", "Laterality", "Hypopigmentation", "DATE"))
ChronicToxHypopigmentation<- rename(ChronicToxHypopigmentation, MRN=MRN, Laterality=Laterality, MaxChronicHypopigmentation = Hypopigmentation, MaxChronicHypopigmentationDt = DATE)


##############   Breast pain   ###################################################################
ChronicToxSetBreastPain   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(`Breast pain`), DATE)

ChronicToxSetBreastPain <- ChronicToxSetBreastPain %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Breast pain") %>%
  top_n(-1, DATE)

ChronicToxBreastPain<- subset(ChronicToxSetBreastPain, select = c("MRN", "Laterality", "Breast pain", "DATE"))
ChronicToxBreastPain<- rename(ChronicToxBreastPain, MRN=MRN, Laterality=Laterality, MaxChronicBreastPain = "Breast pain", MaxChronicBreastPainDt = DATE)


##############   Lymphedema   ###################################################################
ChronicToxSetLymphedema   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Lymphedema), DATE)

ChronicToxSetLymphedema <- ChronicToxSetLymphedema %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Lymphedema") %>%
  top_n(-1, DATE)

ChronicToxLymphedema<- subset(ChronicToxSetLymphedema, select = c("MRN", "Laterality", "Lymphedema", "DATE"))
ChronicToxLymphedema<- rename(ChronicToxLymphedema, MRN=MRN, Laterality=Laterality, MaxChronicLymphedema = "Lymphedema", MaxChronicLymphedemaDt = DATE)



##############   Induration_Fibrosis   ###################################################################
ChronicToxSetInduration_Fibrosis   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Induration_Fibrosis), DATE)

ChronicToxSetInduration_Fibrosis <- ChronicToxSetInduration_Fibrosis %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Induration_Fibrosis") %>%
  top_n(-1, DATE)

ChronicToxInduration_Fibrosis<- subset(ChronicToxSetInduration_Fibrosis, select = c("MRN", "Laterality", "Induration_Fibrosis", "DATE"))
ChronicToxInduration_Fibrosis<- rename(ChronicToxInduration_Fibrosis, MRN=MRN, Laterality=Laterality, MaxChronicInduration_Fibrosis = "Induration_Fibrosis", MaxChronicInduration_FibrosisDt = DATE)


##############   Volume Reduction   ###################################################################
ChronicToxSetVolumeReduction   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(`Volume Reduction`), DATE)

ChronicToxSetVolumeReduction <- ChronicToxSetVolumeReduction %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Volume Reduction") %>%
  top_n(-1, DATE)

ChronicToxVolumeReduction<- subset(ChronicToxSetVolumeReduction, select = c("MRN", "Laterality", "Volume Reduction", "DATE"))
ChronicToxVolumeReduction<- rename(ChronicToxVolumeReduction, MRN=MRN, Laterality=Laterality, MaxChronicVolumeReduction = "Volume Reduction", MaxChronicVolumeReductionDt = DATE)


##############   Fat Necrosis   ###################################################################
ChronicToxSetFatNecrosis   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(`Fat Necrosis`), DATE)

ChronicToxSetFatNecrosis <- ChronicToxSetFatNecrosis %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Fat Necrosis") %>%
  top_n(-1, DATE)

ChronicToxFatNecrosis<- subset(ChronicToxSetFatNecrosis, select = c("MRN", "Laterality", "Fat Necrosis", "DATE"))
ChronicToxFatNecrosis<- rename(ChronicToxFatNecrosis, MRN=MRN, Laterality=Laterality, MaxChronicFatNecrosis = "Fat Necrosis", MaxChronicFatNecrosisDt = DATE)



##############   Telangiectasia   ###################################################################
ChronicToxSetTelangiectasia   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Telangiectasia), DATE)

ChronicToxSetTelangiectasia <- ChronicToxSetTelangiectasia %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Telangiectasia") %>%
  top_n(-1, DATE)

ChronicToxTelangiectasia<- subset(ChronicToxSetTelangiectasia, select = c("MRN", "Laterality", "Telangiectasia", "DATE"))
ChronicToxTelangiectasia<- rename(ChronicToxTelangiectasia, MRN=MRN, Laterality=Laterality, MaxChronicTelangiectasia = "Telangiectasia", MaxChronicTelangiectasiaDt = DATE)


##############   Seroma   ###################################################################
ChronicToxSetSeroma   <- arrange(ChronicToxSetNew, MRN, Laterality, desc(Seroma), DATE)

ChronicToxSetSeroma <- ChronicToxSetSeroma %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Seroma") %>%
  top_n(-1, DATE)

ChronicToxSeroma<- subset(ChronicToxSetSeroma, select = c("MRN", "Laterality", "Seroma", "DATE"))
ChronicToxSeroma<- rename(ChronicToxSeroma, MRN=MRN, Laterality=Laterality, MaxChronicSeroma = "Seroma", MaxChronicSeromaDt = DATE)


##############   Breast Infection   ###################################################################
ChronicToxSetBreastInfection  <- arrange(ChronicToxSetNew, MRN, Laterality, desc(`Breast Infection`), DATE)

ChronicToxSetBreastInfection <- ChronicToxSetBreastInfection %>% group_by(MRN, Laterality, add=TRUE) %>%
  top_n(1, "Breast Infection") %>%
  top_n(-1, DATE)

ChronicToxBreastInfection<- subset(ChronicToxSetBreastInfection, select = c("MRN", "Laterality", "Breast Infection", "DATE"))
ChronicToxBreastInfection<- rename(ChronicToxBreastInfection, MRN=MRN, Laterality=Laterality, MaxChronicBreastInfection = "Breast Infection", MaxChronicBreastInfectionDt = DATE)





rm(ChronicToxSetFatigue, ChronicToxSetCosmesis, ChronicToxSetRadiationDermatits, ChronicToxSetHyperpigmentation,
   ChronicToxSetHypopigmentation, ChronicToxSetLymphedema, ChronicToxSetInduration_Fibrosis, 
   ChronicToxSetVolumeReduction, ChronicToxSetFatNecrosis, ChronicToxSetTelangiectasia, 
   ChronicToxSetBreastInfection, ChronicToxSetSeroma)


merge(x,y) {
  merge(x, y, by="MRN", all=TRUE)
}

ToxTable <- list(ChronicToxFatigue,ChronicToxCosmesis,ChronicToxRadiationDermatits,
              ChronicToxHyperpigmentation,ChronicToxHypopigmentation,
              ChronicToxLymphedema,ChronicToxInduration_Fibrosis, 
              ChronicToxVolumeReduction,ChronicToxFatNecrosis, ChronicToxTelangiectasia, 
              ChronicToxBreastInfection,ChronicToxSeroma)

length(ToxTable)
FinalChronicMaxTox<- UniPatient
for (m in 1: length(ToxTable)){
  
  FinalChronicMaxTox <- merge(FinalChronicMaxTox, ToxTable[m], by=c("MRN","Laterality"), all=TRUE)
}

FinalChronicMaxTox <- merge(UniPatient, ChronicToxFatigue, by=c("MRN","Laterality"), all=TRUE)
FinalChronicMaxTox <- merge(FinalChronicMaxTox, ChronicToxCosmesis, by="MRN", all=TRUE)
FinalChronicMaxTox <- merge(UniPatient, ToxTable[1], by="MRN", all=TRUE)
