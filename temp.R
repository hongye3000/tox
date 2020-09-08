library(base)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
require(data.table)
library(Hmisc)

ID    <- c(1,1,1,1,1,1,1,1,1)
Side <- c(1,1,1,2,2,1,2,2,1)
Fatigue <- c(1,1,2,3,2,1,3,1,2)
Date <- mdy(c("1/1/2018", "1/1/2018", "6/1/2020","6/1/2019", "5/1/2020", "5/16/2017", "12/8/2018","9/1/2020", "3/1/2018"))
#Date <- c("1/1/2018", "6/1/2020","6/1/2019", "5/1/2020", "5/16/2017", "12/8/2018","9/1/2020", "3/1/2018")


group <- data.frame(Subject=ID, Side=Side, Fatigue=Fatigue, Date=Date)
group
group <- group %>% distinct(Subject, Laterality, Date, .keep_all = TRUE)
group
group <- arrange(group, Subject, Side, desc(Fatigue), Date)


UniPatient <- data.frame(MRN = group$Subject, Laterality = group$Side)
NEntry <- nrow(unique(UniPatient))
NEntry

#group1[group1[, .I[maxFatigue == max(Fatigue)], by=Subject,Side]$V1]



group %>% group_by(Subject, Side) %>%
  arrange(Subject)  %>%
  arrange(Side) %>%
  arrange(desc(Fatigue)) %>%
  arrange(mdy(Date), .by_group=TRUE) %>%
  summarise(maxFatigue=max(Fatigue)) %>%

group


ungroup(group)
group %>% group_by(Subject, Side, add=TRUE) %>%
  top_n(1, Fatigue) %>%
  top_n(-1, Date)





TxItem <- c("Fatigue", "Cosmesis", "Radation_Dermatitis", "Hyperpigmentation", "Hypopigmentation", 
            "Breast_Pain", "Lymphedema", "Induration_Fibrosis", "BreastAtrophy_VolumeReduction",
            "FatNecrosis", "Telangiectasia", "BreastInfection", "Seroma")
for (i in 1: length(TxItem)) {
  
  AcuteToxNew <- rbind(AcuteTox, ToxItem[i])
}

 

AllMRN <- data.frame(MRN = c("111111", "222222", "333333", "444444"))
Fatigue <- data.frame(MRN = c("111111", "222222", "333333"), MaxFatigue = c(3, 2, 1))
Cosmesis <- data.frame(MRN = c("111111", "333333"), MaxCosmesis = c(3, 1))
finaltox <- merge(AllMRN, Fatigue, by = "MRN", all.x=TRUE)
finaltox <- merge(finaltox, Cosmesis, by = "MRN", all.x=TRUE)









 
