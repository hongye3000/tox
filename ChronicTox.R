
ChronicToxSet <- subset(tox_SortMRNLateralityDate, chronic==1)


UniPatient <- data.frame(MRN = ChronicToxSet$MRN, Laterality = ChronicToxSet$Laterality)
NEntry <- nrow(unique(UniPatient))
ChronicToxSet$Dup <- duplicated(UniPatient)



#Create a new data frame to keep all the maximum toxicity

finaltox <- data.frame(MRN = numeric(), Laterality=numeric(), MaxFatigue =numeric(), MaxFatigueDt = character())

#
#columnnames <- c("MRN", "Laterality", "MaxFatigue", "MaxFatigueDt")
#finaltox <- data.frame(matrix(ncol=length(columnnames), nrow=0))
#colnames(finaltox) <- columnnames


#group toxicity entries for each patient with latarality
for (i in 1:2) {
  #MRN_temp <- ChronicToxSet$MRN[i]
  #Laterality_temp <-ChronicToxSet$Laterality[i]
  # Fatigue_temp <- ChronicToxSet$Fatigue[i]
  # FatigueDt_temp <- ChronicToxSet$DATE[i]
  
  if (ChronicToxSet$MRN[i] != ChronicToxSet$MRN[i+1]) {   
    #print("I am here")
    #print(ChronicToxSet$MRN[i])
    finaltox <- rbind(finaltox, data.frame(MRN = ChronicToxSet$MRN[i], 
                                           Laterality =ChronicToxSet$Laterality[i], 
                                           MaxFatigue= ChronicToxSet$Fatigue[i],
                                           MaxFatigueDt = ChronicToxSet$DATE[i]) )
    
  } 
}













