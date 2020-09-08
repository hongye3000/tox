library(readr)
TOXICITY_Temp <- read_csv("TOXICITY_Temp.csv")
na.omit(TOXICITY_Temp)
View(TOXICITY_Temp)