install.packages("readxl")
library(readr)
library(readxl)

#Read csv file which was exported from ACCESS database
TOXICITY_Temp <- read_csv("5 TOXICITY MASTER TABLE v4.csv")
#TOXICITY_Temp <- read_excel("5 TOXICITY MASTER TABLE v4.xlsx", sheet_i=_5_TOXICITY_MASTER_TABLE_v4)
na.omit(TOXICITY_Temp)
View(TOXICITY_Temp)

#Link to ODBC
#install.packages(RODBCDBI)
#library(RODBCDBI)
#mdbConnect <- odbcConnectAccess("S:/RadOnc/Database/Breast/BREAST_DATABASE_NEW")
