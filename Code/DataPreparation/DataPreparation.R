#===============================================================================
# purpose: Data preparation - KSI data
# author: tirthankar chakravarty
# comments:
# TODO:
#===============================================================================

rm(list = ls())

#==========================================================
# UK KSI
#==========================================================
dfKSI = read.csv("Data//Raw/UKdriversKSI.txt", header = TRUE)
colnames(dfKSI) = "Drivers"
dfKSI$Date = seq.Date(from = as.Date("1969-01-01"), 
                      by = "month", length.out = 192)
dfKSI$logDrivers = log(dfKSI$Drivers)

dfPetrol = read.table("Data//Raw//logUKpetrolprice.txt", header = FALSE, skip = 1)
colnames(dfPetrol) = "logPetrol"
dfPetrol$Date = seq.Date(from = as.Date("1969-01-01"), 
                      by = "month", length.out = 192)


dfKSI = merge(dfPetrol, dfKSI, by = "Date")
saveRDS(dfPetrol, "Data/Processed/dfPetrol.rds")
saveRDS(dfKSI, "Data/Processed/dfKSI.rds")

#==========================================================
# Norway fatalities
#==========================================================
dfNorway = read.table("Data//Raw/NorwayFinland.txt", skip = 1, 
                      header = FALSE, sep = "\t")
colnames(dfNorway) = c("Year", "Norway", "Finland")
dfNorway$logFatalities = log(dfNorway$Norway)
saveRDS(dfNorway, "Data//Processed/dfNorway.rds")