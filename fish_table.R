#Load libraries
library(tidyverse)

#DATA IMPORTATION
Coast <- read.csv("Coastline_Length_KM.csv", header = TRUE)
DOxy <- read.csv("DOxy_SJ_Mean.csv", header = TRUE)
PProd <- read.csv("PProd_SJ_Mean.csv", header = TRUE)
SeaTemp <- read.csv("SST_SJ_Mean.csv", header = TRUE)
Richness <- read.csv("Fish_Rich_SJ.csv", header = TRUE)
Fish <- read.csv("Fish_MPA_Ocean_SJ.csv", header = TRUE)

#CREATE EnvVariables MATRIX, which lists SUM_Length, DOXY_MN, PPROD_MN & SST_MEAN classified by FID
EnvCoast<-select(Coast, FID, SUM_Length) 
EnvDOxy<-select(DOxy, FID, DOXY_MN)
EnvPProd<-select(PProd, FID, PPROD_MN)
EnvSeaTemp<-select(SeaTemp, FID, SST_MEAN)
EnvVariables1<-merge(EnvCoast, EnvDOxy)
EnvVariables2<-merge(EnvPProd, EnvSeaTemp)
EnvVariables<-merge(EnvVariables1, EnvVariables2)

#Select out the columns to keep for fish
Sp_Rich <- select(Richness, TARGET_FID, Join_Count, Latitude, Longitude)

#Merge enviro data with species richness data
FishEnviro <- merge(EnvVariables, Sp_Rich, by.x = 'FID', by.y = 'TARGET_FID')

#create table with selected data from Fish
fish_selection <- select(Fish, TARGET_FID, BINOMIAL, LEGEND, WDPA_PID, NAME, DESIG, DESIG_TYPE, 
                         IUCN_CAT, GIS_AREA, label)

#Merge FishEnviro table with fish_selection table
final_table <- merge(FishEnviro, fish_selection, by.x = "FID", by.y = "TARGET_FID", all = TRUE)
View(final_table)
