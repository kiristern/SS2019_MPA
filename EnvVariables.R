#DATA IMPORTATION
Coast <- read.csv("Coastline_Length_KM.csv", header = TRUE)
DOxy <- read.csv("DOxy_SJ_Mean.csv", header = TRUE)
PProd <- read.csv("PProd_SJ_Mean.csv", header = TRUE)
SeaTemp <- read.csv("SST_SJ_Mean.csv", header = TRUE)
Fish <- read.csv("Fish_Rich_SJ.csv", header = TRUE)



#CREATE EnvVariables MATRIX, which lists SUM_Length, DOXY_MN, PPROD_MN & SST_MEAN classified by FID
EnvCoast<-select(Coast, FID, SUM_Length) 
EnvDOxy<-select(DOxy, FID, DOXY_MN)
EnvPProd<-select(PProd, FID, PPROD_MN)
EnvSeaTemp<-select(SeaTemp, FID, SST_MEAN)
EnvVariables1<-merge(EnvCoast, EnvDOxy)
EnvVariables2<-merge(EnvPProd, EnvSeaTemp)
EnvVariables<-merge(EnvVariables1, EnvVariables2)

EnvVariables
