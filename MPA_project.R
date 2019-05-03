library("tidyverse")
library("ggplot2")

Coast <- read.csv("Coastline_Length_KM.csv", header = TRUE)
DOxy <- read.csv("DOxy_SJ_Mean.csv", header = TRUE)
PProd <- read.csv("PProd_SJ_Mean.csv", header = TRUE)
SeaTemp <- read.csv("SST_SJ_Mean.csv", header = TRUE)
Fish <- read.csv("Fish_Rich_SJ.csv", header = TRUE)

