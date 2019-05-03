library("tidyverse")
library("ggplot2")

Fish <- read.csv("Fish_MPA_Ocean_SJ.csv", header = TRUE)

head(Fish)

fish_selection <- select(Fish, TARGET_FID, BINOMIAL, LEGEND, WDPA_PID, NAME, DESIG, DESIG_TYPE, 
                         IUCN_CAT, GIS_AREA, label)
View(fish_selection)
