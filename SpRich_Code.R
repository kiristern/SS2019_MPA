##This script is to clean the Species Richness table on Endemic Fish species around the world
##----------------------

#Set working directory
  setwd('C:/Users/danie/OneDrive/Documents/SFU/Courses/BISC890/MPA_Project/SS2019_MPA')

#load libraries
  library(dplyr)
  library(ggplot2)

#Load csv file
  SpRich <- read.csv('Fish_Rich_SJ.csv', header = TRUE)
  Rich_Select <- select(SpRich, TARGET_FID, BINOMIAL, Latitude, Longitude)  
  