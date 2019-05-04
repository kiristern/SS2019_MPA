#This code is to calculate the percent range coverage of threatened fish species within and 
#outside MPAs globally.
#--------------------------------------------------------------------------------------------

#Load libraries
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(reshape)
  library(reshape2)

#View Final Table
  View(final_table)

#Extract the columns we need into a new table
  MPARep <- select(final_table, FID, BINOMIAL, WDPA_PID, IUCN_CAT, label)

#Remove rows with no species
  MPARep_clean <- MPARep %>% 
    filter (BINOMIAL != " ")

#Set columns that are blank to NA
  MPARep_clean$WDPA_PID[MPARep_clean$WDPA_PID == " "] = NA
  MPARep_clean$IUCN_CAT[MPARep_clean$IUCN_CAT == " "] = NA

#Create column to identify is species is in an MPA or not (TRUE/FALSE)
  MPARep_clean_fortab <- MPARep_clean %>% 
    mutate(MPA = !(is.na(WDPA_PID)))
  
#Create Ocean key
  MPARep_clean_fortab$OLabel <- as.numeric(MPARep_clean_fortab$label)
  OceanLoc <- dcast(MPARep_clean_fortab, BINOMIAL ~ label, value.var = "OLabel")
  
#Summarise the TRUE/FALSE column into two columns called 'in_MPA, out_MPA'
  MPA_tab <- MPARep_clean_fortab %>% 
    mutate(MPA_num = as.numeric(MPA)) %>% 
    select(BINOMIAL, MPA_num) %>% 
    table() %>% as.data.frame() %>% 
    spread(key=MPA_num, value=Freq) %>% 
    slice(2:n())
  colnames(MPA_tab) = c("BINOMIAL", "in_MPA", "out_MPA")
  
#Add new columns for total # cells and the proportion calculation
  MPA_tab <- MPA_tab %>%
    group_by(BINOMIAL) %>%
    mutate(TotNum = in_MPA + out_MPA, Prop = (in_MPA/TotNum)*100)
  
#------------------------------------------------------------------------------------------  
#Create a plot to visualize proportion representation
  
  library(RColorBrewer)

  ggplot(data = MPA_tab, aes(MPA_tab$Prop)) +
    geom_histogram(breaks = seq(0, 100, by = 10),
                   col = "white",
                   aes(fill =..count..)) +
    scale_fill_gradient("Count", low = "lightsteelblue", high = "steelblue4") + 
    xlab("Proportion range coverage within MPA(%)") +
    ylab("Number of unique species") +
    theme_classic()
  
#Create a bar plot to visualize species cover by ocean
  OC <- final_table %>%
    group_by(FID) %>%
    summarise(Ocean = first(label), MPA = first(WDPA_PID))
  OC$MPA[OC$MPA == " "] <- NA
  OC$MPA[is.na(OC$MPA)] <- 0
  OC <- OC %>%
    mutate(MPA = as.numeric(MPA))
  OC$MPA[OC$MPA > 1] <- 1
  FishEnviro1 <- merge(FishEnviro, OC, by = "FID")  
  
#Species Richness by Ocean
  Ocean <- FishEnviro1 %>%
    group_by(Ocean) %>%
    summarise(SR = mean(Join_Count), MPA = sum(MPA))
  Ocean <- Ocean[-c(1),]
  
#Create Frequency plot of species richness and mpa per ocean
  ggplot(data = Ocean, aes(x = Ocean, y = SR)) +
    geom_line(color = "darkslategray4", size = 3) +
    theme_classic()
    
    
    
    
    geom_histogram(breaks = seq(0, 100, by = 10),
                   col = "white",
                   aes(fill =..count..)) +
    scale_fill_gradient("Count", low = "lightsteelblue", high = "steelblue4") + 
    xlab("Proportion range coverage within MPA(%)") +
    ylab("Number of unique species") +
    theme_classic()
  
