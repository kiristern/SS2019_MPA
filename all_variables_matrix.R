library(tidyr)
library(dplyr)
library(reshape2)

final_table <- read.csv("data_for_analysis.csv")

#add presence_absence column filled with 1s (each species name = one occurence)
final_table <- final_table %>%
  mutate(Pres_Abs = 1)
View(final_table)

#make pivot table of Grid cell ~ species 
pt <- dcast(final_table, FID ~ BINOMIAL, value.var = "Pres_Abs")

#delete empty column from pivot table
pt <- pt[,-(2)] 
