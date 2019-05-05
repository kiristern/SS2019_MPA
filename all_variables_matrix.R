library(tidyr)
library(dplyr)
library(reshape2)

final_table <- read.csv("data_for_analysis.csv")

#add presence_absence column filled with 1s (each species name = one occurence)
final_table <- final_table %>%
  mutate(Pres_Abs = 1)
View(final_table)

#create columns for each species
sp_col <- dcast(final_table, FID ~ BINOMIAL, value.var = "Pres_Abs")

#delete empty species column 
sp_col_final <- sp_col[,-(2)] 

#merge all variables into their proper FID
final_table <- final_table[,-c(1,10:11)] #remove species column

final_table_distinct <- final_table %>% glimpse %>% 
  distinct  #combine all rows with same value

final_table_distinct %>% glimpse

    #final_table[!duplicated(final_table), ] ##old school way of doing distinct function

#join enviro factor columns to species columns table
all_data_table <- merge(final_table_distinct, sp_col_final, by = "FID")

#add column of MPA presence
all_data_table$MPA_presence <- ifelse(all_data_table$WDPA_PID != " ", yes = NA, no = "MPA")
count(all_data_table, all_data_table$MPA_presence)        

#delete column WDPA_PID and Pres_Abs
all_data_table <- all_data_table[,-c(WDPA_PID, Pres_Abs)] 
View(all_data_table)

#add column with total species richness
all_data_table <- all_data_table %>%
  mutate(richness=rowSums(all_data_table[17:294]))
glimpse(all_data_table)

write.csv(all_data_table, "all_data_table.csv")
