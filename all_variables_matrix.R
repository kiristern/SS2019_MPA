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

#delete empty column from pivot table
sp_col_final <- sp_col[,-(2)] 

#merge all variables into their proper FID
final_table <- final_table[,-c(1,10:11)] #remove species column

final_table_distinct <- final_table %>% glimpse %>% 
  distinct  #combine all rows with same value

final_table_distinct %>% glimpse

#final_table[!duplicated(final_table), ] ##old school way of doing distinct function

#join enviro factor columns to pivot table
all_data_table <- merge(final_table_distinct, sp_col_final, by = "FID")
View(all_data_table)
#write.csv(all_data_table, "all_data_table.csv")

# renaming data table
fish <- all_data_table
head(fish)

# creating cells by species matrix for ordination
fishmatrix <- as.matrix(fish[,17:294])
dim(fishmatrix) # 3078 cells by 278 species
table(fishmatrix)
# we have numbers other than 0 and 1 in the matrix

fishmatrix <- if(fishmatrix>=1){
  fishmatrix=1
} else { 
  fishmatrix=0
}

fish$Rich <- mutate(fishmatrix, Total_richness = sum(FID))






