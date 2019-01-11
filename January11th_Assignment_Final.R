##Assignment for January 11th, 2018. 
##Author: Caitlyn Daley
##Load the required packages
library(tidyverse)
library(readr)
library(dplyr) 
##Call in the dataset into R
Dataset <- read_csv("CD_New_Combined_WingArea.csv")
##Get rid of columns we don't care about (keeping just Wing Area and Block)
Dataset_2 <- select(Dataset, -Count, -AreaPerc, -Mean, -AverageSize)
##Check to make sure we got rid of the right things
str(Dataset_2)
##Split the file name into sensible columns
Dataset_3 <- separate(data = Dataset_2, col =  Slice, 
                      into = c("Mutant", "DGRP", "replicate", "temperature", "Individual"), 
                      sep = "_")
##Do a substantive calculation using the data
##Calculate the mean wing area for each DGRP cross
Mutant_MeanArea <- Dataset_3 %>%
  group_by(DGRP, Mutant) %>%
  summarize(mean_area = mean(TotalArea, na.rm = TRUE))
##Look at means in each block
Block_MeanArea <- Dataset_3 %>%
  group_by(DGRP, Mutant, Block) %>%
  summarize(mean_area = mean(TotalArea, na.rm = TRUE))
#Calculate standard deviation 
Block_sdArea <- Dataset_3 %>%
  group_by(DGRP, Mutant, Block) %>%
  summarize(sd_area = sd(TotalArea, na.rm = TRUE))
