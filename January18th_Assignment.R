library(tidyverse)
library(readr)
library(dplyr) 
##demonstrate you can source, save and load:
##saving revised dataset as a new csv file from previous assignment
#write.csv(Dataset_5, file = "NewData.csv")
##I could also use:
##save(Dataset_5, file = "Newdata.csv")
##Close R and reopen to load data
NewData <- read_csv("NewData.csv")
##I could also use: 
##load("NewData.csv")
##At this point I could source previous assignment by: 
##source('January11th_Assignment_Final.R')
##but I didn't make my own functions during that assignment,
##so this i not useful right now
##Lets examine the structure of the data file: 
#summary(NewData)
##but first we need to make Block into numbers
NewData2 <- separate(data = NewData, col = Block,
                             into = c("B", "Block"),
                             sep = "B")
NewData3 <- NewData2 [, -7]
##Check 
#head(NewData3)
##start data cleaning
##Break data up into tibbles to analyze anomolies, errors, etc. 
##This will tell us if there are any duplicate rows in our data
print(NewData3
      %>% group_by(DGRP, Mutant, Block, temperature, TotalArea, AreaPerc)
      %>% summarize(count = n())
      %>% filter(count>1)
)
##At this point I would delete the duplicate rows but I don't know how to code for that
##Next I could identify the crosses that didn't have large enough sample sizes
print(NewData3
      %>% group_by(Mutant, DGRP, Block)
      %>% summarize(count = n())
      %>% filter(count<5)
)
##I could also put data into their appropriate classes
#It might be more useful to make Mutant, DGRP and Block as factors as they are catagorical
##This dataset has only one temperature, so unsure what its class is... interger?
NewData4 <-(NewData3 %>% 
              mutate(Mutant=as.factor(Mutant)) %>% 
              mutate(DGRP=as.factor(DGRP)) %>%
              mutate(Block=as.factor(Block)) %>%
              mutate(TotalArea=as.numeric(TotalArea)) %>%
              mutate(temperature=as.numeric(temperature)))
##Check and make sure this worked
#str(NewData4)
#summary(NewData4)
##filter(NewData4, temperature != 24)
##The above code is incorrect but I am unsure how to make sure every row has temperature is 24
##I am also unsure how to code to pull out any NA's in the data, but I would also do that using the select or filter funtion?
##use plots to visualize data
##histograms
hist(NewData4$TotalArea)
print(ggplot(NewData, aes(x=TotalArea))
      + geom_histogram())
##There doesn't appear to be any outliers but I'm not really sure what this data structure indicates












