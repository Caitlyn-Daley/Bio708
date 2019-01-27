library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
##Please see QMEE/README file on git hub for why I have chosen the plots below to visualize my data
WingArea <- read.csv("CleanData.csv")
##summary(WingArea)
##DGRP is not a continuous variable (its catagorical), so we need to fix this
WingArea1 <-(WingArea %>% 
              mutate(Mutant=as.factor(Mutant)) %>% 
              mutate(DGRP=as.factor(DGRP)) %>%
              mutate(Block=as.factor(Block)) %>%
              mutate(TotalArea=as.numeric(TotalArea)) %>%
              mutate(temperature=as.numeric(temperature)))
##summary(WingArea1)
##Lets first look at a plot to see the general distribution of phenotypes
p1 <- ggplot(WingArea1,aes(Mutant,TotalArea, colour=DGRP))
print(p1+geom_boxplot())
##Now lets look at blocks to see if we see any anomolies
theme_set(theme_bw())
p2 <- ggplot(WingArea1,aes(Mutant,TotalArea, colour=Block))
print(p2+geom_boxplot())
##Something werid is going on with OREw Block 3 
##We could also look at a violin plot 
p3 <- ggplot(WingArea1,aes(Mutant,TotalArea, colour=Block))
print(p3+geom_violin())
##There is maybe an outlier in OREw block 6 and sd[1] block 8
##I am not sure if using a graph is the right way to determine if the rank order of alleles remained fixed
##i.e OREw>sd[1]>sd[E3]>sd[ETX4]
##I think it would be better to write a script that loops through each DGRP until 
##OREw>sd[1]>sd[E3]>sd[ETX4] = FALSE? 
##I could also try and make a line graph to look if the lines follow the same trend
##I am using the mean to repsent the phenotype for each mutant/DGRP line combo 
Mutant_MeanArea <- WingArea1 %>%
  group_by(DGRP, Mutant) %>%
  summarize(mean_area = mean(TotalArea, na.rm = TRUE))
p5 <- ggplot(Mutant_MeanArea,aes(Mutant,mean_area,colour=DGRP))
print(p5+geom_line()
##This doesn't work since each is only one observation and I'm unsure why it wont plot





