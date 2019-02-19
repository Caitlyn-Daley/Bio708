##Assignment 6
##Permutation tests
##Author: Caitlyn Daley

##See README.txt file on GitHub for hypothesis tests
library(lmPerm)
library(coin)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(gtools)
library(ggplot2);
##set theme for to make plots "prettier"
theme_set(theme_bw())
##call in the dataset
WingData <- read.csv("NewData.csv")
##str(WingData)

##I've been meaning to convert "WingArea" to more sensible units (mmsq instead of pixels)
WingData$Area_mmsq <- WingData$TotalArea * (0.00215^2)

##Since we can really only do permutation tests to compare 2 groups,
##I will create a subset from my dataset with just 2 groups: The wildtype (control) and one mutant
#levels(WingData$Mutant)

##droping all the columns we won't need
myvars <- names(WingData) %in% c("temperature", "TotalArea", "AreaPerc", "Block") 
newdata <- WingData[!myvars]
##create a vector with the data for just the two groups 
PermuteData <- newdata %>%
  filter(Mutant == c("OREw", "sd[E3]")) %>% 
  droplevels()
#summary(PermuteData)

##first lets visualize the data
(ggplot(PermuteData,aes(Mutant,Area_mmsq))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)
##1st hypothesis: Mutants develop smaller wings than the wildtype 
##we already know this to be true, but I guess we can check to make sure
##Here we will ultimately want to use a T statistic because we want to compare the means of the w.t. and the mutant allele.
Control <- filter(WingData,Mutant=="OREw") %>% select(Area_mmsq) %>% unlist
Mutant <- filter(WingData,Mutant=="sd[E3]") %>% select(Area_mmsq) %>% unlist
obsdiff <- mean(Mutant)-mean(Control)
##lets start with ~1000 simulations 
N <- 1500
avgdiff <- replicate(1000, {
  all <- sample(c(Control,Mutant))
  newcontrols <- all[1:N]
  newmutants <- all[(N+1):(2*N)]
  return(mean(newmutants) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)
#the proportion of permutations with larger difference
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)
##Alternative code
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(PermuteData))
  bdat <- transform(PermuteData,Area_mmsq=Area_mmsq[perm])
  res[i] <- mean(bdat[bdat$Mutant=="OREw","Area_mmsq"])-
    mean(bdat[bdat$Mutant=="sd[E3]","Area_mmsq"])
}
  obs <- mean(PermuteData[PermuteData$Mutant=="OREw","Area_mmsq"])-
  mean(PermuteData[PermuteData$Mutant=="sd[E3]","Area_mmsq"])
##add the observed value to the list of results
res <- c(res,obs)
hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")
##This looks a little wonky and I'm not sure how to fix it
##However, Compute the test statistic as that is what we are interested in 
(tt <- t.test(Area_mmsq~Mutant,data=PermuteData,var.equal=TRUE))
##incredibly small P value, helps confirm what we already know... it doesn't happen by chance. Great. 
####################################
#Second hypothesis: 
##Variation in wing area can be accounted for by genotype (i.e either wt. or sd[E3]mutant)
##in this case an F statistic will be more appropriate as we are interested in variances, rather than means 
##Here we will incorperate a linear model as we are interested in whether genotype (ie. DGRP) predicts variation in wing size(area)
#Will stick with just the wildtype/sd[e3] example to keep it simple
##Use an ANOVA as although we are doing essentially a T test(we have only 2 groups),  we want the F statistic rather than the T statistic,
##Because we want variance rather than means 
Mutant.Anova = lm(Area_mmsq ~ Mutant, data = PermuteData)
##intercept by default is the w.t. 
summary(Mutant.Anova) ##Look at model summary stats
Mutant.Anova.Fstat <- summary(Anova)$fstat ##Just pull out the Fstat
Mutant.Anova.Fstat1 <- Mutant.Anova.Fstat[1] ##just pulls the main value for later 
##lets start with the permutation test, using the modeled values 
N = 1000
PermuteFunction <- function(y=PermuteData$Area_mmsq, x=PermuteData$Mutant) {
  model.resample = lm(sample(y, replace = F) ~ x)
  fstats = summary(model.resample)$fstat[1]
  return(fstats)}
##This histogram will just show us a distribution of F values under the null model (No assosiation between wing area and genotype)
Permute_fstats <- replicate(N, PermuteFunction())
hist(Permute_fstats)
##Just like before, we can compare this to our observed value 
