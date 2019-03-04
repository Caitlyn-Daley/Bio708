##Assignment 5
##Permutation tests
##Author: Caitlyn Daley

##See README.txt file on GitHub for hypothesis tests
## BMB: I can't find README.txt; there's a README.md ?
##  doesn't seem to have specific hypotheses relating to this assignment?

## BMB: do you need all of these?
##   are you actually using coin?
##   tidyr is included in tidyverse, I think? try tidyverse::tidyverse_packages()
library(lmPerm)
library(coin)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(gtools)
library(ggplot2)
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
## BMB: or WingDat %>% select(-c(temperature,TotalArea,AreaPerc,Block))
##create a vector with the data for just the two groups 
PermuteData <- newdata %>%
    ## BMB THIS DOES NOT WORK THE WAY YOU THINK IT DOES ...
    ## filter(Mutant == c("OREw", "sd[E3]")) %>%
    filter(Mutant %in% c("OREw", "sd[E3]")) %>% 
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
##Here we will ultimately want to use a t statistic because we want to compare the means of the w.t. and the mutant allele.
## BMB: for a univariate test call it "t statistic" not "T statistic" ...

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
hist(avgdiff,xlim=c(-0.7,0.05))
abline(v=obsdiff, col="red", lwd=2)
#the proportion of permutations with larger difference
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)
## BMB note this is == 1/1001 (no permutations led to values more extreme than observed)
##Alternative code
nsim <- 1000
res0 <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(PermuteData))
  bdat <- transform(PermuteData,Area_mmsq=Area_mmsq[perm])
  res0[i] <- mean(bdat[bdat$Mutant=="OREw","Area_mmsq"])-
    mean(bdat[bdat$Mutant=="sd[E3]","Area_mmsq"])
}
obs <- mean(PermuteData[PermuteData$Mutant=="OREw","Area_mmsq"])-
  mean(PermuteData[PermuteData$Mutant=="sd[E3]","Area_mmsq"])
##add the observed value to the list of results
res <- c(res0,obs)
hist(res0,col="gray",las=1,main="",xlim=range(res))
abline(v=obs,col="red")
##This looks a little wonky and I'm not sure how to fix it
## BMB: it's wonky because we included the extreme value.
## we could also have increased the "breaks=" argument

##However, Compute the test statistic as that is what we are interested in
## BMB: this is **not** how we use the t-test to do permutation tests.
## this simply does a t-test on the original data.
(tt <- t.test(Area_mmsq~Mutant,data=PermuteData,var.equal=TRUE))
##incredibly small P value, helps confirm what we already know... it doesn't happen by chance. Great.
## BMB: more specifically, you again want
2*(mean(res>=obs))
## which is again equal to 2/1001

####################################
#Second hypothesis: 
##Variation in wing area can be accounted for by genotype (i.e either wt. or sd[E3]mutant)
##in this case an F statistic will be more appropriate as we are interested in variances, rather than means 
##Here we will incorperate a linear model as we are interested in whether genotype (ie. DGRP) predicts variation in wing size(area)
#Will stick with just the wildtype/sd[e3] example to keep it simple
##Use an ANOVA as although we are doing essentially a t test(we have only 2 groups),  we want the F statistic rather than the t statistic,
##Because we want variance rather than means
## BMB: why ... ? (you'll get exactly the same p-value ...)
Mutant.Anova = lm(Area_mmsq ~ Mutant, data = PermuteData)
##intercept by default is the w.t. 
summary(Mutant.Anova) ##Look at model summary stats
## BMB: summary(Anova) didn't work! Automatic penalty for code that
##  doesn't replicate ...
Mutant.Anova.Fstat <- summary(Mutant.Anova)$fstat ##Just pull out the Fstat
Mutant.Anova.Fstat1 <- Mutant.Anova.Fstat[1] ##just pulls the main value for later 
##lets start with the permutation test, using the modeled values 
N = 1000
PermuteFunction <- function(y=PermuteData$Area_mmsq, x=PermuteData$Mutant) {
  model.resample = lm(sample(y, replace = F) ~ x)
  fstats = summary(model.resample)$fstat[1]
  return(fstats)}
##This histogram will just show us a distribution of F values under the null model (No association between wing area and genotype)
## BMB: Never say "no association"!  Say "no clear association" ...
Permute_fstats <- replicate(N, PermuteFunction())
hist(Permute_fstats)
##Just like before, we can compare this to our observed value 

## score: 2.25
