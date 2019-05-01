library(dplyr)
library(readr)
library(R2jags)
library(coda)
library(broom.mixed)
library("ggplot2"); theme_set(theme_bw())
##Read in the data and make sure catagorical elements are factors
wings <- (read.csv("WingData.csv")
            %>% mutate(Mutant=factor(Mutant,levels=c("OREw","sd[1]","sd[e3]", "sd[ETX4]")))
            %>% mutate_if(is.character,factor)
)
##Read in data I will use for my prior
Prior_Data_DGRP <- read.csv("Chandler_DGRP_Data.csv")
##To recap, in this experiment we crossed x-linked mutants in the OREw strain (including mutant sd[e3]) to many w.t. DGRP lines
##to understand the distribution of phenotypes among genetic backgrounds 
##Prior: We can use data from another pervious experiment with mutants in the SAM strain crossed to 16 DGRP lines 
##we can use the mean wing area and the variance from this dataset to set out prior
##lets calculate the mean wing area and variance for the previous data
priormean <- mean(Prior_Data_DGRP$TotalArea)
priorvar <- var(Prior_Data_DGRP$TotalArea)
##we can also calculate precision 
priortau <- 1/priorvar
##basic linear model from last week
linearmodel <- lm(Area_mmsq ~ Mutant + DGRP + Mutant:DGRP, data = wings)
summary(linearmodel)
## Next we can create the Baysiean model with the priors from the previous dataset
##function taken from Ben 
named_list <- function (...) 
{
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) 
    nm <- snm
  if (any(nonames <- nm == "")) 
    nm[nonames] <- snm[nonames]
  setNames(L, nm)
}
##releving mutant to make them numeric so jags will run: 
wingdat1 <- with(wings,
                named_list(N=nrow(wings),
                           nmutant=length(levels(Mutant)),
                           Mutant=as.numeric(Mutant)))
##The model
Nwings <- nrow(wingdat1)
Model <- function() {
  for (i in 1:Nwings) {
    mu[i] <- b[mutant[i]]
    area[i] ~dnorm(mu[i], tau)
  }}
##defining priors
##the prior distibution has a mean of 33 and variance of 275. If we assume a broad prior with a coefficient of variation of 0.5 (50%) on either side,
##we would do 1/0.5^2 to get a shape of 4 and variance/4= 68.75
##loop to specify each mutant individually 
m1 <- function() {
    for (i in 1:Nwings) {
      mutant[i] ~ dnorm(4, 68.75)
    }}
#defining b
b1 <- function() {
  for (i in 1:Nwings) {
    mu[i] <- b[mutant[i]]
  }}
##define tau
tau <- tau~dgamma(4, 68.75)

params <- c("m1", "b1", "tau")
##Finally we can run the model using JAGS:
modelrun <- jags(data=wingdat1,
                 inits=NULL,n.iter = 500, n.thin = 100,
                 parameters=c("m1", "b1", "tau"),
                 model.file=Model, n.chains = 4)
##I'm not entirely sure why the model isn't working.... 
   

                    
                           
                           



