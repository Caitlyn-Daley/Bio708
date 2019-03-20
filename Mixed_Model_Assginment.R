library(lmerTest)
library(ggplot2)
wings <- read.csv("WingData.csv")
##Mixed model analysis
##This dataset lends itself well to a mixed model because it has both random and fixed effects
##In this dataset, the following are fixed effects: Mutant, Area_mmsq
##Random effects: DGRP, Block
##DGRP is random because I don't care about the identity of the DGRP line per se.
##DGRP lines were chosen randomly to investigate phenotypic range accross genetic backgrounds
##releveling so we have the intercept as the control
wings$DGRP <- factor(wings$DGRP)
wings$Mutant <- relevel(wings$Mutant, "OREw")
##First model is the simplest model with DGRP as a random effect, and random intercept
##random intercept is useful because each DGRP strain will have bigger or smaller wings to begin with
lmer1 <- lmer(Area_mmsq ~ Mutant + (1|DGRP), data = wings)
plot(lmer1)  
summary(lmer1)
##not unexpectedly, mutants have a big effect 
#Next, we can incorperate the effect of block 
lmer4 <- lmer(Area_mmsq ~ Mutant + (1|DGRP) + (1|Block), data = wings)
summary(lmer4)
##After accounting for the variance due to DGRP, Block, and Residual, we are left with
##The variance that is due to genetic effects
##Intercept variance/Block variance + residual variance
geneticvar <- 0.013220/0.004651+0.043564
print(geneticvar)
##it doesn't look like much of the variance can be attributed to genetic effects
##which maybe suggests a sensitivity to the microenvironment i.e Block
##Next we can look at the effect of DGRP on the mutants individually
lmer2 <- lmer(Area_mmsq ~ Mutant + (Mutant| DGRP) , data = wings)
summary(lmer2)
##the variance/sd of the wildtype (intercept) and the weak mutant is less than that of the two moderate mutants
##this suggests DGRP has a bigger effect on the phenotype of the moderate mutants than the other two
##Next we can incorperate the effect of block
lmer3 <- lmer(Area_mmsq ~ Mutant + (Mutant| DGRP) + (Mutant|Block), data = wings)
summary(lmer3)
##The effect of block, or the effect of the microenvironment seems to have a bigger effect on the weak mutant
##This is the maximal model 

