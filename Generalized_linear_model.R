library(tidyr)
wings <- read.csv("WingData.csv")
summary(Data)
wings$DGRP <- factor(wings$DGRP)
wings$Mutant <- relevel(wings$Mutant, "OREw")
##releveling so we have the control as the reference level to compare to 
str(wings)
##all of my variables in my dataset are continuous, so to make a GLM make sense,
##I will use a gamma distribution with a log/link. 
##This general linear model accounts for the effects of each mutant and each DGRP
generallm <- glm(Area_mmsq ~ Mutant + DGRP + Mutant:DGRP,
                 wings, family= Gamma(link="log"))

## BMB: NOT reproducible.  I had to call Data "wings" instead?
summary(generallm)
plot(generallm)
##the linear model appears to be a better fit than the generalized linear model
##ideally for this dataset you would use a mixed model rather than a generalized linear model to account for fixed 
##effects (i.e Mutant, Sex, Temperature) and random effects (DGRP, Block)

## BMB: true, at least in terms of the Q-Q plot.
## it would 

loglm <- lm(log(Area_mmsq) ~ Mutant + DGRP + Mutant:DGRP,
                 wings)
AIC(loglm)+sum(2*log(wings$Area_mmsq))

                 wings)
AIC(reglm)
AIC(generallm)

## https://stats.stackexchange.com/questions/61332/comparing-aic-of-a-model-and-its-log-transformed-version

## BMB: would be interesting to do a more careful comparison of log-linear GLM (i.e. log transform wing area), Gamma GLM, regular GLM (maybe try Box-Cox transformation too?)

## score: 2.25
