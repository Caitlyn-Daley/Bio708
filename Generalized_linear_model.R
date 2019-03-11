library(tidyr)
Data <- read.csv("WingData.csv")
summary(Data)
Data$DGRP <- factor(Data$DGRP)
Data$Mutant <- relevel(Data$Mutant, "OREw")
##releveling so we have the control as the reference level to compare to 
str(Data)
##all of my variables in my dataset are continuous, so to make a GLM make sense,
##I will use a gamma distribution with a log/link. 
##This general linear model accounts for the effects of each mutant and each DGRP
generallm <- glm(Area_mmsq ~ Mutant + DGRP + Mutant:DGRP, wings, family= Gamma(link="log"))
summary(generallm)
plot(generallm)
##the linear model appears to be a better fit than the genrealized linear model
##ideally for this dataset you would use a mixed model rather than a genrealized linear model to account for fixed 
##effects (i.e Mutant, Sex, Temperature) and random effects (DGRP, Block)
