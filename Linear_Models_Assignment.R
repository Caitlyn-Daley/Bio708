library(dotwhisker)
library(car)
library(emmeans)
library(tidyr)
Data <- read.csv("WingData.csv")
summary(Data)
Data$DGRP <- factor(Data$DGRP)
Data$Mutant <- relevel(Data$Mutant, "OREw")
##releveling so we have a reference level to compare to 
str(Data)
##This model accounts for the effects of each mutant and each DGRP
mutantgenotypelm <- lm(Area_mmsq ~ Mutant + DGRP + Mutant:DGRP, data = Data,
                       contrasts = list(DGRP = contr.sum))
plot(mutantgenotypelm)
summary(mutantgenotypelm)
##The drop1 fuction tells us the overall effect of each mutant indivudally
##opposed to the mutants as a whole 
drop1(mutantgenotypelm, test = "F")
## you can use this dwplot to look at the interactions between the mutant and 
##the DGRP and you can write more code to rank DGRP in terms of their effect on the mutant
dwplot(mutantgenotypelm)
OREwemmeans <- emmeans(mutantgenotypelm, "DGRP",
                         at = list(Mutant = "OREw"))
sde3emmeans <- emmeans(mutantgenotypelm, "DGRP",
                       at = list(Mutant = "sd[E3]"))
sd1emmeans <- emmeans(mutantgenotypelm, "DGRP",
                       at = list(Mutant = "sd[1]"))
sdETX4emmeans <- emmeans(mutantgenotypelm, "DGRP",
                      at = list(Mutant = "sd[ETX4]"))
##Wildtype
plot(OREwemmeans)
##sd[1]
plot(sd1emmeans)
##sd[e3]
plot(sde3emmeans)
##sd[ETX4]
plot(sdETX4emmeans)
     