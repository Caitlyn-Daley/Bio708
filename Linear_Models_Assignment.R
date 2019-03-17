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
## BMB: could use Mutant*DGRP equivalently (but this is OK too)
plot(mutantgenotypelm)

## BMB: what do you conclude about the diagnostics??

summary(mutantgenotypelm)
##The drop1 function tells us the overall effect of each mutant individually
##opposed to the mutants as a whole
## BMB: isn't it kind of the opposite? drop1() gives *overall* p-value
## -- also, it gives the p-value for the *interaction*
drop1(mutantgenotypelm, test = "F")

## you can use this dwplot to look at the interactions between the mutant and 
##the DGRP and you can write more code to rank DGRP in terms of their effect on the mutant
dwplot(mutantgenotypelm)

## BMB how about:
cc <- broom::tidy(mutantgenotypelm,conf.int=TRUE)
library(cowplot)
cc_main <- droplevels(subset(cc, term!="(Intercept)" & !grepl(":",term)))
cc_main$term <- reorder(cc_main$term, cc_main$estimate)
cc_inter <- droplevels(subset(cc, grepl(":",term)))
cc_inter$term <- reorder(cc_inter$term, cc_inter$estimate)

gg_main <- ggplot(cc_main, aes(term, estimate))+
    geom_pointrange(aes(ymin=conf.low,ymax=conf.high))+
    scale_y_continuous(limits=c(-1,1))+
    coord_flip()

gg_inter <- gg_main %+% cc_inter
pdf("linmodels.pdf",height=25,width=10)
plot_grid(gg_main,gg_inter)
dev.off()

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
## BMB: can you explain what all of these mean?
## score: 2
