## ------------------------------------------------------------------------

library(faraway)
library(ggplot2)
data("seatpos")
head(seatpos)


## ------------------------------------------------------------------------

comfort <- lm(hipcenter ~ ., data = seatpos)
summary(comfort)


## ---- warning=FALSE------------------------------------------------------

library(MASS)
data(nels88)

ses_edu_lm <- lm(math ~ ses + paredu + paredu:ses, data = nels88)


## ------------------------------------------------------------------------
anova(ses_edu_lm)

