## ----setup,echo=F,results=F,cache=F--------------------------------------
library(broman) # used for myround 

## ----echo=F--------------------------------------------------------------
set.seed(23) 

## ------------------------------------------------------------------------
## Make 10 draws with replacement from {1,2,3,4,5,6}
## This models 10 realizations of rolling a fair die
sample(1:6,size=10,replace=TRUE)

