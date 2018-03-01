## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 

## ----reconstruct_variables,echo=F----------------------------------------
L <- read.table(file="life_expectancy.txt",header=TRUE)
L_fit <- lm(Total~Year,data=L)
L_detrended <- L_fit$residuals
U <- read.table(file="unemployment.csv",sep=",",header=TRUE)
U_annual <- apply(U[,2:13],1,mean)
U_detrended <- lm(U_annual~U$Year)$residuals
L_detrended <- subset(L_detrended,L$Year %in% U$Year)
lm1 <- lm(L_detrended~U_detrended)

## ----lm------------------------------------------------------------------
c1 <- summary(lm(L_detrended~U_detrended))$coefficients ; c1
beta_U <- c1["U_detrended","Estimate"]
SE_U <- c1["U_detrended","Std. Error"]
z <- qnorm(1-0.05/2) # for a 95% CI using a normal approximation
cat("CI = [", beta_U - z * SE_U, ",", beta_U + z * SE_U, "]")

## ----sim-----------------------------------------------------------------
N <- 1000 ; sigma <- 1 ; d <- 10
X <- matrix(rnorm(N*(d+1),mean=0,sd=sigma),nrow=N)

## ----T_eval--------------------------------------------------------------
T_evaluator <- function(x) x[d+1] / sqrt(sum(x[1:d]^2)/d) 

## ----T_sim---------------------------------------------------------------
T_simulated <- apply(X,1,T_evaluator)

## ----T_plot,fig.width=4,fig.height=3,out.width="3in",echo=T,eval=F-------
## hist(T_simulated)

## ------------------------------------------------------------------------


