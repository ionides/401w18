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
N <- 50000 ; sigma <- 1 ; d <- 10 ; set.seed(23)
X <- matrix(rnorm(N*(d+1),mean=0,sd=sigma),nrow=N)

## ----T_eval--------------------------------------------------------------
T_evaluator <- function(x) x[d+1] / sqrt(sum(x[1:d]^2)/d) 

## ----T_sim---------------------------------------------------------------
Tsim <- apply(X,1,T_evaluator)

## ----T_plot_code,echo=T,eval=F-------------------------------------------
## hist(Tsim,freq=F,main="",
##   breaks=30,ylim=c(0,0.4))
## x <- seq(length=200,
##   min(Tsim),max(Tsim))
## lines(x,dnorm(x),
##   col="blue",
##   lty="dashed")
## lines(x,dt(x,df=d),
##   col="red")

## ----T_plot,echo=F,eval=T,fig.width=4,fig.height=3,out.width="2.5in"-----
par(mai=c(0.8,0.8,0.1,0.1))
hist(Tsim,freq=F,main="",
  breaks=30,ylim=c(0,0.4))
x <- seq(length=200,
  min(Tsim),max(Tsim))
lines(x,dnorm(x),
  col="blue",
  lty="dashed")
lines(x,dt(x,df=d),
  col="red")

## ----range---------------------------------------------------------------
range(Tsim)

## ----tail_z--------------------------------------------------------------
2*(1-pnorm(5))
2*(1-pnorm(6))

## ----tail_t--------------------------------------------------------------
2*(1-pt(5,df=d))
2*(1-pt(6,df=d))

## ----data----------------------------------------------------------------
goals <- read.table("FieldGoals2003to2006.csv",header=T,sep=",")
goals[1:5,c("Name","Teamt","FGt","FGtM1")]
lm0 <- lm(FGt~FGtM1+Name,data=goals)

## ----factor_class--------------------------------------------------------
class(goals$Name)

## ----design--------------------------------------------------------------
X <- model.matrix(lm0)
dim(X)
unname(X[c(1,5,9,13,17),1:8])

## ----anova---------------------------------------------------------------
anova(lm0)

