## ----setup,echo=F,results=F,cache=F--------------------------------------
# library(broman) # used for myround 
par(mai=c(0.8,0.8,0.1,0.1))

## ----gpa_data------------------------------------------------------------
gpa <- read.table("gpa.txt",header=T); head(gpa,3)

## ----lm_a----------------------------------------------------------------
lm1 <- lm(GPA~ACT+High_School*Year,data=gpa) 
coef(summary(lm1))[,1:2]

## ----lm_b----------------------------------------------------------------
head(model.matrix(lm1))

## ------------------------------------------------------------------------
lm2 <- lm(GPA~ACT+High_School+Year+High_School:Year,data=gpa)
head(model.matrix(lm2),4)

## ------------------------------------------------------------------------
lm3 <- lm(GPA~ACT*High_School,data=gpa)

## ------------------------------------------------------------------------
coef(summary(lm3))[,1:2]

## ------------------------------------------------------------------------
s3 <- summary(lm3)$sigma
lm4 <- lm(GPA~ACT+High_School,data=gpa)
s4 <- summary(lm4)$sigma
lm5 <- lm(GPA~1,data=gpa)
s5 <- summary(lm5)$sigma
cat("s3 =",s3,"; s4 =",s4,"; s5 =",s5)

## ------------------------------------------------------------------------

## ----data----------------------------------------------------------------
goals <- read.table("FieldGoals2003to2006.csv",header=T,sep=",")
goals[1,c("Name","Teamt","FGt","FGtM1")]
lm6 <- lm(FGt~FGtM1*Name,data=goals)

## ------------------------------------------------------------------------
X<-model.matrix(lm6) ; colnames(X)<-1:38 ; X[1:17,c(1:8,21:26)]

## ------------------------------------------------------------------------
anova(lm6)

