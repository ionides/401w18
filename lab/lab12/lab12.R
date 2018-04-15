# The data set comes from the 1998 General Socail Survey. This data set is 
# analyzed in the paper 'Partisan Differences in Charitable Giving: Evidence
# from Individual-Level Survey Data' by Michele F. Margolis and Michael W. Sances

# Similar as HW9, you are asked to explore this data to find statistically 
# supported evidence of associations that might explain indivial charity giving 
# (In_totgiv).

# The gss.csv file can be downloaded from Canvas

# Read the file
gss = read.csv("C:/Users/sy/Downloads/gss.csv")
dim(gss)
summary(gss)

gss = gss[,-1]
# Change region into factor
gss$region = as.factor(gss$region)
# Notice the democrat,republican... should also be factor
# But since their values are either 0 or 1, fine to keep them as integer

hist(gss$ln_totgiv)
# May potentially be a problem since its distribution has two mode

# fit a model with all the covariates
fit1 = lm(ln_totgiv ~ ., data = gss)
summary(fit1)
# This is because independet is redundant if democrat and republican are already
# provided; Similar for moderate

# Remove independet and moderate from the model
fit2 = lm(ln_totgiv ~ . - independent - moderate, data = gss)
summary(fit2)
# Seems that some efficients are not significant
# Want to remove them one by one

# Remove race
fit3 = lm(ln_totgiv ~ . - independent - moderate - white - black, data = gss)
summary(fit3)
anova(fit3, fit2) # ok to remove

# Remove political ideology
fit4 = lm(ln_totgiv ~ . - independent - moderate - white - black - 
            liberal - conservative, data = gss)
summary(fit4)
anova(fit4, fit3) # ok to remove

# Remove family size and gender
fit5 = lm(ln_totgiv ~ . - independent - moderate - white - black - 
            liberal - conservative - family_size - male, data = gss)
summary(fit5)
anova(fit5, fit4) # ok to remove

# Remove relig
fit6 = lm(ln_totgiv ~ democrat + republican + attend + income + married +
            age + hs + coll + grad + factor(region), data = gss)
summary(fit6)
anova(fit6, fit5) # ok to remove

# Remove region
fit7 = lm(ln_totgiv ~ democrat + republican + attend + income + married +
            age + hs + coll + grad, data = gss)
summary(fit7)
anova(fit7, fit6) # probably ok to remove

# use fit 7 as final model

# Intepretation

# Model diagnostics
plot(gss$ln_totgiv,resid(fit7), ylab="residuals", xlab="Total giving")
abline(0,0)
# some patterns can be observed

qqnorm(resid(fit7))
qqline(resid(fit7))
# residual may not be normal

# Out of the scope of this course
# One possible solution to above issue, use a logistic regression
gss$givingyes = (gss$ln_totgiv>0)
fit8 = glm(givingyes ~ democrat + republican + attend + income + married +
                age + hs + coll + grad, data = gss, family = binomial(link="logit"))
summary(fit8)



