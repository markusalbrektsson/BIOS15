# ex3 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# variance of response = sum(variance of predictors)
# variance is additive 

## Instructions ----

set.seed(100)
groups = as.factor(rep(c("Low", "Medium", "High"), each=50)) 
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3)) # Simulate sample data
plot(groups, x, las=1, xlab="", ylab="Body size (g)")

m <- lm(x~groups) # fit a linear model
anova(m) # perform an ANOVA

SS_T = 319.97+1200.43 # adding the sums of squares of the groups factor and the residuals gives the total sums of squares 
SS_T/(150-1) # dividing the total sums of squares by n-1 gives the total variance of the sample
var(x)

319.97/SS_T # proportion of variance explained by the groups factor is the same as for r squared 

# Mean Sq is the variance attributable to each variable of the factor = Sum sq / Df
# F-ratio (test statistic of ANOVA) = Mean Sq(of factor) / Mean Sq(of Residuals)
#   thus it is ratio of among group var to within group var but also sample size (which gives resid df)

# Stat supported among-group var as here -> at least on group is different from the others
# To asses which differ, extract summary:
summary(m)
# Some similar info as ANOVA table 
# Parameter estimates: first: intercept: estimated mean for first variable of groups factor (here HIGH, alfabetic order)
#   next estimates: contrast of other variables of groups factor from intercept
#   associated tests test H0 that the variable has the same mean as the intercept variable
# To get magnitude of diff: divide contrast by intercept -> how diff that group is from intercept

# Change which is intercept, by changing levels 
groups = factor(groups, levels=c("Low", "Medium", "High")) 
m = lm(x~groups)
summary(m)


# Suppress intercept by adding -1 to predictor factor
m = lm(x~groups-1)
summary(m)$coef # just show coefficients of the summary 

confint(m) # gives the 95% CI of the groups factor









