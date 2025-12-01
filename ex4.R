# ex4 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 4 ----

## Multiple regression ----

# Simulate data of two predictors that corrolate. (No interaction bcs no multiplacation of x1 and x2)
set.seed(187)
x1 = rnorm(200, 10, 2)
x2 = 0.5*x1 + rnorm(200, 0, 4)
y = 0.7*x1 + 2.2*x2 + rnorm(200, 0, 4)

# Fit a multiple regression model 
m = lm(y~x1+x2)

coefs = summary(m)$coef

summary(m)
# r-square is 0.868 which means that 86.8% of the variance in y is explained by the model
#   we can see why by computing the variance in the predicted values (y_hat) and divide by the total variance in y (the response variable)

y_hat = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2
var(y_hat)
var(y_hat)/var(y)

# var explained by each variable
#   same but keep other at its mean 

y_hat1 = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*mean(x2)
var(y_hat1)
var(y_hat1)/var(y)
# var expl by x1 is 1.6%

y_hat2 = coefs[1,1] + coefs[2,1]*mean(x1) + coefs[3,1]*x2
var(y_hat2)
var(y_hat2)/var(y)
# var expl by x2 is 80.6%

# compare the sum of x1 and x2 contribution to total

var(y_hat) # 85.4
var(y_hat1) + var(y_hat2) # 80.9

# the last missing percent is the covariance: Var(x+y) = Var(x) + Var(y) + 2Cov(x,y)

var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2)

# As before, we can also do this by computing V(x) = \beta_x^2\sigma_x^2.
#  (i do not really understand this)

coefs[2,1]^2*var(x1) # the variance explained by x1 alternative way to calculate


# 

t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1]


# to include covariance between predictors; use matrix notation  

t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1] # gives y_hat 
# is general, can use any nr of predictors and apply to a subset, if we want to know the variance explained by 3 out of 5 predictors 


# Method to obtain parameter estimates that reflect effect strength of each predictor
#   if all variables had same var, then var explained in response would be directly proportional to regression slope 
#   z- transform, standardize: scale to mean 0 and unit variance (sd 1)
#   z = (x - mean(x)) / sd(x)

x1_z = (x1 - mean(x1))/sd(x1)
x2_z = (x2 - mean(x2))/sd(x2)

m = lm(y ~ x1_z + x2_z)

summary(m)
# r-square is same, intercept now mean y because it is y when both x1, and x2 are 0, their new mean
# slopes have unit of SD; change in y with change of 1 SD of predictor 
#   directly shows that x2 explains more var in y than x1 does


# Another transformation: means, gives slopes units of means. allows interpreting change i y per percent change in x. these prop. slopes are known as elasticities
x1_m = (x1 - mean(x1))/mean(x1)
x2_m = (x2 - mean(x2))/mean(x2)

summary(lm(y ~ x1_m + x2_m))

## Multicollinearity ----
# when correlation too high - hard to estimate independent effects 
# rule of thumb: problem over 0.6 to 0.7 
# Assess using variance inflation factors 
# VIFi = 1 / (1 - r-square of i)
#  this r-square is for a model with the fical variable i as response and all others as predictors 
# for our example
m1 = lm(x1~x2)
r2 = summary(m1)$r.squared
1/(1-r2) # 1.04...
# low because predictors not strongly correlated 
# severe VIF is 3 to 10, then the estimates become associated with too much variance -> less reliable
# -> simplify model 


## ANCOVA ----

rm(list = ls())

set.seed(12)
x = rnorm(200, 50, 5)
gr = factor(c(rep("Male", 100), rep("Female", 100)))
y = -2 + 1.5*x + rnorm(200, 0, 5)
y[101:200] = 2 + 0.95*x[101:200] + rnorm(100, 0, 6)

plot(x, y, pch=c(1,16)[as.numeric(gr)], las=1)

m = lm(y~x*gr)
anova(m)

summary(m)

# Suppressing global intercept to get values of estimates, not contrast

m2 = lm(y ~ -1 + gr + x:gr)
summary(m2)

logLik(m) == logLik(m2) # same model as before, can confirm using log lokelihood is same 

coefs <- summary(m2)$coef

x_pred <- seq(min(x), max(x), by = 0.1)
yhat_f <- coefs[1,1] + coefs[3,1] * x_pred
lines(x_pred, yhat_f)
yhat_m <- coefs[2,1] + coefs[4,1] * x_pred
lines(x_pred, yhat_m)













































