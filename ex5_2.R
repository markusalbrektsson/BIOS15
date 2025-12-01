# ex5_2 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

# Exercise 5_2 ----

## Example: basics ----

x <- rpois(200, 3) # creates a poisson distribution of n values, with defined lambda
hist(x, las=1)

# lambda equals the mean and variance of the distribution 


# Three different lambdas 

x = seq(0, 20, 1)
y = dpois(x, lambda=1)
plot(x,y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)
points(x, dpois(x, lambda=3), type="b", pch=16, col=2)
points(x, dpois(x, lambda=10), type="b", pch=16, col=3)
legend("topright", col=1:3, pch=16, 
       legend=c(expression(paste(lambda, " = 1")),
                expression(paste(lambda, " = 3")),
                expression(paste(lambda, " = 10"))))


# Creating data for exercise

x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = ceiling(exp(eta + rpois(200, 0.3))) 

par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)
par(mfrow=c(1,1))

# Fitting a Poisson regression to created data 

m <- glm(formula = y ~ x, family = "poisson")
summary(m)


# Plot predicted values from the model using predict(), and adding 95% CI (= +/- 1.96 SE)

plot(x, y, las=1, col="darkgrey", pch=16)
xx = seq(min(x), max(x), 0.01)
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
#lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
#lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)




# Deviance in place of r2 to assess fit of the model 

#   Old pseudo r2
# McFaddens: 0.2-0.4 indicate excellent fit  
1-(m$deviance/m$null.deviance)
#   Nother pseudo r2 option
r.squaredGLMM(m)


## Example: Overfitting ----
rm(list = ls())

set.seed(1)
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = floor(exp(eta + rnbinom(200, 1, mu=.8)))

par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)
par(mfrow=c(1,1))

m = glm(y~x, family="poisson")
summary(m)
# OVERDISPERSION

# Negative binomial
# is similar to poisson but includes a parameter modelling the disproportionate increase in variance with increasing mean 
library(MASS)
m = glm.nb(y~x)
summary(m)

## Hurdle ----

# Seperate 0 values and others, fit two models, combine them 
# see below 



y1 = ((y>1)*1)
m1 = glm(y1~x, family="binomial" (link="logit"))

y2 = y
y2[which(y==0)] = NA

m2 = glm(y2~x, family="poisson", na=na.exclude)




coefs1 = summary(m1)$coef
coefs2 = summary(m2)$coef
y_hat1 = coefs1[1,1] + coefs1[2,1]*x
y_hat2 = coefs2[1,1] + coefs2[2,1]*x

y_pred = invlogit(y_hat1)*exp(y_hat2)

par(mfrow=c(1,3))
plot(x, invlogit(y_hat1), las=1)
plot(x, exp(y_hat2), las=1)
plot(x, y_pred, las=1)
par(mfrow=c(1,1))







