# ex5 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 5 ----

## Binomial distribution ----

# Bernoulli trials (1 or 0 outcome)
# proportion, p, of the n trials with a positive outcome

# variance of binomial distribution is given by: var = np(1-p)

x <- seq(from=0, to=1, by=0.01)
v_b <- x*(1-x) # Binomial variance
plot(x, v_b, type="l", xlab="Probability", ylab="Theoretical variance", las=1)

# Explained: 
# If e.g. one population has a mean of 0.5, if will be expected to be much more 
# variable than a second population with a mean of 0.1, 
# just because there is less opportunity to vary.

# The logit transformation
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

x = runif(200)
logit_x = logit(x)

par(mfrow=c(2,2))
hist(x, las=1)
hist(logit_x, las=1)

xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit (x)",
     ylab="P")
plot(x, invlogit(logit_x), las=1)
par(mfrow=c(1,1))

# Probit similar to logit
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit/Probit (x)",
     ylab="P")
lines(xx, pnorm(xx), lty=2)
legend("topleft", legend=c("Logit", "Probit"),
       lty=c(1,2), bty="n")

## Logistic regression ----

# creating data to demonstrate 
x = rnorm(200, 10, 3) 
eta = -2 + 0.4*x + rnorm(200, 0, 2) # Linear predictor
p = invlogit(eta) # Transform into probabilities using inverse of logit 
y = rbinom(200, 1, p) # Binarizing by sampling from the binomial distribution

par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)
par(mfrow=c(1,1))

# Fitting a glm to y ~ x
#   use glm(), specify family and link-function
m = glm(y~x, family=binomial(link="logit")) 
summary(m)
# parameter estimates on link-scale (here logit)
#   backtransform to probability scale interpret biologically and present in graphs


### Exercise: replicate plot ----
# Plotting the regression line, transformed to probability 

coefs = summary(m)$coef # coefficients from model

x_pred = seq(from=min(x), to=max(x), by=0.01) # x values within data range 
y_hat = coefs[1,1] + coefs[2,1]*x_pred # regression line from model parameter estimates 
p_hat = invlogit(y_hat) # transformed to probability 

# compute x corresponding to probability of 0.5

x05 <- -coefs[1,1]/coefs[2,1]

plot(x, y, las=1)
lines(x_pred, p_hat)
abline(h = 0.5, lty=2)
abline(v = x05, lty=2)


### no r2 ----

# The GLM summary table does not provide an $r^2$ value, 
# because the normal $r^2$ does not work for logistic regression. 
# There are however several 'Pseudo-$r^2$' available, 
# typically based on comparing the likelihood of the model to that of a null model 
#   (a similar model but with only an intercept). 
# The `MuMIn` package provides one such measure.

library(MuMIn)
r.squaredGLMM(m)

# Coefficient of discrimination, tjurs D 
# how well the model discriminates between true positives and negatives in the data

# (replace y_hat and p_hat with to use x instead of x_pred)
y_hat = coefs[1,1] + coefs[2,1]*x # y values predicted from model
p_hat = invlogit(y_hat) # transformed to probability

mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)]) # coefficient of discrimination

### Final notes on fitting binomial GLMs in R ----

# Some final notes on fitting binomial GLM's. There are three ways to formulate these models in R. In the example above, the data were 0's and 1's, and we could specify the model simply as 

# `glm(y ~ x, family=binomial(link="logit"))`

# When each observation is based on more than one trial, we can formulate the model in two ways. The first is 

# `glm(y ~ x, family=binomial(link="logit"), weights=n)` 

# where `y` is the proportion of successes, and `n` is the number of trials. The second method is to fit a two-column matrix as response variable, where the first colomn is the number of successes, and the second column is the number of failures, i.e. `y = cbind(successes, failures)`.  The model formula is then

# `glm(cbind(successes, failures) ~ x, family=binomial(link="logit"))` 













