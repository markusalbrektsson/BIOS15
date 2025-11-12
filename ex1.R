# Exercise 1 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()
# Testing stuff from the text

log(1.1/1)
-log(1/1.1)

# Simulating dat from statistical distributions
x = rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)
hist(x, las = 1, main = "")

## Intro to exercice ----
# Bootstrapping
# To ensure reproducibility of the results 
# (i.e. that we will get the same result every time, even when we workon different computers), 
# we set the “seed” of the random number generator using the set.seed() function.

# As a first example, we will use bootstrapping to obtain the standard error of the mean
# a non-parametric bootstrap, where we resample the data many times.
# (non-parametric is directly from the observed data)

set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))


out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
}

# The variable out now contains what we can call the sampling distribution of the mean of x.
hist(out, las=1, main="")

sd(out) 
# The standard deviation of the sampling distribution gives an approximation of the standard error 
# (and this is very different from the standard deviation of the original data!)

se_x
# As expected, this is close to the theoretical standard error

quantile(out, c(0.025, 0.975))
#Given that we also have the full sampling distribution, we can also choose to derive some quantiles, 
# such as a 95% confidence interval.

qnorm(c(0.025, 0.975))
#  The quantiles of the standard normal distribution are available in R through the qnorm function. 

mean(x) - 1.96*se_x
mean(x) + 1.96*se_x

## Start of exercice ----
# EXERCISE: Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable.
# Start by writing a function that computes the CV for a variable (see the Appendix for a brief introduction
# to writing functions in R). Then, simulate a random variable and write a loop that samples many times from
# this variable and computes the CV.

rm(list = ls())
source("CV.R")

set.seed(1) # set seed to get same "random" stuff
a <- rnorm(50, 10, 2) # create a data set / variable
CV_a <- CV(a) # use function CV to get the coefficient of variance for the variable

# create an empty vector
b <- NULL 
# write a loop that samples from a (1000 times)
# and computes the CV
for (i in 1:1000) {
  asample <- sample(a, replace = TRUE)
  b[i] <- CV(asample)
}
# b is now a sampling distribution of the CV of a 
hist(b, las=1, main="")


# the 95% confidence interval for the CV of a 
quantile(b, c(0.025, 0.975))

## Optional exercise ----
# Use simulated data to show the close relationship between the SD of log-transformed data 
# and the CV on arithmetic scale.
rm(list = ls())
source("CV.R")

# simulate data with rnorm (vector)
d <- rnorm(100, 10, 2)

# make empty vectors for SD-log-transformed data and CV data 
sdlogd <- NULL
cvd <- NULL

# in a for loop (1:1000), sample the data and fill empty vectors 
# log-transform the data (vector)
# SD of the log-transformed data (value)
# CV the data (value) 
for (i in 1:1000) {
  dsample <- sample(d, replace = T)
  sdlogd[i] <- sd(log(dsample))
  cvd[i] <- CV(dsample)
}

# plot SD-log-transformed data and CV data
plot(cvd, sdlogd)
abline(0,1, col = "red")















