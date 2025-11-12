# ex2 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()
## Instructions ----

# Making a simulated (fake) dataset out using a function and added residual noise
set.seed(85)
x = rnorm(n=200, mean=10, sd=2) # Makes a vector of 200 "leaf length"- values from a normal distribution with mean 10 and sd 2
y = 0.4*x + rnorm(200, 0, 1) # Makes the leaf width 0.4 times the length plus noise 
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

# Regression analysis / fitting linear regression model (using ordinary least-squares(OLS)?)
m = lm(y~x) 

# Components (parameter estimates)
str(m)
cf = m$coefficients
cf

# Compute predicted values from parameter estimates and model equation 
predvals=cf[1]+ cf[2]*x # Computes
par(mfrow=c(1,2)) # Change plot windows
plot(x,y,las=1,
     xlab="Leaflength(mm)",
     ylab="Leafwidth(mm)")
abline(m) 
segments(x,y,x,predvals) # Draws line segments between from x, y to x, predvals. So from real values to predicted values
hist(residuals(m),xlab="",las=1) # plots a histogram of the residuals of the model using the function residuals()
# Residuals look normally distributed


par(mfrow=c(2,2))
plot(m) # 4 ways of assessing residuals
par(mfrow=c(1,1))

# Same plot but with fitted regression line within data range 
newx= seq(min(x), max(x),length.out=200) 
predy=cf[1]+ cf[2]*newx
plot(x,y,las=1,
     xlab="Leaflength(mm)",
     ylab="Leafwidth(mm)")
lines(newx,predy)


# a look at the results of the function
summary(m)



# optional exercise 
# Use non-parametric bootstrapping to derive a standard error for the slope of the
# linear regression above

df = data.frame(x, y)
head(df)

dfs <- data.frame(x = numeric(200), y = numeric(200)) # empty dataframe
summary(lm(y~x, data = dfs))
slopeest <- NULL # empty vector 

for (i in 1:1000) {
  dfs[,1] <- sample(df[,1], replace = TRUE) # samples from df x
  dfs[,2] <- sample(df[,2], replace = TRUE) # samples from df x
  sm <- lm(dfs$y~dfs$x) # sample model 
  slopeest[i] <- sm$coefficients[2] # store slope in vector 
}
slopeSE <- sd(slopeest)
slopeSE

hist(slopeest)


# regression slope is given by:
cov(y,x)/var(x)

# diff in predicted y when x is its mean and plus one SD 
(cf[2]*(mean(x) + sd(x)))- (cf[2]*mean(x))
# why ignore intercept? 
# because it is a difference 

# r-square,  
# In our simple univariate regression, the r2 is simply the square of the 
# Pearson correlation coefficient r between the response and predictor
cor(x,y)^2

y_hat = cf[1] + cf[2]*x # predicted y values
var(y_hat) 

var(y_hat)/var(y) # variance of predicted y divided by variance of y is r-squared

# Another way to compute the variance explained by a predictor
cf[2]^2*var(x) # slope and variance of predictor 



## Exercise ----
rm(list = ls())

birds <- read.csv("bird_allometry.csv")
head(birds)

# Allometry
# y = ax^b, which can be linearized through the logarithmic transformation log(y) = log(a) + b ×log(x).

# test if brain body scaling is similar between sexes
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]

# fit linear regression to logtransorm
mm = lm(log(brain_mass)~log(body_mass), data=males)
mf = lm(log(brain_mass)~log(body_mass), data=females)

# check residuals
hist(residuals(mm))
hist(residuals(mf))

# look at fit
summary(mm) # looks good, slope SE is small far from 0
summary(mf) # similar

# get estimate coefficients
mx <- seq(min(log(males$body_mass)), max(log(males$body_mass)), length = 100)
fx <- seq(min(log(females$body_mass)), max(log(females$body_mass)), length = 100)
my <- mm$coefficients[1] + mm$coefficients[2]*mx
fy <- mf$coefficients[1] + mf$coefficients[2]*fx

plot(log(males$body_mass), log(males$brain_mass),
     xlab = "log body mass",
     ylab = "log brain mass",
     pch = 21,
     cex = 0.8,
     col = "black",
     bg = "lightblue")
points(log(females$body_mass), log(females$brain_mass),
       pch = 21,
       cex = 0.8,
       col = "black",
       bg = "pink")
lines(mx, my, lwd = 1.5, col = "blue")
lines(fx, fy, lwd = 1.5, col = "red")
legend("bottomright", pch=c(16,16), 
       col=c("lightblue", "pink"),
       legend=c("Males", "Females"))

head(birds)

birds[which.max(birds$body_mass),]
max(birds$body_mass)

# Optional exercise ----

# Simulate data 
bsd <- data.frame(NULL)
bsd[1] <- birds[[4]] + 0.1*sd(birds[[4]]) #CONTINUE

# Get slope estimate from original: fit linear regression, take coefficient
lr = lm(log(brain_mass)~log(body_mass), data=birds)
b = lr$coefficients[2]

