# ex5_3 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 5_3 ----

## Looking at data ----

dat <- read.csv("Eulaema.csv")
head(dat)
names(dat)
names(dat)[11] <- "forest"

hist(dat$Eulaema_nigrita) # Data very skewed 

pairs(dat[,c(1, 5:12)], panel=panel.smooth)
# makes me want to test land use heterogeneity and forest as predictors, and control for effort 

plot(dat$forest, dat$Eulaema_nigrita)
plot(dat$effort, dat$Eulaema_nigrita)
plot(dat$lu_het, dat$Eulaema_nigrita)

# Just to see (control for??)
groups <- as.factor(dat$method)
plot(groups, dat$Eulaema_nigrita)  


## testing forest ----

m <- glm(Eulaema_nigrita ~ forest, family = "poisson", data = dat)
summary(m)
# this is very overdispersed based on residual deviance approx 100+ times as big as resid dfs
par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))
# qqplot: points not on line

n <- glm.nb(Eulaema_nigrita ~ forest, data = dat)
summary(n)
# resid deviance and dfs more similar
par(mfrow=c(2,2))
plot(n)
par(mfrow=c(1,1))
# qqplot better

# Assess fit of the model 
# McFaddens 
1-(n$deviance/n$null.deviance) # 0.11 is kind of bad?  
# Another pseudo r2 option
r.squaredGLMM(n) # delta 0.13



# Plot the predicted values 

plot(dat$forest, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16,
     xlab = "Forest cover (proportion)", ylab = "E. nigrita abundance")
xx <- seq(min(dat$forest), max(dat$forest), 0.01)
y_hat = predict(n, newdata=list(forest=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)





## testing effort ----

m <- glm(Eulaema_nigrita ~ effort, family = "poisson", data = dat)
summary(m)
# this is very overdispersed 

n <- glm.nb(Eulaema_nigrita ~ effort, data = dat)
summary(n)

# Assess fit of the model 
# McFaddens 
1-(n$deviance/n$null.deviance) # 0.16 is kind of good  
# Another pseudo r2 option
r.squaredGLMM(n) # also very low 



# Plot the predicted values 

plot(dat$effort, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16,
     xlab = "Effort log hours of sampling", ylab = "E. nigrita abundance")
xx <- seq(min(dat$effort), max(dat$effort), 0.01)
y_hat = predict(n, newdata=list(effort=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)




## testing land use heterogeneity ----

# m <- glm(Eulaema_nigrita ~ effort + altitude + MAT, family = "poisson", data = dat)

m <- glm(Eulaema_nigrita ~ lu_het, family = "poisson", data = dat)
summary(m)
# this is very overdispersed 

n <- glm.nb(Eulaema_nigrita ~ lu_het, data = dat)
summary(n)

# Assess fit of the model 
# McFaddens 
1-(n$deviance/n$null.deviance) # 0.012 Very low, poor fit  
# Another pseudo r2 option
r.squaredGLMM(n) # also very low 



# Plot the predicted values 

plot(dat$lu_het, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16,
     xlab = "Land use heterogeneity (Shannon diversity index)", ylab = "E. nigrita abundance")
xx <- seq(min(dat$lu_het), max(dat$lu_het), 0.01)
y_hat = predict(n, newdata=list(lu_het=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)





## testing altitude ----

# m <- glm(Eulaema_nigrita ~ effort + altitude + MAT, family = "poisson", data = dat)

m <- glm(Eulaema_nigrita ~ altitude, family = "poisson", data = dat)
summary(m)
# this is very overdispersed 

n <- glm.nb(Eulaema_nigrita ~ altitude, data = dat)
summary(n)

# Assess fit of the model 
# McFaddens 
1-(n$deviance/n$null.deviance) # 0.002 Very low, poor fit  
# Another pseudo r2 option
r.squaredGLMM(n) # also very low 



# Plot the predicted values 

plot(dat$altitude, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16,
     xlab = "Altitude (meters)", ylab = "E. nigrita abundance")
xx <- seq(min(dat$altitude), max(dat$altitude), 0.01)
y_hat = predict(n, newdata=list(altitude=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)




## testing effort and land use heterogeneity ----

m <- glm(Eulaema_nigrita ~ effort + lu_het, family = "poisson", data = dat)
summary(m)
# this is very overdispersed 

n <- glm.nb(Eulaema_nigrita ~ effort + lu_het, data = dat)
summary(n)

# Assess fit of the model 
# McFaddens 
1-(n$deviance/n$null.deviance) # 0.016 kinda low 
# Another pseudo r2 option
r.squaredGLMM(n) # similar



# Plot the predicted values 

plot(dat$lu_het, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16,
     xlab = "Land use heterogeneity (Shannon diversity index)", ylab = "E. nigrita abundance")
xx <- seq(min(dat$lu_het), max(dat$lu_het), 0.01)
y_hat = predict(n, newdata=list(lu_het=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)


# test forest cover
# include effort as control
# mean-center effort, include se.fit=T in predict, add 1.96SE=95%CI of effort in plot, 
