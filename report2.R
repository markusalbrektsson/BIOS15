# report 2 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# GLM analysis ----

## Looking at data ----

dat <- read.csv("Eulaema.csv")
head(dat)
names(dat)
names(dat)[11] <- "forest"

hist(dat$Eulaema_nigrita) # Data very skewed 

pairs(dat[,c(1, 5:12)], panel=panel.smooth)
# interested in forest cover and want to control for effort 

# Response count data and continous covariates -> poisson GLM

# mean center effort, for later interpretation
dat$mceffort <- dat$effort - mean(dat$effort)




## Forest and effort poisson ----

m <- glm(Eulaema_nigrita ~ forest + mceffort, family = "poisson", data = dat)
summary(m)
# this is very overdispersed based on residual deviance almost 100 times as big as resid dfs

coefm <- summary(m)$coef
coefm
exp(coefm[1,1]) # intercept at 109 bees (0% forest, mean effort)
exp(coefm[2,1]) # slope is 0.25, decrease to 25% of bee intercept at forest 100%
exp(coefm[3,1]) # not as interesting (when forest 0%, bee count increase 1.3 with one unit effort (log) )

par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))
# qqplot: points not on line

# do a negative binomial instead to account for overdispersion




## Forest and effort negative binomial ----

nb <- glm.nb(Eulaema_nigrita ~ forest + mceffort, data = dat)
summary(nb)
# resid deviance and dfs more similar

coefnb <- summary(nb)$coef
coefnb
# parameter estimates and 95% CI 
exp(coefnb[1,1]) # intercept at 106 bees (0% forest, mean effort)
exp(coefnb[1,1] - 1.96*coefnb[1,2])
exp(coefnb[1,1] + 1.96*coefnb[1,2])
exp(coefnb[2,1]) # slope is 0.26, decrease to 26% of bee intercept at forest 100%
exp(coefnb[2,1] - 1.96*coefnb[2,2])
exp(coefnb[2,1] + 1.96*coefnb[2,2])
exp(coefnb[1,1] + coefnb[2,1]) # intercept at 27 bees (100% forest, mean effort)
exp(coefnb[3,1]) # not as interesting (when forest 0%, bee count increase 1.4 with one unit effort (log) )
exp(coefnb[3,1] - 1.96*coefnb[3,2])
exp(coefnb[3,1] + 1.96*coefnb[3,2])


par(mfrow=c(2,2))
plot(nb)
par(mfrow=c(1,1))
# qqplot looks better

# Assess fit of the model 
# McFaddens 
1-(nb$deviance/nb$null.deviance) # 0.23 is kind of good  (0.2 to 0.4 is "excellent")
# Another pseudo r2 option
r.squaredGLMM(nb) # delta 0.25
?r.squaredGLMM

# Plot the predicted values and CI 

plot(dat$forest, dat$Eulaema_nigrita, las=1, col=rgb(0,0,0,.3), pch=16,
     ylim = c(0,200),
     xlab = "Forest cover (proportion)", 
     ylab = expression(paste(italic("E. nigrita")," abundance")),
     main = "") 

predforest <- seq(min(dat$forest), max(dat$forest), 0.01)
predmceffort <- rep(mean(dat$mceffort), length(predforest))
y_hat = predict(nb, newdata=list(forest=predforest, 
                                 mceffort=predmceffort), type="link", se.fit=T)
lines(predforest, exp(y_hat$fit), lwd = 2)

predmceffort2 <- rep(mean(dat$mceffort) + sd(dat$mceffort), length(predforest))
y_hat2 = predict(nb, newdata=list(forest=predforest, 
                                 mceffort=predmceffort2), type="response")
lines(predforest, y_hat2, lty=2, lwd = 2, col="red")

predmceffort3 <- rep(mean(dat$mceffort) - sd(dat$mceffort), length(predforest))
y_hat3 = predict(nb, newdata=list(forest=predforest, 
                                  mceffort=predmceffort3), type="response")
lines(predforest, y_hat3, lty=2, lwd = 2, col="blue")


# 95% CI 

# lines(predforest, exp(y_hat$fit+1.96*y_hat$se.fit), lty=2, lwd = 2)
# lines(predforest, exp(y_hat$fit-1.96*y_hat$se.fit), lty=2, lwd = 2)

polygon(c(predforest,rev(predforest)),
        c(exp(y_hat$fit+1.96*y_hat$se.fit), rev(exp(y_hat$fit-1.96*y_hat$se.fit))),
        col = rgb(.4,1,.4,.5),
        border = FALSE)

# Legend

legend("topright", lty=c(1, 1, 2, 2), lwd=c(2,10,2,2), col=c(1, rgb(.4,1,.4,.5), "red", "blue"), 
       bty="n", 
       legend=c("Effort = Mean",
                "95% CI at mean effort",
                "Effort = Mean + SD",
                "Effort = Mean - SD"))


AIC(m) # 16899
AIC(nb) # 1840
