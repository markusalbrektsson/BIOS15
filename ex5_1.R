# ex5_1 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

# Exercise 5_1 ----

dat = read.csv("dormancy.csv")
names(dat)
# Biological effect on germ2 (proportion germinated) 
# I expect: timetosowing (duration allowed to ripen) will have a large effect, some optimum
#     this will probably interact with pop/mother/crossID which are similar 
#     blocktray may effect
#     MCseed will effect positively 
#     nseed, no idea

ggplot(data = dat, 
       aes(
         x = timetosowing,
         y = germ2*nseed,
         color = pop
       )) +
  geom_point()

## CC----
# Fitting the data of pop CC using two different methods
 
subdat = dat[dat$pop=="CC",]

germ = subdat$germ2 * subdat$nseed #Successes
notgerm = subdat$nseed - germ #Failures

mod1 = glm(cbind(germ, notgerm) ~ timetosowing, "binomial", data=subdat)
mod2 = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
logLik(mod1) == logLik(mod2) # TRUE so they are identical 
summary(mod2)
coefmod2 <- summary(mod2)$coef
nulldevmod2 <- summary(mod2)$null.deviance
devmod2 <- summary(mod2)$deviance

# Can you use the fitted models to estimate the duration of after-ripening 
# required for the expected germination rate to be 0.5?

# Find x where probability is 0.5
x05 <- -coefmod2[1,1]/coefmod2[2,1]
# = 107.2903

# for plotting
x_pred <- seq(min(subdat$timetosowing), max(subdat$timetosowing), by = 0.1) # x values within data range 
y_hat <- coefmod2[1,1] + coefmod2[2,1]*x_pred # regression line from model parameter estimates 
p_hat = invlogit(y_hat) # transformed to probability 

# plotting cc 
plot(subdat$timetosowing, subdat$germ2, las = 1, main = "CC")
lines(x_pred, p_hat)
abline(h = 0.5, lty=2)
abline(v = x05, lty=2) 

# coefficient of discrimination 
# does this work for data with 0.5 values which are ignored??? 

y_hatp <- coefmod2[1,1] + coefmod2[2,1]*subdat$timetosowing # predicted values from model parameter estimates 
p_hatp = invlogit(y_hatp) # transformed to probability 

dp <- mean(p_hatp[which(subdat$germ2==1)]) - mean(p_hatp[which(subdat$germ2==0)]) # 0.4621511

cc <- paste("CC\n", "intercept", "\n", coefmod2[1,1], "\n", "slope", "\n", coefmod2[2,1], "\n", 
    "null deviance", "\n", nulldevmod2, 
    "\n", "resid deviance", "\n", devmod2, "\n", "discriminatory power", "\n", dp, 
    "\n", "duration 50% germ rate", "\n", x05)

## LM----

subdat = dat[dat$pop=="LM",]

mod2 = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
summary(mod2)
coefmod2 <- summary(mod2)$coef
nulldevmod2 <- summary(mod2)$null.deviance
devmod2 <- summary(mod2)$deviance

# Can you use the fitted models to estimate the duration of after-ripening 
# required for the expected germination rate to be 0.5?

# Find x where probability is 0.5
x05 <- -coefmod2[1,1]/coefmod2[2,1]
# = 107.2903

# for plotting
x_pred <- seq(min(subdat$timetosowing), max(subdat$timetosowing), by = 0.1) # x values within data range 
y_hat <- coefmod2[1,1] + coefmod2[2,1]*x_pred # regression line from model parameter estimates 
p_hat = invlogit(y_hat) # transformed to probability 

# plotting cc 
plot(subdat$timetosowing, subdat$germ2, las = 1, main = "LM")
lines(x_pred, p_hat)
abline(h = 0.5, lty=2)
abline(v = x05, lty=2) 

# coefficient of discrimination 
# does this work for data with 0.5 values which are ignored??? 

y_hatp <- coefmod2[1,1] + coefmod2[2,1]*subdat$timetosowing # predicted values from model parameter estimates 
p_hatp = invlogit(y_hatp) # transformed to probability 

dp <- mean(p_hatp[which(subdat$germ2==1)]) - mean(p_hatp[which(subdat$germ2==0)]) # 0.4621511

lm <- paste("LM\n", "intercept", "\n", coefmod2[1,1], "\n", "slope", "\n", coefmod2[2,1], "\n", 
    "null deviance", "\n", nulldevmod2, 
    "\n", "resid deviance", "\n", devmod2, "\n", "discriminatory power", "\n", dp, 
    "\n", "duration 50% germ rate", "\n", x05)


## PM----

subdat = dat[dat$pop=="PM",]

mod2 = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
summary(mod2)
coefmod2 <- summary(mod2)$coef
nulldevmod2 <- summary(mod2)$null.deviance
devmod2 <- summary(mod2)$deviance

# Can you use the fitted models to estimate the duration of after-ripening 
# required for the expected germination rate to be 0.5?

# Find x where probability is 0.5
x05 <- -coefmod2[1,1]/coefmod2[2,1]
# = 107.2903

# for plotting
x_pred <- seq(min(subdat$timetosowing), max(subdat$timetosowing), by = 0.1) # x values within data range 
y_hat <- coefmod2[1,1] + coefmod2[2,1]*x_pred # regression line from model parameter estimates 
p_hat = invlogit(y_hat) # transformed to probability 

# plotting cc 
plot(subdat$timetosowing, subdat$germ2, las = 1, main = "PM")
lines(x_pred, p_hat)
abline(h = 0.5, lty=2)
abline(v = x05, lty=2) 

# coefficient of discrimination 
# does this work for data with 0.5 values which are ignored??? 

y_hatp <- coefmod2[1,1] + coefmod2[2,1]*subdat$timetosowing # predicted values from model parameter estimates 
p_hatp = invlogit(y_hatp) # transformed to probability 

dp <- mean(p_hatp[which(subdat$germ2==1)]) - mean(p_hatp[which(subdat$germ2==0)]) # 0.4621511

pm <- paste("PM\n", "intercept", "\n", coefmod2[1,1], "\n", "slope", "\n", coefmod2[2,1], "\n", 
    "null deviance", "\n", nulldevmod2, 
    "\n", "resid deviance", "\n", devmod2, "\n", "discriminatory power", "\n", dp, 
    "\n", "duration 50% germ rate", "\n", x05)

## T----

subdat = dat[dat$pop=="T",]

mod2 = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
summary(mod2)
coefmod2 <- summary(mod2)$coef
nulldevmod2 <- summary(mod2)$null.deviance
devmod2 <- summary(mod2)$deviance

# Can you use the fitted models to estimate the duration of after-ripening 
# required for the expected germination rate to be 0.5?

# Find x where probability is 0.5
x05 <- -coefmod2[1,1]/coefmod2[2,1]
# = 107.2903

# for plotting
x_pred <- seq(min(subdat$timetosowing), max(subdat$timetosowing), by = 0.1) # x values within data range 
y_hat <- coefmod2[1,1] + coefmod2[2,1]*x_pred # regression line from model parameter estimates 
p_hat = invlogit(y_hat) # transformed to probability 

# plotting cc 
plot(subdat$timetosowing, subdat$germ2, las = 1, main = "T")
lines(x_pred, p_hat)
abline(h = 0.5, lty=2)
abline(v = x05, lty=2) 

# coefficient of discrimination 
# does this work for data with 0.5 values which are ignored??? 

y_hatp <- coefmod2[1,1] + coefmod2[2,1]*subdat$timetosowing # predicted values from model parameter estimates 
p_hatp = invlogit(y_hatp) # transformed to probability 

dp <- mean(p_hatp[which(subdat$germ2==1)]) - mean(p_hatp[which(subdat$germ2==0)]) # 0.4621511

t <- paste("T\n", "intercept", "\n", coefmod2[1,1], "\n", "slope", "\n", coefmod2[2,1], "\n", 
    "null deviance", "\n", nulldevmod2, 
    "\n", "resid deviance", "\n", devmod2, "\n", "discriminatory power", "\n", dp, 
    "\n", "duration 50% germ rate", "\n", x05)



## T other factors: MCseed / population mean centered seed mass ----


mod3 = glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=subdat)
summary(mod3)
coefmod3 <- summary(mod3)$coef
nulldevmod3 <- summary(mod3)$null.deviance
devmod3 <- summary(mod3)$deviance

# Can you use the fitted models to estimate the duration of after-ripening 
# required for the expected germination rate to be 0.5?

# Find x where probability is 0.5
x05 <- -coefmod3[1,1]/coefmod3[2,1]
x05_l <- -(coefmod3[1,1] + coefmod3[3,1]*sd(subdat$MCseed))/coefmod3[2,1]
x05_s <- -(coefmod3[1,1] - coefmod3[3,1]*sd(subdat$MCseed))/coefmod3[2,1]

# for plotting
x_pred <- seq(min(subdat$timetosowing), max(subdat$timetosowing), by = 0.1) # x values within data range 
y_hat1 <- coefmod3[1,1] + coefmod3[2,1]*x_pred # regression line from model parameter estimates 
y_hat2 <- coefmod3[1,1] + coefmod3[2,1]*x_pred + coefmod3[3,1]*sd(subdat$MCseed)
y_hat3 <- coefmod3[1,1] + coefmod3[2,1]*x_pred - coefmod3[3,1]*sd(subdat$MCseed)

# plotting cc 
plot(subdat$timetosowing, subdat$germ2, las = 1, main = "T + MCseed")
lines(x_pred, invlogit(y_hat1))
lines(x_pred, invlogit(y_hat2), lty = 2)
lines(x_pred, invlogit(y_hat3), lty = 2)
abline(h = 0.5, lty=1)
abline(v = x05, lty=1) 
abline(v = x05_l, lty=2)
abline(v = x05_s, lty=2)
# coefficient of discrimination 
# does this work for data with 0.5 values which are ignored??? 

y_hatp <- coefmod2[1,1] + coefmod2[2,1]*subdat$timetosowing # predicted values from model parameter estimates 
p_hatp = invlogit(y_hatp) # transformed to probability 

dp <- mean(p_hatp[which(subdat$germ2==1)]) - mean(p_hatp[which(subdat$germ2==0)]) # 0.4621511

tmcseed <- paste("T + MCseed\n", "intercept", "\n", coefmod3[1,1], "\n", "duration slope", "\n", coefmod3[2,1],
                 "\n", "MCseed slope", "\n", coefmod3[3,1], "\n", 
           "null deviance", "\n", nulldevmod3, 
           "\n", "resid deviance", "\n", devmod3, "\n", "discriminatory power", "\n", dp, 
           "\n", "duration 50% germ rate", "\n", x05, 
           "\n", "duration 50% germ rate, seed -1SD", "\n", x05_s, 
           "\n", "duration 50% germ rate, seed +1SD", "\n", x05_l)


cat(cc)
cat(lm)
cat(pm)
cat(t)
cat(tmcseed)












