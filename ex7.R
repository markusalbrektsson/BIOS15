# ex7 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 7 ----


## Model selection in confirmatory vs. exploratory analyses ----

## Information criteria ----

## Example ----

set.seed(12)
x1 = rnorm(200, 10, 3)
group = as.factor(sample(c("A", "B"), 200, replace=T))
y = 0.5*x1 + rnorm(200, 0, 4)
y[group=="A"] = y[group=="A"] + rnorm(length(y[group=="A"]), 2, 1)

m1 = lm(y ~ x1 * group)
m2 = lm(y ~ x1 + group)
m3 = lm(y ~ x1)
m4 = lm(y ~ group)
m5 = lm(y ~ 1)

mlist = list(m1, m2, m3, m4, m5)
AICTab = AIC(m1, m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab





# Exercise ----
rm(list = ls())

# vilken "reklam" förklarar bäst belöningen 
# ga ~ UBL UBW LBL LBW 

# Get data 
blossoms = read.csv("blossoms.csv")
blossoms <- na.omit(blossoms)
names(blossoms)
unique(blossoms$pop) # 9 interesting, fixed factor, scratch that use both as random factor today 
unique(blossoms$patch) # 26 not interesting, random factor 
blossoms$pop <- as.factor(blossoms$pop)
blossoms$patch <- as.factor(blossoms$patch)


library(glmmTMB)

### random effects ---- 

m1 = glmmTMB(GA ~ UBW + (1|patch), data=blossoms)
m2 = glmmTMB(GA ~ UBW + (1|pop), data=blossoms)
m3 = glmmTMB(GA ~ UBW + (1|pop/patch), data=blossoms) # lowest AIC, by alot, include both 
m4 = glmmTMB(GA ~ UBW, data=blossoms)

mlist = list(m1, m2, m3, m4)
AICTab = AIC(m1, m2, m3, m4)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#    df      AIC    logLik delta w
# m3  5 1153.764 -571.8821  0.00 1
# m2  4 1168.908 -580.4538 15.14 0
# m1  4 1205.781 -598.8907 52.02 0
# m4  3 1224.124 -609.0620 70.36 0


### fixed effects ---- 

# UBL UBW 

f1 = glmmTMB(GA ~ UBW + (1|pop/patch), data=blossoms)
f2 = glmmTMB(GA ~ UBL + (1|pop/patch), data=blossoms)
f3 = glmmTMB(GA ~ UBW + UBL + (1|pop/patch), data=blossoms)
f4 = glmmTMB(GA ~ UBW * UBL + (1|pop/patch), data=blossoms) # lowest AIC 
f5 = glmmTMB(GA ~ 1 + (1|pop/patch), data=blossoms)

flist = list(f1, f2, f3, f4, f5)
AICTabf = AIC(f1, f2, f3, f4, f5)
AICTabf$logLik = unlist(lapply(flist, logLik))
AICTabf = AICTabf[order(AICTabf$AIC, decreasing=F),]
AICTabf$delta = round(AICTabf$AIC - min(AICTabf$AIC), 2)
lhf = exp(-0.5*AICTabf$delta)
AICTabf$w = round(lhf/sum(lhf), 2)
AICTabf

#    df      AIC    logLik delta    w
# f4  7 1143.887 -564.9433  0.00 0.96
# f3  6 1150.475 -569.2377  6.59 0.04
# f1  5 1153.764 -571.8821  9.88 0.01
# f2  5 1185.802 -587.9009 41.92 0.00
# f5  4 1220.747 -606.3734 76.86 0.00



# Maybe too few datapoint 

## Exercise try 2 ----
rm(list = ls())

read.csv()

