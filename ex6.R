# ex6 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 6 ----

## Variance component analysis using random-effect models ----

# estimate variance at different levels: variation in variable due to diff among pops
#                                        variation in variable due to diff among inds within pops

set.seed(145)
x1 = rnorm(200, 10, 2) 

groupmeans = rep(rnorm(10, 20, 4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))

y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)

plot(x1, y, col=as.numeric(groupID), las=1)


# Variance component analysis: estimating variance in intercepts among groups

library(glmmTMB)

data = data.frame(y, x1, groupID)
head(data)

m = glmmTMB(y ~ 1 + (1|groupID), data=data)

summary(m)

# Extract variances 
# In-depth look at how the model assigns variance explained by among groups
VarCorr(m)

VarAmongGroups = attr(VarCorr(m)$cond$groupID, "stddev")^2 # Among group variance from the model 
VarWithinGroups = attr(VarCorr(m)$cond, "sc")^2 # Within group variance from the model 

VarAmongGroups # model 
var(groupmeans) # actial variance of group means 
# models is smaller, it takes into account the variation within groups 

mean_sampling_variance = mean(tapply(y, groupID, var)/20) 
var(groupmeans) - mean_sampling_variance
# subtracted the average sampling variance of each group mean
# then the actual becomes similar to the models

# Percentage of variance explained by groups 
VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100


# Squared CV: CV2
# To interpret the actual variances. Squaring mean as variance is squared and thus get a unitless number 

CV2_Among = VarAmongGroups/mean(y)^2
CV2_Within = VarWithinGroups/mean(y)^2
CV2_Total = CV2_Among + CV2_Within

# Put stuff in a table
df = data.frame(Mean = mean(x1), SD = sd(x1), 
                Among = VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100,
                Within = VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100,
                CV2_Among, CV2_Within, CV2_Total)
df = apply(df, MARGIN=2, FUN=round, digits=2)
df
# It is about the Intercept!

## Data exercise: Variance partitioning with random-effects models. ----
# Pick any of the datasets we have worked with in the course that includes at least one grouping variable, 
# and perform a random-effect variance partitioning. 
# Produce a neat table and interpret the results biologically and statistically.
rm(list=ls())

dat <- read.csv("blossoms.csv")

head(dat)


plot(dat$GW, dat$GA, col=as.numeric(as.factor(dat$patch)), las=1)

m = glmmTMB(GA ~ 1 + (1|patch), data=dat)
summary(m)

VarAmongpatch <- attr(VarCorr(m)$cond$patch, "stddev")^2
VarWithinpatch <- attr(VarCorr(m)$cond, "sc")^2

CV2_Among = VarAmongpatch/mean(dat$GA)^2
CV2_Within = VarWithinpatch/mean(dat$GA)^2
CV2_Total = CV2_Among + CV2_Within

# Put stuff in a table
df = data.frame(Mean = mean(dat$GA), SD = sd(dat$GA), 
                PercentAmong = VarAmongpatch/(VarAmongpatch+VarWithinpatch)*100,
                PercentWithin = VarWithinpatch/(VarAmongpatch+VarWithinpatch)*100,
                CV2_Among, CV2_Within, CV2_Total)
df = apply(df, MARGIN=2, FUN=round, digits=2)
df

# the patch of the flowers explains 20% of the variation in gland area 
# so CV or CV2 gives a measure of absolute variance 
# the point is to give proportion of variance and a measure of actual/absolute variance 


## Random-intercept regression ----

# Now we include predictor, as that is the relationship we are interested in
# And the random factor groupID to account for it 
m = glmmTMB(y ~ x1 + (1|groupID), data=data)

summary(m)

# doesnt work ??
# par(mfrow=c(2,2))
# plot(m)
# par(mfrow=c(1,1))

coef(m) # gives estimated intercepts for each level of the group factor
# These estimates are called *best linear unbiased predictors* (BLUPs)

# plot values and lines from model, group intercepts and same slope
newx = seq(min(x1), max(x1), length.out=200)

plot(x1, y, las=1)
for(i in 1:length(levels(groupID))){
  y_hat = coef(m)$cond$groupID[i,1] + coef(m)$cond$groupID[i,2]*newx
  lines(newx, y_hat, col=i)
}

# Can use predict() to get predicted data of a specified group (level of the random factor)
y_hat = predict(m, newdata=list(x1=newx, groupID=rep("Group1",200)), re.form=NULL)
lines(newx, y_hat, type = "l", lwd = 3)


## Fit a naive linear model to the same data ----
# naive = dont know about the random factor

m <- lm(y ~ x1 , data=data)
summary(m)

# slope lower, less accurate, also larger SE, double  

par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))

## Data exercise: random-intercept models ----
# Pick any of the datasets we have worked with in the course that includes 
# at least one grouping variable, and perform a random-intercept analysis (regression, ANCOVA or ANOVA). 
# Produce relevant summary statistics and interpret the results biologically and statistically.
rm(list=ls())

dat <- read.csv("blossoms.csv")
head(dat)

plot(dat$GW, dat$GA, col=as.numeric(as.factor(dat$patch)), las=1)

m = glmmTMB(GA ~ GW + (1|patch), data=dat)
summary(m)

coef(m)

newx = seq(min(dat$GW), max(dat$GW), length.out=200)

plot(dat$GW, dat$GA, las=1)
for(i in 1:length(unique(dat$patch))){
  y_hat = coef(m)$cond$patch[i,1] + coef(m)$cond$patch[i,2]*newx
  lines(newx, y_hat, col=i)
}

# the intercepts do not differ much but, but the variance explained by patch is about 20% 
# much less variance explained by patch and as much is explained by fixed factor GW

## Nested and crossed random effects ----
rm(list=ls())

dat = read.csv("blossoms.csv")
names(dat)

dat$pop = as.factor(dat$pop)
dat$patch = as.factor(paste(dat$pop, dat$patch, "_"))

m = glmmTMB(UBW ~ 1 + (1|pop/patch), data=dat)
summary(m)
#   Variance    
# patch:pop - among patches in pops
# pop       - among pops
# Residual  - within patches

# including a fixed factor

m = glmmTMB(UBW ~ LBW + (1|pop/patch), data=dat)
summary(m)
# much less variance explained by population and patch because much is explained by the fixed factor 


## Generalized linear mixed models ----

# combining glm and lmm into glmm
# here using the bee data 
rm(list=ls())

dat = read.csv("Eulaema.csv")
dat$SA = as.factor(dat$SA)

m = glmmTMB(Eulaema_nigrita ~ MAP + (1|SA), family="nbinom2", data=dat)
summary(m)


# Plotting with individual intercept for study area (SA)

newMAP = seq(min(dat$MAP), max(dat$MAP), length.out=200)

plot(dat$MAP, dat$Eulaema_nigrita, las=1)

for(i in 1:length(levels(dat$SA))){
  y_hat = exp(coef(m)$cond$SA[i,1] + coef(m)$cond$SA[i,2]*newMAP)
  lines(newMAP, y_hat, col="grey")
}

# Adding a line for the whole model 

y_hat = exp(summary(m)$coef$cond[1,1] + summary(m)$coef$cond[2,1]*newMAP)
lines(newMAP, y_hat, lwd=2)







