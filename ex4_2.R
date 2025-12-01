# ex4_2 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 4_2 ----
# - ASD: anther-stigma distance ($mm$)    self-pollination

# - GAD: gland-anther distance ($mm$)     flower to pollinator fit
# - GSD: gland-stigma distance ($mm$)

# - LBL: lower bract length ($mm$)        advertisement
# - LBW: lower bract width ($mm$)
# - UBL: upper bract length ($mm$)
# - UBW: upper bract width ($mm$)

# - GW: gland width ($mm$)                reward
# - GA: gland area ($mm^2$)

blossoms = read.csv("blossoms.csv")
# blossoms <- na.omit(blossoms)
names(blossoms)
unique(blossoms$pop) # 9
unique(blossoms$patch) # 26

tapply(blossoms$UBW, blossoms$pop, mean, na.rm=T) # a way te perform mean of UBW on seperate pops

library(tidyverse)
blossoms <- as.tibble(blossoms)

# summary of means and SDs of the pops 
sumblossoms <- blossoms %>% 
  group_by(pop)  %>% 
  summarize(ASDm = mean(ASD, na.rm=T),
            ASDsd = sd(ASD, na.rm=T),
            GADm = mean(GAD, na.rm=T),
            GADsd = sd(GAD, na.rm=T),
            GSDm = mean(GSD, na.rm=T),
            GSDsd = sd(GSD, na.rm=T),
            LBLm = mean(LBL, na.rm=T),
            LBLsd = sd(LBL, na.rm=T),
            LBWm = mean(LBW, na.rm=T),
            LBWsd = sd(LBW, na.rm=T),
            UBLm = mean(UBL, na.rm=T),
            UBLsd = sd(UBL, na.rm=T),
            UBWm = mean(UBW, na.rm=T),
            UBWsd = sd(UBW, na.rm=T),
            GWm = mean(GW, na.rm=T),
            GWsd = sd(GW, na.rm=T),
            GAm = mean(GA, na.rm=T),
            GAsd = sd(GA, na.rm=T),
            n = n())


# Look for stuff   
pairs(blossoms[,3:11], panel=panel.smooth)

# Trying stuff out

plot(blossoms$LBW, blossoms$UBW, xlim = c(10,30), ylim = c(0,30))
points(blossoms$LBW, blossoms$GSD)

plot(log(blossoms$LBW), log(blossoms$UBW), ylim = c(1,4))
points(log(blossoms$LBW), log(blossoms$GSD))

mm <- lm(UBW ~ LBW, data = blossoms)
hist(resid(mm))
summary(mm)

m <- lm(GSD ~ LBW, data = blossoms)
hist(resid(m))
summary(m)


# log-transform 
# because the measurements can then be compared better ( and possible because no 0s)

mlog <- lm(log(GSD) ~ log(LBW), data = blossoms)
hist(resid(mlog))
summary(mlog)

# GSD not affected by LBW
# differ between pops?

m1 <- lm(log(GSD) ~ log(LBW)*pop, data = blossoms) # interaction check
anova(m1) # very low r2 for interaction, slopes similar
m2 <- lm(log(GSD) ~ log(LBW)+pop, data = blossoms) # intercept check
anova(m2) # r2 for pop (intercept difference) is relatively high 
# different mean GSD between populations
summary(m2)
# general slope, increase 0.47... 4.7% in GSD with 10% increase in LBW
# means differ among populations 

popmeans <- tapply(log(blossoms$GSD), blossoms$pop, mean, na.rm=T)
cv <- sd(popmeans)*100
# the coefficient of variation (SD but comparable) is 4.4% 




## testing

ggplot(blossoms, aes(x = UBW, y = GW, color = pop)) +
  geom_point()
ggplot(blossoms, aes(x = log(UBW), y = log(GW), color = pop)) +
  geom_point()

g <- lm(log(GW) ~ log(UBW), data = blossoms)
anova(g)
summary(g)

g1 <- lm(log(GW) ~ log(UBW)*pop, data = blossoms)
anova(g1)

g2 <- lm(log(GW) ~ log(UBW)+pop, data = blossoms)
anova(g2)
summary(g2)



