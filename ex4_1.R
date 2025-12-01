# ex4_1 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 4_1 ----
plants = read.csv(file="alpineplants.csv")
plants <- na.omit(plants)

str(plants)
head(plants)
plot(plants$altitude, plants$Carex.bigelowii)
plot(plants$light, plants$Carex.bigelowii)
plot(plants$snow, plants$Carex.bigelowii)
plot(plants$soil_moist, plants$Carex.bigelowii)
plot(plants$mean_T_summer, plants$Carex.bigelowii)
plot(plants$mean_T_winter, plants$Carex.bigelowii)

plot(plants$min_T_winter, plants$Carex.bigelowii)
plot(plants$max_T_winter, plants$Carex.bigelowii)

plot(plants$altitude, plants$Thalictrum.alpinum)
plot(plants$light, plants$Thalictrum.alpinum)
plot(plants$snow, plants$Thalictrum.alpinum)
plot(plants$soil_moist, plants$Thalictrum.alpinum)
plot(plants$mean_T_summer, plants$Thalictrum.alpinum)
plot(plants$mean_T_winter, plants$Thalictrum.alpinum)

plot(plants$min_T_winter, plants$Thalictrum.alpinum)
plot(plants$max_T_winter, plants$Thalictrum.alpinum)

pairs(plants[,3:12], panel=panel.smooth)

# plants <- plants[-which.max(plants$max_T_winter),]
# removed outlier of max t winter 

# how do i see what seems relevant through looking at plots? 
# i guess you don't, you have to get to know the biology to make informative choices 

m <- lm(Thalictrum.alpinum ~ mean_T_summer + mean_T_winter, data = plants, na = na.exclude)
hist(residuals(m))

# sqrt-transform
m <- lm(sqrt(Thalictrum.alpinum) ~ mean_T_summer + mean_T_winter, data = plants, na = na.exclude)
hist(residuals(m))

summary(m)

# r-square: 46% of variance explained by mean
var(sqrt(plants$Thalictrum.alpinum))

# var expl by each 
coef <- summary(m)$coef
coef[2,1]^2*var(plants$mean_T_summer)
(coef[2,1]^2*var(plants$mean_T_summer))/var(sqrt(plants$Thalictrum.alpinum))

coef[3,1]^2*var(plants$mean_T_winter)
(coef[3,1]^2*var(plants$mean_T_winter))/var(sqrt(plants$Thalictrum.alpinum))

t(coef[2:3,1]) %*% cov(cbind(plants$mean_T_summer, plants$mean_T_winter)) %*% coef[2:3,1]
(t(coef[2:3,1]) %*% cov(cbind(plants$mean_T_summer, plants$mean_T_winter)) %*% coef[2:3,1])/var(sqrt(plants$Thalictrum.alpinum))

# reverse it 
m <- lm(sqrt(Thalictrum.alpinum) ~ mean_T_summer + mean_T_winter + snow, data = plants)
summary(m)

# multicollinearity 

m1 <- lm(mean_T_summer ~ mean_T_winter, data = plants)
r2 <- summary(m1)$r.squared
1/(1-r2)
# VIF = 1.2 , very low, predictors not strongly correlated

m1 <- lm(mean_T_winter ~ snow, data = plants)
r2 <- summary(m1)$r.squared
1/(1-r2)
# VIF = 2.9 , almost 3 wich is the limit (rule of thumb) , predictors maybe somewhat correlated



