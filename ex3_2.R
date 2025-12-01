# ex3_2 ----
rm(list = ls())
setwd("~/Master Biology/BIOS1315/BIOS15")
getwd()
list.files()

# Exercise 3

# y ~ factor1 * factor2
# AdultWeight ~ LarvalHost * MaternalHost

dat = read.csv("butterflies.csv")
names(dat)

# Check normality 
boxplot(AdultWeight ~ LarvalHost * MaternalHost, data = dat)

# Summary statistics of mean adult weight of the larvae for each treatment combination
means = tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)
means

m <- lm(AdultWeight ~ LarvalHost * MaternalHost, data = dat) # fit linear model 
anova(m) # perform anova
am <- anova(m)

# Proportion of variance gotten from sum of squares
am$`Sum Sq`[1]/sum(am$`Sum Sq`) # Larval host
am$`Sum Sq`[2]/sum(am$`Sum Sq`) # Maternal host
am$`Sum Sq`[3]/sum(am$`Sum Sq`) # Interaction 

# get summary
summary(m)


# Suppress intercept by adding -1 to predictor factor
# This worked when i just included the interaction term 
ms <- lm(AdultWeight ~ LarvalHost:MaternalHost -1, data = dat)
summary(ms)$coef 

confint(ms) # gives the 95% CI of the estimated means 


# For plot 
sumdat <- as_tibble(dat) %>%
  group_by(LarvalHost, MaternalHost) %>% 
  summarise(
    mean_AdultWeight = mean(AdultWeight),
    sd_AdultWeight = sd(AdultWeight),
    n = n(),
    se_AdultWeight = sd_AdultWeight/sqrt(n)
  )

ggplot(data = sumdat,
       aes(x = LarvalHost, y = mean_AdultWeight, colour = MaternalHost, group = MaternalHost)
       ) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  geom_errorbar(position = position_dodge(width = 0.2), 
                aes(ymin = mean_AdultWeight - se_AdultWeight,
                    ymax = mean_AdultWeight + se_AdultWeight),
                width = 0.1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Larval host",
       y = "Adult weight (mg)",
       colour = " Maternal host") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.8, 0.8),
        panel.grid = element_blank())

