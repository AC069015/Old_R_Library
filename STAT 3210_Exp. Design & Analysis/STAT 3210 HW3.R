# Problem 1 #

cement <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0311.txt", header=TRUE)
summary(aov(Strength ~ factor(Technique), data=cement))

par(mfrow=c(1,2))
boxplot(Strength ~ Technique, cement)
plot(Strength ~ Technique, cement)

a=4
n=4
dfError=a*(n-1)
msError=12826
tcritical=qt(0.025, dfError, lower.tail=FALSE)
SE=sqrt(2*msError/n)
fisherLSD=tcritical*SE
fisherLSD
library(emmeans)
cementLM <- lm(Strength ~ factor(Technique), data=cement)
lsmeans(cementLM, pairwise ~ Technique, adjust="none")

qqnorm(residuals(cementLM))
plot(fitted(cementLM), residuals(cementLM))

bartlett.test(Strength ~ factor(Technique), data=cement)

# Problem 2 #

lsmeans(cementLM, pairwise ~ Technique)

pairwise.t.test(cement$Strength, cement$Technique, p.adjust.method="bonferroni")

# Problem 3 #

batteryLife <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0330.txt", header=TRUE)

anova(lm(Life ~ factor(Brand), data=batteryLife))

batteryLifeLM=lm(Life ~ factor(Brand), data=batteryLife)
qqnorm(residuals(batteryLifeLM))
plot(fitted(batteryLifeLM), residuals(batteryLifeLM))

library(car)
linearHypothesis(batteryLifeLM, c(0.5, 0.5, -1), rhs=0)
linearHypothesis(batteryLifeLM, c(1, -0.5, -0.5), rhs=0)

a=3
n=5
dfError=a*(n-1)
msError=15.6
tcritical=qt(0.025, dfError, lower.tail=FALSE)
SE=sqrt(2*msError/n)
fisherLSD=tcritical*SE
fisherLSD 

qcritical=qtukey(0.05, a, dfError, lower.tail=FALSE)
tukeyHSD=qcritical*SE
tukeyHSD

fcritical=qf(0.05, a-1, dfError, lower.tail=FALSE)
scheffeMSD=fcritical*SE
scheffeMSD

library(multcomp)
brand <- as.factor(batteryLife$Brand)
batteryLife.aov <- aov(Life ~ brand, batteryLife)
batteryLife.Dunnett <- glht(batteryLife.aov, linfct=mcp(brand="Dunnett"))
summary(batteryLife.Dunnett)