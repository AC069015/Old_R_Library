#PROBLEM 2#
soapTest <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0409.txt", header=TRUE)

summary(lm(Growth ~ factor(Days) + factor(Solution), data=soapTest))
summary(aov(Growth ~ factor(Days) + factor(Solution), data=soapTest))

soapTestlm <- lm(Growth ~ factor(Days) + factor(Solution), data=soapTest)
par(mfrow=c(2,2))
plot(Growth ~ factor(Days) + factor(Solution), data=soapTest)
qqnorm(residuals(soapTestlm), datax=TRUE)
plot(fitted(soapTestlm), residuals(soapTestlm))
nonadd=predict(soapTestlm)^2
anova(update(soapTestlm,Growth ~ factor(Days) + factor(Solution) + nonadd))
interaction.plot(factor(soapTest$Days), factor(soapTest$Solution), soapTest$Growth)

summary(aov(Growth ~ factor(Solution), data=soapTest))

library(emmeans)
emmeans(soapTestlm, pairwise ~ Solution)

library(lme4)
soapTestlmer=lmer(Growth ~ factor(Solution) + (1|Days), data=soapTest)
summary(soapTestlmer)
lsmeans(soapTestlmer, pairwise ~ Solution)

ybars = c(-3, 3, 0)
a=3
MSE=8.64
alpha=.05
power.anova.test(groups=a, n=NULL, between.var=var(ybars), within.var=MSE, sig.level=alpha, power=0.90)

soapTestCopy <- soapTest
soapTestCopy["9", "Growth"] <- NA
soapTestCopy
summary(aov(Growth ~ factor(Days) + factor(Solution), data=soapTestCopy))

#PROBLEM 3#
hardness <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0412.txt", header=TRUE)
hardness
summary(aov(Hardness ~ factor(Coupon) + factor(Tip), data=hardness))

hardnesslm <- lm(Hardness ~ factor(Coupon) + factor(Tip), data=hardness)
emmeans(hardnesslm, pairwise ~ Tip, adjust="none")

par(mfrow=c(2,2))
plot(Hardness ~ factor(Coupon) + factor(Tip), data=hardness)
qqnorm(residuals(hardnesslm), datax=TRUE)
plot(fitted(hardnesslm), residuals(hardnesslm))

nonadd=predict(hardnesslm)^2
anova(update(hardnesslm,Hardness ~ factor(Coupon) + factor(Tip) + nonadd))
interaction.plot(factor(hardness$Coupon), factor(hardness$Tip), hardness$Hardness)

hardnesslmer=lmer(Hardness ~ factor(Tip) + (1|Coupon), data=hardness)
summary(hardnesslmer)
lsmeans(hardnesslmer, pairwise ~ Tip, adjust="none")

hardnessCopy <- hardness
hardnessCopy["16", "Hardness"] <- NA
summary(aov(Hardness ~ factor(Coupon) + factor(Tip), data=hardnessCopy))
