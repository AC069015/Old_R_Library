# Problem 1 #

assemblyTime <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0428.txt", header=TRUE)

summary(aov(Time ~ factor(Order) + factor (Operator) + Method, assemblyTime))
summary(aov(Time ~ factor(Order) + Method, assemblyTime))
summary(aov(Time ~ factor (Operator) + Method, assemblyTime))
summary(aov(Time ~ Method, assemblyTime))

par(mfrow = c(2,2))
plot(aov(Time ~ factor(Operator) + factor(Order) + Method, data=assemblyTime))

assemblyTimeLM = lm(Time ~ factor(Order) + factor (Operator) + Method, assemblyTime)
nonadd = predict(assemblyTimeLM)^2
anova(update(assemblyTimeLM, . ~ . + nonadd))

emmeans(assemblyTimeLM, pairwise ~ Method, adjust="none")

critValue = sqrt(0.5)*qtukey(0.95,4,6)
SE = sqrt((2*1.75)/4)
(HSD = critValue*SE)
emmeans(assemblyTimeLM, pairwise ~ Method)

assemblyTime.lmer = lmer(Time ~ (1|Operator) + (1|Order)+ Method, data=assemblyTime)
summary(assemblyTime.lmer)
emmeans(assemblyTime.lmer, pairwise~Method, adjust="none")
emmeans(assemblyTime.lmer, pairwise~Method)

assemblyTimeMissing <- assemblyTime
assemblyTimeMissing ["1", "Time"] <- NA
assemblyTimeMissing ["14", "Time"] <- NA
summary(aov(Time ~ factor(Operator) + factor(Order) + Method, assemblyTimeMissing))

# Problem 2 #

assemblyTimeGLS <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0441.txt", header=TRUE)
summary(aov(Time ~ factor(Order) + factor (Operator) + Workplace + Method, assemblyTimeGLS))


# Problem 3 #

reactionTime <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0427.txt", header=TRUE)
summary(aov(Time ~ factor(Batch) + factor(Day) + Catalyst, reactionTime))
