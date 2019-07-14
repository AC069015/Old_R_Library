# Problem 1 #

surfaceFinish <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0509.txt", header=TRUE)

library(car)
surfaceFinishlm = lm(Surface ~ factor(Feed)*factor(Depth), surfaceFinish)
Anova(surfaceFinishlm, type=2)

par(mfrow=c(2,2))
plot(aov(surfaceFinishlm))

with(surfaceFinish, interaction.plot(Feed, Depth, Surface, type="b"))
with(surfaceFinish, interaction.plot(Depth, Feed, Surface, type="b"))

qt.val = qtukey(0.95, 3, 24)
SE.diff = sqrt((2*(689.33/24))/3)
( HSD = (qt.val/sqrt(2))*SE.diff )
TukeyHSD(aov(surfaceFinishlm))

qt.val = qtukey(0.95, 4, 24)
SE.diff = sqrt((2*(689.33/24))/3)
( HSD = (qt.val/sqrt(2))*SE.diff )

ybars = c(84.77778, 89.77778, 97.88889, 104.88889)
d = 4
f = 3
n = 3
MSE = 28.7
df.num = d - 1
df.den = d*f*(n - 1)
alpha = 0.05
( ncp = f*n*sum((ybars - mean(ybars))^2)/MSE )
( Fcritical = qf(alpha, df.num, df.den, lower.tail = FALSE) )
( power = 1 - pf(Fcritical, df.num, df.den, ncp) )

n = 5
D = 8
ncp = f*n*D^2/(2*MSE)
( power = 1 - pf(Fcritical, df.num, df.den, ncp) )

# Problem 2 #

brightness <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0511.txt", header=TRUE)

brightnessLM = lm(Current ~ factor(Glass) * factor(Phosphorous), brightness)
summary(aov(brightnessLM))

with(brightness, interaction.plot(Phosphorous, Glass, Current, type="b"))
with(brightness, interaction.plot(Glass, Phosphorous, Current, type="b"))

brightnessAdditive = lm(Current ~ factor(Glass) + factor(Phosphorous), brightness)
summary(aov(brightnessAdditive))

TukeyHSD(aov(brightnessAdditive))

# Question 3 #

tapeStrength <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0521.txt", header=TRUE)

tapeStrengthLM = lm(Strength ~ factor(Pressure) + factor(Temperature), tapeStrength)
summary(aov(tapeStrengthLM))

nonadd = predict(tapeStrengthLM)^2
anova(update(tapeStrengthLM, .~. + nonadd))

with(tapeStrength, interaction.plot(Pressure, Temperature, Strength, type="b"))
with(tapeStrength, interaction.plot(Temperature, Pressure, Strength, type="b"))

par(mfrow = c(2,2))
plot(aov(tapeStrengthLM, tapeStrength))

TukeyHSD(aov(tapeStrengthLM))
p = 3
dfe = 6
alpha = 0.05
MSE = 0.359
n = 1
qt.val = qtukey((1 - alpha), p, dfe)
SE.diff = sqrt(MSE)
(HSD = qt.val*SE.diff)
# Question 4 #

paperStrength <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0523.txt", header=TRUE)

paperStrengthLM = lm(Strength ~ factor(Hardwood)*factor(Pressure)*factor(Cooking), paperStrength)
summary(aov(paperStrengthLM))

par(mfrow = c(2,2))
plot(aov(paperStrengthLM, paperStrength))

par(mfrow = c(1,2))
with(paperStrength, interaction.plot(Hardwood, Cooking, Strength, type="b"))
with(paperStrength, interaction.plot(Cooking, Hardwood, Strength, type="b"))


