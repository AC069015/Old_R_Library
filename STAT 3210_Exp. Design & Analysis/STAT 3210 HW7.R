 # Problem 1 #

(light <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0515.txt", header=TRUE))

with(light, interaction.plot(Temperature, Glass, Light, type="b"))

lightLM = lm(Light ~ factor(Glass)*factor(Temperature), light)
summary(aov(lightLM))

lightQuad = lm(Light ~ factor(Glass)*(Temperature + I(Temperature^2)), light)
summary(aov(lightQuad))

ngrid = 20
tg = with(light, seq(min(Temperature), max(Temperature), length = ngrid))
grid = expand.grid(Temperature = tg, Glass = levels(factor(light$Glass)))
y.hat <- predict(lightQuad, grid)
y.hat <- matrix(y.hat, nrow = length(tg))
matplot(tg, y.hat, type = "l", xlab = "Temperature", lwd=3)
abline(v = c(100, 125, 150), lty = 2)
legend("topleft", legend = paste("Glass Type", levels(factor(light$Glass))), lty = 1:3, col = 1:3, lwd=3)

# Problem 2 #

(current <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0530.txt", header=TRUE))

with(current, interaction.plot(Temperature, Doping, Current, type="b"))
with(current, interaction.plot(Doping, Temperature, Current, type="b"))

currentLM = lm(Current~factor(Doping)*factor(Temperature), current)
summary(aov(currentLM))

currentQLM = lm(Current~Doping + Temperature + Doping:Temperature, current)
summary(currentQLM)
currentLackofFit = lm(Current~Doping + Temperature + Doping:Temperature + factor(Doping):factor(Temperature), current)
summary(aov(currentLackofFit))

currentFullLM = lm(Current~Doping*(Temperature + I(Temperature^2)), current)
summary(aov(currentFullLM))
summary(currentFullLM)

library(rsm)
persp(currentFullLM, Doping~Temperature)
contour(currentFullLM, Doping~Temperature)

# Problem 3 #

(vibration <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0609.txt", header=TRUE))

vibrationLM = lm(Vibration~Bit*Speed, vibration)
summary(aov(vibrationLM))

with(vibration, interaction.plot(Bit, Speed, Vibration, type="b"))
with(vibration, interaction.plot(Speed, Bit, Vibration, type="b"))

c(tapply(Vibration, Bit, mean), eff = diff(tapply(Vibration, Bit, mean)))
c(tapply(Vibration, Speed, mean), eff = diff(tapply(Vibration, Speed, mean)))
c(tapply(Vibration, Bit*Speed, mean), eff = diff(tapply(Vibration, Bit*Speed, mean)))
attach(vibration)
summary(vibrationLM)

persp(vibrationLM, Bit~Speed)
contour(vibrationLM, Bit~Speed)

