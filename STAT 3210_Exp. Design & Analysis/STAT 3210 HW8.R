# Problem 1 #

bottleFill <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0624.txt", header = TRUE)
bottleFill$C <- ifelse(bottleFill$Carbonation == 10, -1, 1)
bottleFill$P <- ifelse(bottleFill$Pressure == 25, -1, 1)
bottleFill$S <- ifelse(bottleFill$Speed == 200, -1, 1)
bottleFill

bottleFillLM = lm(Deviation ~ C*P*S, bottleFill)
summary(aov(bottleFillLM))
summary(bottleFillLM)

library(gplots)
qqnorm(aov(bottleFillLM), label = TRUE)

bottleFillReduced = lm(Deviation ~ C + P + S + C:P, bottleFill)
summary(aov(bottleFillReduced))
summary(bottleFillReduced)
lackOfFitTestLM = lm(Deviation ~ C + P + S + C:P + factor(C):factor(P):factor(S), bottleFill)
summary(aov(lackOfFitTestLM))

par(mfrow = c(2,2))
plot(aov(bottleFillReduced))

# Problem 2 #

uec <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0626.txt", header = TRUE)

uecLM = lm(UEC ~ Laser*Pulse*Cell*Writing, uec)
summary(uecLM)

library(gplots)
qqnorm(aov(uecLM), label = TRUE)

effs = 2*coef(uecLM)[-1][-7][-8][-9]
PSE = function(e) {
	abseff = abs(e)
	s0 = 1.5*median(abseff)
	1.5*median(abseff[abseff<2.5*s0])
	}
cbind(effs, pseudo.t = effs/PSE(effs))

uecCollapsed = lm(UEC~Laser*Cell*Writing, uec)
summary(aov(uecCollapsed))

uecReduced = lm(UEC ~ Laser + Cell + Writing + Laser:Cell + Cell:Writing, uec)
summary(uecReduced)

# Problem 3 #

uec27 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0627.txt", header = TRUE)

uec27LM = lm(UEC ~ Laser*Pulse*Cell*Writing + I(Laser^2) + I(Pulse^2) + I(Cell^2) + I(Writing^2), uec27)
(effs = 2*coef(uec27LM)[-1])
qqnorm(aov(uec27LM), label = TRUE)
PSE = function(e) {
	abseff = abs(e)
	s0 = 1.5*median(abseff)
	1.5*median(abseff[abseff<2.5*s0])
	}
cbind(effs, pseudo.t = effs/PSE(effs))

summary(aov(uec27LM))

uec27Reduced = lm(UEC ~ Laser*Cell + Cell*Writing + I(Laser^2) + I(Cell^2) + I(Writing^2), uec27)
summary(aov(uec27Reduced))

uec27LackOfFit = lm(UEC ~ Laser*Cell + Cell*Writing + I(Laser^2) + I(Cell^2) + I(Writing^2) + factor(Laser):factor(Pulse):factor(Cell):factor(Writing), uec27)
summary(aov(uec27LackOfFit))