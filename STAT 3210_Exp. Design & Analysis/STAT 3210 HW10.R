#  STAT 3210 HOMEWORK 10 R CODE #

# PROBLEM 1 #

p0815 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0815.txt", header=TRUE)
p0815$A <- ifelse(p0815$Time == 2.5, -1, 1)
p0815$B <- ifelse(p0815$Concentration == 14, -1, 1)
p0815$C <- ifelse(p0815$Pressure == 60, -1, 1)
p0815$D <- ifelse(p0815$Temperature == 225, -1, 1)

p0815LM = lm(Yield ~ A*B*C*D, p0815)
summary(p0815LM)

alias(p0815LM)

library(gplots)
qqnorm(aov(p0815LM), label = TRUE)

# PROBLEM 2 #

p0809 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0809.txt", header=TRUE)

p0809LM = lm(Color ~ Solvent*Catalyst*Temperature*Purity*pH, p0809)
summary(p0809LM)

alias(p0809LM)

qqnorm(aov(p0809LM), label=TRUE)

# PROBLEM 3 #

p0852 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0852.txt", header=TRUE)
p0852LM = lm(Gain ~ A*B*C*D*E*F, p0852)

alias(p0852LM)

qqnorm(aov(p0852LM), label=TRUE)

