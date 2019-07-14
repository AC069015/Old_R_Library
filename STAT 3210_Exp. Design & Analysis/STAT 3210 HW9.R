 # STAT 3210 Homework 9 #

# Problem 1 #

problem1 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0704.txt", header=TRUE)

problem1LM = lm(Life ~ Block + Speed*Geometry*Angle, problem1)
summary(problem1LM)

library(gplots)
qqnorm(aov(problem1LM), label = TRUE)

problem1Reduced = lm(Life ~ Block + Speed + Geometry + Angle + Speed:Angle + Geometry:Angle, problem1)
summary(aov(problem1Reduced))

# Problem 2 #

problem2 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0713.txt", header=TRUE)

problem2LM = lm(UEC ~ Block + Laser*Pulse*Cell*Writing, problem2)
summary(problem2LM)

qqnorm(aov(problem2LM), label = TRUE)

# Problem 3 #

problem3 <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0724.txt", header=TRUE)

problem3Rep1 <- problem3[problem3$Rep == 1,]
Speed <- problem3Rep1$Speed
Geometry <- problem3Rep1$Geometry
Angle <- problem3Rep1$Angle
problem3Rep1$Block <- ifelse(Speed*Geometry*Angle < 0, 1, 2)
problem3Rep1 <- problem3Rep1[order(problem3Rep1$Block),]

problem3Rep2 <- problem3[problem3$Rep == 2,]
Speed <- problem3Rep2$Speed
Geometry <- problem3Rep2$Geometry
Angle <- problem3Rep2$Angle
problem3Rep2$Block <- ifelse(Speed*Geometry*Angle > 0, 1, 2)
problem3Rep2 <- problem3Rep2[order(problem3Rep2$Block),]

problem3Rep3 <- problem3[problem3$Rep == 3,]
Speed <- problem3Rep3$Speed
Geometry <- problem3Rep3$Geometry
Angle <- problem3Rep3$Angle
problem3Rep3$Block <- ifelse(Speed*Geometry*Angle < 0, 1, 2)
problem3Rep3 <- problem3Rep3[order(problem3Rep3$Block),]

(partialConfounding <- rbind(problem3Rep1, problem3Rep2, problem3Rep3))
partialConfounding$Blocks <- factor(paste(partialConfounding$Rep, partialConfounding$Block, sep = "-"))

problem3PC = lm(Life ~ Blocks + Speed*Geometry*Angle, partialConfounding)
summary(aov(problem3PC))
summary(problem3PC)
library(car)
vif(problem3PC)

