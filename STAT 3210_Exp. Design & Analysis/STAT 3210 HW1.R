( miceID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L") )
sample(miceID)
flares <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0228.txt",header = TRUE)
flares
var.test(flares$Type1, flares$Type2, ratio = 1)
t.test(flares$Type1, flares$Type2, var.equal = TRUE)
par(mfrow=c(2,1))
qqnorm(flares[,"Type1"], datax=FALSE);
qqnorm(flares[,"Type2"], datax=FALSE);
t.test(flares$Type1, flares$Type2, var.equal = FALSE)
boxplot(flares)
