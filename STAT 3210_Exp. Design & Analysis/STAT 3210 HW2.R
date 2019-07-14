# Problem 1
bearings <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0234.txt", header = TRUE)
bearings
t.test(bearings$Caliper1, bearings$Caliper2, paired=TRUE)
diff = bearings$Caliper1 - bearings$Caliper2
qqnorm(diff)
qqline(diff)
t.test(bearings$Caliper1, bearings$Caliper2, var.equal=TRUE)

#Problem 2
machines <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0226.txt", header = TRUE)
machines
mean1=mean(machines[,1])
mean2=mean(machines[,2])
diff=mean1-mean2
diff

#Problem 3
wafers <- read.table("http://www.stat.uiowa.edu/~ernli/DOEdata/problem0231.txt", header = TRUE)
wafers
t.test(wafers$Thick95, wafers$Thick100, var.equal=TRUE, alternative="greater")
boxplot(wafers)
var.test(wafers$Thick95, wafers$Thick100, ratio=1)
par(mfrow=c(2,1))
qqnorm(wafers$Thick95)
qqline(wafers$Thick95)
qqnorm(wafers$Thick100)
qqline(wafers$Thick100)
power.t.test(n=8, delta=1.5, sd=1.884, sig.level=0.05, power=NULL, type="two.sample", alternative="one.sided", strict=FALSE)
delta=1.5
sigma=1.884
n=8
alpha=0.05
df=n+n-2
tcritical=qt(1-alpha, df)
ncp=abs(delta)/sqrt(2*sigma^2/n)
beta=pt(tcritical, df, ncp)
power=1-beta
power
power.t.test(n=8, delta=2.5, sd=1.884, sig.level=0.05, power=NULL, type="two.sample", alternative="one.sided", strict=FALSE)
power.t.test(n=15, delta=1.5, sd=1.884, sig.level=0.05, power=NULL, type="two.sample", alternative="one.sided", strict=FALSE)
delta=1.5
sigma=1.884
n1=6
n2=10
alpha=0.05
df=n1+n2-2
tcritical=qt(1-alpha, df)
ncp=abs(delta)/sigma*sqrt((1/n1) +(1/n2))
beta=pt(tcritical, df, ncp)
power=1-beta
power
power.t.test(n=NULL, delta=2.5, sd=1.884, sig.level=0.05, power=0.80, type="two.sample", alternative="one.sided", strict=FALSE)
power.t.test(n=NULL, delta=1.5, sd=1.884, sig.level=0.05, power=0.80, type="two.sample", alternative="one.sided", strict=FALSE)
power.t.test(n=NULL, delta=1.5, sd=1.884, sig.level=0.05, power=0.90, type="two.sample", alternative="one.sided", strict=FALSE)
t.test(wafers$Thick95, wafers$Thick100, paired=TRUE, var.equal=TRUE, alternative="greater")
