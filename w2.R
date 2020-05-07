# hypergeometric distribution (sampling without replacement)

# TASK 1a
prob1 <- dhyper(x=1, m=30, n=10000-30, 10)
prob0 <- dhyper(x=0, m=30, n=10000-30, 10)

# TASK 1b
x = 0:10
pmf = dhyper(x, 30, 10000-30, 10)
plot(x, pmf, type="o",
     main="hypergeometric pmf",
     sub="0 <= X <= 10",
     xlab="X",
     ylab="f(X)",
     cex.main=2,
     lwd=2)

# TASK 1c
pmf1 = dhyper(x, 30, 1000-30, 10)
pmf2 = dhyper(x, 30, 100-30, 10)
pmf3 = dhyper(x, 30, 50-30, 10)

plot(x, pmf1, type="o",
     main="hypergeometric pmf with total=1000, 100, 50",
     xlab="X",
     ylab="f(X)",
     col="green") # highest pmf will be on E(x) = np = 10*30/1000 = 0.3, since 3% is improvable.
lines(x, pmf2, col="red", type="o") # highest pmf will be on x = 3, since 30 out of 100 is improvable.
lines(x, pmf3, col="blue", type="o") # highest pmf will be on x = 6, since 30 out of 50 is improvable.

legend(6, 0.7, legend=c("N=1000", "N=100", "N=50"), col=c("green", "red", "blue"), lwd=2)

# TASK 2a
# This explains P(B|Ai). 2nd method will explain P(Ai|B).
loc = 0:1000
pmf = dhyper(5, loc, 1000-loc, 20)

#TASK 2b
plot(loc, pmf, type="l",
     main="pmf when the total loc is between 0 and 1k")

# TASK 2c
# COMMENT
# To meet the standard the # loc must be between 0 and 100. And from the pmf graph, it seems the
# probability is below 0.5 to meet the standard.

# if the observed number is 0
pmf = dhyper(0, loc, 1000-loc, 20)
plot(loc, pmf, type="l",
     main="pmf when the total loc is between 0 and 1k")
# COMMENT
# if the observed number is 0, then it's most likely that the code meets the standard.

# if the observed number is 1
pmf = dhyper(1, loc, 1000-loc, 20)
plot(loc, pmf, type="l",
     main="pmf when the total loc is between 0 and 1k")
# COMMENT
# if the observed number is 1, then the probability that the code meets the standard is quite
# high; higher than 0.5.

# 2ND METHOD
# TASK 2a: P(Ai|B)

# Given we found 5 improvable lines in the sample.
i1 = 0:100; i2 = 101:1000
prb = (0.9/101)*sum(dhyper(5, i1, 1000-i1, 20)) + (0.1/900)*sum(dhyper(5, i2, 1000-i2, 20))
pr=rep(0,1001)
# init vector of posterior probability
pr[1:101] = (0.9/101)*dhyper(5, i1, 1000-i1, 20)/prb
pr[102:1001] = (0.1/101)*dhyper(5, i2, 1000-i2, 20)/prb
plot(0:1000, pr, xlab="x", ylab="pmf", lwd=2,
     main=expression("Pr("~Lambda[i]~"|B)"))
sum(pr[1:101])

# Given we found 0 improvable lines in the sample.
prb = (0.9/101)*sum(dhyper(0, i1, 1000-i1, 20)) + (0.1/900)*sum(dhyper(0, i2, 1000-i2, 20))
pr=rep(0,1001)
# init vector of posterior probability
pr[1:101] = (0.9/101)*dhyper(0, i1, 1000-i1, 20)/prb
pr[102:1001] = (0.1/101)*dhyper(0, i2, 1000-i2, 20)/prb
plot(0:1000, pr, xlab="x", ylab="pmf", lwd=2,
     main=expression("Pr("~Lambda[i]~"|B)"))
sum(pr[1:101])

# Given we found 1 improvable line in the sample.
prb = (0.9/101)*sum(dhyper(1, i1, 1000-i1, 20)) + (0.1/900)*sum(dhyper(1, i2, 1000-i2, 20))
pr=rep(0,1001)
# init vector of posterior probability
pr[1:101] = (0.9/101)*dhyper(1, i1, 1000-i1, 20)/prb
pr[102:1001] = (0.1/101)*dhyper(1, i2, 1000-i2, 20)/prb
plot(0:1000, pr, xlab="x", ylab="pmf", lwd=2,
     main=expression("Pr("~Lambda[i]~"|B)"))
sum(pr[1:101])

# TASK 3
# binomial distribution (sampling with replacement)
x = 0:40
y1 = dbinom(x, size=40, prob=0.1)
y2 = dbinom(x, size=40, prob=0.5)
y3 = dbinom(x, size=40, prob=0.7)
y4 = dbinom(x, size=40, prob=0.9)

plot.default(x, y1, type="o", col="blue", ylab="pmf", xlab="x",
             main="binomial distribution",
             pch=0)
lines(x, y2, type="o", col="orange", pch=1)
lines(x, y3, type="o", col="green", pch=2)
lines(x, y4, type="o", col="red", pch=5)

# Workshop Problems
# p6
(0.15 * 0.95) / ((0.65 * 0.05) + (0.15 * 0.95))
