qbinom(c(0.025,0.975), size = 11, prob = 0.5)

pm <- pbinom(8, size = 11, prob = 0.5) - pbinom(1, size = 11, prob = 0.5)

genq <- function(n) {
  X <- runif(n)
  Y <- sort(X)
  c(Y[2], Y[9])
}
genq(11)

t = as.matrix(rep(11,10000))
C = t(apply(t, 1, genq))
matplot(C, type='l')
abline(0.5, 0, lty='dotted')

psim <- sum(C[,1] < 0.5 & C[,2] > 0.5)/nrow(C)
diff <- abs(psim - pm)
# NOTE: diff is very small, 1e-03.
# And diff grows smaller as the number of samples increase.

setwd('/Users/smanurung/Documents/unimelb/Methods of Math Stats/Lab')
source('./Lab11.RData')
load(Lab11.RData)

X <- c(79, 315, 445, 350, 136, 723, 198, 75, 161, 13,
       215, 24, 57, 152, 238, 288, 272, 9, 315, 11, 51,
       98, 620, 244, 34)
plot(ecdf(X))

abline(0.25, 0)
abline(0.5, 0)
abline(0.75, 0)
Y <- sort(X)
q0.25 <- Y[6]; q0.5 <- Y[12]; q0.75 <- Y[19]
# NOTE: approximation to respective quantile:
# q0.25 = ~51
# q.05 = ~152
# q0.75 = ~288

# Q: Why do we use ppoints(100) in the solution?
qqplot(X, qexp(ppoints(25), 1/mean(X)))
# NOTE: The distribution is fairly similar.

qqplot(X, qexp(ppoints(25), 1))
# NOTE: if ppoints(25), both qqplot result in the same graph.
# However, if using ppoints(100), the later will have smaller angle from x-axis.

qqnorm(X)
# NOTE: qqnorm shows that the data is not of normal distribution.

quantile(X, c(0.25, 0.5, 0.75), type=6)

# confidence interval and confidence level
p0.25 <- pbinom(9, 25, 0.25) - pbinom(2, 25, 0.25)
Y[3]
Y[10]
# NOTE:
# - confidence level = 0.8965632
# - confidence interval = (19, 98)
qbinom(c(0.05, 0.95), 25, 0.25)

p0.5 <- pbinom(16, 25, 0.5) - pbinom(8, 25, 0.5)
Y[9]
Y[17]
# NOTE:
# - confidence level: 0.8922479
# - confidence interval: (79, 244)
qbinom(c(0.05, 0.95), 25, 0.5)
# NOTE: it's originally (8, 17), becomes (9,17) after certain adjustment.

p0.75 <- pbinom(22, 25, 0.75) - pbinom(15, 25, 0.75)
Y[16]
Y[23]
# NOTE:
# - confidence level: 0.8965632
# - confidence interval: (238, 445)
qbinom(c(0.05, 0.95), 25, 0.75)
# NOTE: the original interval is (15, 22)

# t-approximation for mean of the same confidence level.
mean(X) + qt(c(0.05, 0.95), df = 24) * sqrt(var(X)/length(X))
# NOTE: confidence interval for mean: (133.6997, 276.1403)
# This is quite far from median value: (79, 244),
# This is mainly because the distribution is not normal, as seen from
# the qqplot.

# 2nd method: using the exact confidence level for median: 0.8922479
alpha <- (1 - 0.8922479)/2
mean(X) + qt(c(alpha, 1-alpha), df=24) * sqrt(var(X)/length(X))
# NOTE: confidence interval for mean: (142.7109, 267.1291)

# 3rd method: t-test.
t.mean <- t.test(X, conf.level = 0.8922479, alternative = "two.sided")
t.mean$conf.int
# NOTE: confidence interval for mean: (142.7571, 267.0829)
# the result using both t.test & manual computation are the same.

t.test(X, conf.level = 0.9)$conf.int
# NOTE: (141.2186, 268.6214)