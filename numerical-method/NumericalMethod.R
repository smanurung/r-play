######################################################
# Sample generation
######################################################

cdf <- function(x, theta) {
  1 - theta/(x^3) - (1-theta)/(x^5)
}

loss <- function(x, theta, q) {
  (cdf(x, theta) - q)^2
}

# 1st method: use L-BFGS-B
fit.cdf <- function(x) {
  optim(c(3), loss, theta=0.3, q=x, method="L-BFGS-B",
      lower = 1)
}

# 2nd method: use BFGS
fit.cdf2 <- function(x) {
  optim(c(3), loss, theta=0.3, q=x, method="BFGS")
}

# TODO: 3rd method: use nlm

fit.cdf(0.5)
fit.cdf2(0.5)

inverse.cdf <- function(x){
  f <- fit.cdf(x)
  f$par
}

# generate n<-100 samples
rcustom <- function(n) {
  sapply(runif(n), inverse.cdf) # TODO: how's lapply and sapply different?
}

samples <- rcustom(100)

######################################################
# Maximum Likelihood Estimation
######################################################

pdf <- function(x, theta) {
  3*theta/(x^4) + 5*(1-theta)/(x^6)
}

# theta is put first, because it's the parameter to be optimised
llfunc <- function(theta, x) {
  sum(log(pdf(x, theta)))
}

# 1st method: unconstraint optim
fit.optim.unc = optim(c(0.5), llfunc,
      x = samples,
      control = list(fnscale = -1),
      method = "BFGS")

fit.optim.con = optim(c(0.5), llfunc,
                      x = samples,
                      control = list(fnscale = -1),
                      method = "L-BFGS-B",
                      lower = 0,
                      upper = 1)
# NOTE:
# Estimate reported for theta is 0.3023.

# Warning message:
#  In optim(c(0.5), llfunc, x = samples, control = list(fnscale = -1)) :
#  one-dimensional optimization by Nelder-Mead is unreliable:
#  use "Brent" or optimize() directly

# 2nd method: nlm

# 3rd method: fitdistr

######################################################
# Linear Regression
######################################################

x <- sapply(1:20, function(x) x/10)
alpha <- 1; beta <- 2
ep <- rnorm(length(x), mean = 0, sd = x)
y <- alpha + beta*x + ep
plot(x, y, main = "Y = alpha + beta*xi + ep")

# theta <- [alpha, beta, sigma.squared]
llfunc <- function(theta, x, y) {
  sum(log(dnorm(y, mean = theta[1]+theta[2]*x, sd = sqrt(theta[3]*x))))
}

optim(c(0.1, 0.1, 0.1), llfunc, x=x, y=y,
      control = list(fnscale = -1))
fit.linear <- optim(c(0.1, 0.1, 0.1), llfunc, x=x, y=y,
      control = list(fnscale = -1),
      method = "L-BFGS-B", lower = 0)
# NOTE: with default method (Nelder-Mead), some return "NaN"
# because the parameters value aren't bounded.
# It is resolved by using 'L-BFGS-B' method.

# So, the results of the estimation:
# alpha         : 0.8068435
# beta          : 2.6419369
# sigma.squared : 1.6643479
fit.linear.alpha <- fit.linear$par[1]
fit.linear.beta <- fit.linear$par[2]
fit.linear.sigma.squared <- fit.linear$par[3]
abline(fit.linear.alpha, fit.linear.beta, col='green',
       lty='dashed')
