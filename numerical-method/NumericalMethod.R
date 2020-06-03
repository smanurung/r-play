######################################################
# Sample generation
######################################################

cdf <- function(x, theta) {
  1 - theta/x^3 - (1-theta)/x^5
}

loss <- function(x, q, theta) {
  (cdf(x, theta) - q)^2
}

# TODO: to avoid local optimum, try multiple starting point.

# 1st method: use L-BFGS-B
fit.cdf <- function(quant) {
  optim(c(3), loss, theta=0.3, q=quant, method="L-BFGS-B",
      lower = 1)
}

# 2nd method: use BFGS
fit.cdf2 <- function(quant) {
  optim(c(3), loss, theta=0.3, q=quant, method="BFGS")
}

# TODO: 3rd method: use nlm

fit.cdf(0.5)
fit.cdf2(0.5)
# NOTE: both approaches return the same estimate: 1.173604.
# TODO: but why are the fn values different? Since the
# objective functions are the same.

inverse.cdf <- function(x){
  f <- fit.cdf(x)
  f$par
}

# generate samples
# 1st method: using sapply
rcustom <- function(n) {
  sapply(runif(n), inverse.cdf) # TODO: how's lapply and sapply different?
}
samples <- rcustom(100)

# 2nd method: using for loop
n <- 100; samples1 = rep(0,100); u = runif(100)
for(i in 1:n) samples1[i] <- inverse.cdf(u[i])

######################################################
# Maximum Likelihood Estimation
######################################################

pdf <- function(x, theta) {
  3*theta/x^4 + 5*(1-theta)/x^6
}

# theta is put first, because it's the parameter to be optimised
llfunc <- function(theta, x) {
  sum(log(pdf(x, theta)))
}

# TODO: use negative log likelihood

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
set.seed(123)
# x <- sapply(1:20, function(x) x/10)
x <- 1:20/10
alpha <- 1; beta <- 2; sigma <- 1
ep <- rnorm(length(x), mean = 0, sd = x)
y <- alpha + beta*x + ep
plot(x, y, main = "Y = alpha + beta*xi + ep")
set.seed(123)
y2 <- rnorm(length(x), mean=alpha+beta*x, sd=sigma*sqrt(x))
plot(x, y2)

# theta <- [alpha, beta, sigma.squared]
# use negative log likelihood for minimisation
llfunc <- function(theta, x, y) {
  -sum(dnorm(y, mean = theta[1]+theta[2]*x, sd = sqrt(theta[3]*x), log = T))
}
fit.linear <- optim(c(1, 2, 1), llfunc, x=x, y=y2,
      method = "L-BFGS-B", lower = c(0.1, 0.1, 0.1))
# NOTE: with default method (Nelder-Mead), some return "NaN"
# because the parameters value aren't bounded.
# It is resolved by using 'L-BFGS-B' method.

# So, the results of the estimation:
# alpha         : 0.8068435, 1.0464741
# beta          : 2.6419369, 2.0676009
# sigma.squared : 1.6643479, 0.9038637
fit.linear.alpha <- fit.linear$par[1]
fit.linear.beta <- fit.linear$par[2]
fit.linear.sigma.squared <- fit.linear$par[3]
abline(fit.linear.alpha, fit.linear.beta, col='green',
       lty='dashed')
