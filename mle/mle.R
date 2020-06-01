setwd('/Users/smanurung/Documents/unimelb/Methods of Math Stats/Lab/mle')
tasmania=read.csv('EditedRainfall.csv')
View(tasmania)
names(tasmania)
dim(tasmania)

s1 = tasmania[,2]
s2 = tasmania[,3]

# assuming normal approximation
mu.hat = mean(s1)
n = length(s1)
sigma.hat = sqrt((n-1)/n*var(s1))

library(MASS)
library(evd)

gumbel.fit = fitdistr(x = s1, densfun = dgumbel,
                      start = list(loc = 50, scale = 10),
                      lower = 1e-04)
normal.fit = fitdistr(x = s1, densfun = "normal")

# log likelihood function
llfunc = function(theta){
  loc = theta[1]; scale = theta[2]
  sum(log(dgumbel(s1, loc = loc, scale = scale)))
}

fit = optim(c(50,10), llfunc, lower = 1e-04,
            method = "L-BFGS-B",
            control = list(fnscale = -1))
theta.hat = fit$par

# plot check
pdf_normal = function(x){
  dnorm(x, mean = mu.hat, sd = sigma.hat)
}

pdf_gumbel = function(x){
  dgumbel(x, loc = theta.hat[1], scale = theta.hat[2])
}

# xlim attr below is crucial to compare the lower tail
# probability.
hist(s1, freq = FALSE, col = "gray", xlab = "x",
     xlim = c(0,100))
curve(pdf_normal, from = 0, to = 100, add = TRUE, col = "green",
      lty="dashed", lwd=2)
curve(pdf_gumbel, from = 1, to = 100, add = TRUE, col = "blue",
      lty="dotted")

# NOTE:
# Gumbel distribution matches better than normal approximation

########################################
# Method of Moments Estimation
########################################

# Given the formula (calculated on paper)
sigma.tilde = sqrt(6)*sqrt(var(s1))/pi
mu.tilde = mean(s1) - (-digamma(1))*sigma.tilde

########################################
# Error in Estimation
########################################

# generate 1000 samples from extreme value distribution.

random_gumbel = function(x) {
  # use estimates from mle.
  rgumbel(length(s1), loc = theta.hat[1], scale = theta.hat[2])
}
samples = lapply(1:100, random_gumbel)

# evaluate mle approach
# generate estimate
mle.fit = function(x) {
  # QUESTION: why doesn't it work if I use scale = 15, but
  # works when scale is 10?
  # UPDATE: but seems once I changed to 10 or 0, error still happens!
  fitdistr(x, dgumbel, start = list(loc = 50, scale = 0),
           lower = 1e-04)
}
gumbel.mle.fit = lapply(samples, mle.fit)

# evaluate mom approach
mm.fit = function(x) {
  scale.tilde = sqrt(6)*sqrt(var(x))/pi
  location.tilde = mean(x) + digamma(1)*scale.tilde
  c(location.tilde, scale.tilde)
}
gumbel.mm.fit = lapply(samples, mm.fit)

location.mle = sapply(gumbel.mle.fit, function(x) x$estimate[1])
location.mm = sapply(gumbel.mm.fit, function(x) x[1])
scale.mle = sapply(gumbel.mle.fit, function(x) x$estimate[2])
scale.mm = sapply(gumbel.mm.fit, function(x) x[2])

estimatel = list(mle.location = location.mle,
             mle.scale = scale.mle,
             mm.location = location.mm,
             mm.scale= scale.mm)
lapply(estimatel, mean)
lapply(estimatel, sd)
# NOTE:
# 1. MM has closer mean than MLE for both location and scale.
# 2. MLE has lower sd for scale parameter, but MM has lower sd for location param.

# OPINION:
# In this context, I guess MM is better approach than MLE.

# fitdistr example w/ Gamma distribution
# x <- rgamma(1000000, shape = 5, rate = 0.1)
# fd <- fitdistr(x, "gamma")