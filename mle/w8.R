tasmania=read.csv("/Users/smanurung/Documents/unimelb/Methods of Math Stats/Lab/EditedRainfall.csv")
View(tasmania)

s1 = tasmania[,2]
s2 = tasmania[,3]

mu.hat = mean(s1)
mu.hat
n = length(s1)
sigma.hat = sqrt((n-1) * var(s1)/n)
sigma.hat

library(MASS)
library(evd)
(gumbel.fit = fitdistr(x=s1, densfun=dgumbel, start=list(loc=50, scale=10), lower=1e-04))

(normal.fit = fitdistr(x=s1, densfun="normal"))
# TODO: compute var for x.bar and s manually!

log.like = function(theta) {
  loc = theta[1]; scale = theta[2]
  out = sum(log(dgumbel(s1, loc=loc, scale=scale)))
  return(out)
}

fit = optim(c(50,10), log.like, lower=0.0001, method="L-BFGS-B",
     control = list(fnscale=-1))
theta.hat = fit$par
theta.hat

# -0,13178
# -0,2626
