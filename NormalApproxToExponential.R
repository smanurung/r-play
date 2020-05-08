# Normal approximation to exponential distribution

n <- c(1:100) # sample size
actual_prob <- pgamma(2 * sqrt(n) + n, shape=n, rate=1) - pgamma(-2 * sqrt(n) + n, shape=n, rate=1)
plot(n, actual_prob, type="l",
     main="Normal approx. to Exponential with 0<=n<=30")
abline(a = pnorm(2) - pnorm(-2), b = 0, lty=2)
