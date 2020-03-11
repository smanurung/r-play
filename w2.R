# hypergeometric distribution (sampling without replacement)
dhyper(1, 30, 10000-30, 10)
dhyper(0, 30, 10000-30, 10)

x = 0:10
x
pmf = dhyper(x, 30, 10000-30, 10)
pmf
plot(x, pmf)
plot(x, pmf, type="l")

pmf1k = dhyper(x, 30, 1000-30, 10)
pmf100 = dhyper(x, 30, 100-30, 10)
pmf50 = dhyper(x, 30, 50-30, 10)

plot(x, pmf1k, type="l") # highest pmf will be on x = 30/1000 * 10 = 0.3, since 30 out of 1000 is improvable.
lines(x, pmf100) # highest pmf will be on x = 3, since 30 out of 100 is improvable.
lines(x, pmf50) # highest pmf will be on x = 6, since 30 out of 50 is improvable.

# binomial distribution (sampling with replacement)
x = 0:40
pmf01 = dbinom(x, 40, 0.1)
pmf05 = dbinom(x, 40, 0.5)
pmf07 = dbinom(x, 40, 0.7)
pmf09 = dbinom(x, 40, 0.9)

plot(x, pmf01, col="blue")
points(x, pmf05, col="yellow")
points(x, pmf07, col="green")
points(x, pmf09, col='red')
