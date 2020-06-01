library(readxl)
setwd('/Users/smanurung/Documents/unimelb/Methods of Math Stats/Lab/linear-regression')

moz = read_excel('Mozart.xls')
moz = as.matrix(moz)
a = moz[,2] # Mozart's development/recapitulation
b = moz[,1] # Mozart's exposition

##############################################################
# a+b vs a
##############################################################

## regression with intercept
plot(a, a+b)
reg = lm(a+b ~ a)
summary(reg) # TODO: understand every details information!
intercept = coef(reg)[1]
slope = coef(reg)[2]
#lines(a, slope*a + intercept, col="red")
abline(reg, lty="solid", col='green')

# regression fit w/o intercept
reg0 = lm(a+b ~ 0+a)
summary(reg0)
slope0 = coef(reg0)
#lines(a, slope0*a, col="blue")
abline(reg0, lty="dashed", col="blue")

# TODO: then, how could we evaluate the meaning of both slopes?

# golden ratio line
goldenSlope = (1 + sqrt(5)) / 2
abline(0, goldenSlope, lty='dotted', col="red")
# Golden line is very similar to the other two lines though the small gap is obvious.

x.bar = mean(a+b/a)
phi = goldenSlope
# The difference is around 0.03, with x.bar less than phi.
# And yes, sample mean is close to phi.

# x.bar > slope w/o intercept > slope w/ intercept > goldenSlope.

##############################################################
# a vs b
##############################################################

## Regression fit w/ intercept
plot(b, a)
reg2 = lm(a ~ b)
summary(reg2)
abline(reg2, lty='solid', col='green')

# regression fit w/o intercept
reg20 = lm(a ~ 0+b)
summary(reg20)
abline(reg20, lty='dashed', col='blue')

# golden slope line
abline(0, goldenSlope, lty='dotted', col='red')

# sample mean
x.bar2 = mean(a/b)
# goldenSlope > x.bar2 > slope w/o intercept > slope w/ intercept.

## Residuals vs a.
resGolden = (a + b) - goldenSlope*a # residual is y - y.hat
resLM = reg$residuals
resLM0 = reg0$residuals
plot(a, resGolden, ylim=c(-15,20),
     main = 'residuals vs a',
     ylab = 'residuals')
points(a, resLM, pch=0, col="green")
points(a, resLM0, pch=2, col='blue')

## Residuals vs b.
resGolden2 = a - goldenSlope*b
resLM2 = reg2$residuals
resLM20 = reg20$residuals
plot(b, resGolden2,
     main = 'residuals vs b',
     ylab = 'residuals')
points(b, resLM2, pch=0, col='green')
points(b, resLM20, pch=2, col='blue')
