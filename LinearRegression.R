library(readxl)
setwd('/Users/smanurung/Documents/unimelb/Methods of Math Stats/Lab')

moz = read_excel('Mozart.xls')
moz = as.matrix(moz)
a = moz[,2] # Mozart's development/recapitulation
b = moz[,1] # Mozart's exposition
plot(a, a+b)
# The plot seems to show linear relationship between a and a+b

reg = lm(a+b ~ a) # regression with intercept

# TODO: understand every details information!
summary(reg)
intercept = coef(reg)[1]
slope = coef(reg)[2]
# So the equation is Y = 1.625*X + 1.553
#lines(a, slope*a + intercept, col="red")
abline(reg, lty="dotted")

reg0 = lm(a+b ~ 0+a)
summary(reg0)
slope0 = coef(reg0)
#lines(a, slope0*a, col="blue")
abline(reg0, lty="dashed", col="green")

# golden ratio line
goldenSlope = (1 + sqrt(5)) / 2
abline(0, goldenSlope)
# Golden line is very similar to the other two lines though the small gap is obvious.
