62+5-7*9+15/3-2^2 #^means power
2+3/4^3
2+(3/4)^3

log(15)
sqrt(4)
pi*4^2

# two ways of doing assignments.
a <- pi*4^2
a
b = pi * 4^2

rm(list=ls())

x = c(4.1, -1.3, 2.5, -0.6, -21.7)
x

y = c(30,35,4)
y

z = c(x,y)
z

n = 5
1:n-1

1:(n-1)

z[2]
z[1:4]
z[c(1, 2, 6)]
z[2] = 2015
z

seq(1, 2, by=0.1)
seq(1, 2, length=20)

A = matrix(c(2, -1, 4, 2, -1, 3), nrow=2)
A

A[1,2]
A[1,]
A[,2]
mean(A)

u1 = c("male", "female")
u2 = c("apple", "pear", "kiwi", "orange")
u1
u2

# What's potentially the use of this?
labels = paste(c("X", "Y"), 1:10, sep="")
labels

z = rnorm(10)
z
mean(z)
x = rnorm(50, mean = 10, sd = 2)
x
mean(x)

x = rnorm(100, mean = 10, sd = 2)
y = rnorm(100, mean = 0, sd = 4)
plot(x, y, xlab="Name of variable x", ylab="Name of variable y")

boxplot(x, y, names = c("x", "y"))
hist(x, freq=FALSE, nclass=10)

qqnorm(x)
qqline(x)
