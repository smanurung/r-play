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

# quantile-quantile plot.
qqnorm(x)
qqline(x)

# dataframe
x = rnorm(100)
y = 2*x + rnorm(100, 0, 0.8)
z = 0.5*x + rnorm(100, 0, 0.5)
t = data.frame(x, y, z)
summary(t$x)
plot(t)

# list
L = list(one=1, two=c(1,2), five=seq(0, 1, length=5))
L
L$five + 10

# i/o
t = data.frame(x=c(1, 2, 3), y=c(30, 20, 10))
t
write.table(t, file="mydata.txt", row.names=FALSE)
t2 = read.table(file="mydata.txt", header=TRUE)
t2

x=c(rnorm(10), NA, rnorm(2))
x
min(x)
min(x, na.rm=TRUE)
mean(x, na.rm=TRUE)

# conditionals
x = rnorm(10)
x
if(mean(x) > median(x))
{
  "The mean is greater than the median"
}else{
  "The mean is smaller than the median"
}

# for loop
B = 1000 # number of trials
n = 5 # sample size
xbar.seq = 1:B
for (i in 1:B)
{
  sample = rnorm(5)
  xbar.seq[i] = mean(sample)
}
hist(xbar.seq)

# function
myfun = function(x)
{
  y = x^2
  return(y)
}
myfun(1.5)

x = seq(-2, 2, length=100)
plot(x, myfun(x), type="l")

mymedian <- function(x) {
  n = length(x)
  if(n %% 2 == 1) {
    sort(x)[(n+1)/2]
  }else{
    middletwo = sort(x)[(n/2) + 0:1]
    return(mean(middletwo))
  }
}
x = rnorm(100)
mymedian(x)
median(x)
