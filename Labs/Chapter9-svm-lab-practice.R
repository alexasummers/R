library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library(e1071)

#SVM _ simulated data

set.seed(10111)
n = 40
c = 2
x=matrix(rnorm(n),n/c,c) #normal distribution
y = rep(c(-1,1), c(10,10))
print(y)
x[y == 1, ] = x[y == 1, ] + 1
plot (x, col = y + 3, pch = 19)

set.seed(10111)
m = matrix(rnorm(40), 20, 2)
n = rep (c(-1,1), c(10,10))
m[n == 1, ] = m[n == 1, ] + 1
plot(m, col = n + 3, pch = 19)

# Linear SVM kernal

d = data.frame(x, y = as.factor(y))
#cost is the regularization term
svmfit = svm(y ~ ., data = d, kernal = "linear", cost = 10, scale = FALSE)
print (svmfit)
plot(svmfit,d)
#not fancy at all

