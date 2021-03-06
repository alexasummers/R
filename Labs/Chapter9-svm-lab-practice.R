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

# Linear SVM kernel

d = data.frame(x, y = as.factor(y))
#cost is the regularization term
svmfit = svm(y ~ ., data = d, kernal = "linear", cost = 10, scale = FALSE)
print (svmfit)
plot(svmfit,d)
#not fancy at all

make.grid = function(x, n = 75){
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1],length = n)
  x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
  expand.grid(X1 = x1, X2 = x2) 
}

xgrid = make.grid(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red", "blue") [as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)

#?svm
beta = drop(t(svmfit$coefs) %*% x[svmfit$index, ]) #scalar product for coefficient
beta0 = svmfit$rho #the negative intercept

plot(xgrid, col = c("red", "blue") [as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch= 19)
points(x[svmfit$index, ], pch = 5, cex = 2)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2)
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2)


#radial kernel
load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)
plot(x, col = y + 1)

d1 = data.frame(y=factor(y), x)

#radial kernel with cost term 5
fit = svm(factor(y) ~ ., data = d1, scale = FALSE, kernel = "radial", cost = 5)
str(fit)

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y + 1, pch = 19)

func = predict (fit, xgrid, decision.vaules = TRUE)
func = attributes(func)$decision
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(prob, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)
contour(px1, px2, matrix(prob, 69, 99), level = 0.6, add = TRUE, col = "green", lwd = 2)
