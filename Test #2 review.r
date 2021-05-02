#Libraries

library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library(leaps)
library(glmnet)

#Stepwise selection(forward and backward)
#For the given data, use the regsubsets() function to perform forward stepwise
#or backward stepwise selection, using the argument method= "forward" or "backward"

#With the given data

set.seed(1)
x = rnorm(100)
error = rnorm(100)
#y =rnorm(100)


b0 = 17; b1 = 3; b2 = .8; b3 = -2 #Set coefficients
y = b0 + b1*x + b2*x^3 + error #f(x)

#create data./frame with x^1,...x^10 <--- up to 10 degree polynomial

x.new = x
for (i in 2:10) {
  x.new = cbind(x.new,x^i)
}
colnames(x.new) = paste("x", 1:ncol(x.new), sep="")
data.8 = data.frame(cbind(y,x.new))

#Cut & paste up to here probably

#?regsubsets
#10 variables per the for statement on line 27
#responder ~ parameters
#nvmax = maximum size of subsets to examine
regfit.8 = regsubsets(y~ ., data=data.8, nvmax = 10) #regression subsets (Model selection by exhaustive search, forward or backward stepwise, or sequential replacement-- contains adjr, bic, cp, etc.)
#can add method = "forward" to line 40 for forward or backward, etc.
regsum.8 = summary(regfit.8)
#regsum.8 #for summary to see what is significant and what isn't significant
#regsum.8$bic

#all he's going to ask for
plot(regsum.8$cp)
plot(regsum.8$bic)
plot(regsum.8$adjr2)

#CP is the same as AIC
plot(regsum.8$cp, type="l", col = 4, xlab = "# of variables", ylab = "mallows cp")
points(which.min(regsum.8$cp),regsum.8$cp[which.min(regsum.8$cp)], col = 4, pch = 15, cex = 2)
#Lowest point is around 4, so you need four variables for having the list cp

plot(regsum.8$bic, type="l", col = 6, xlab = "# of variables", ylab = "Bayes information criterion")
points(which.min(regsum.8$bic),regsum.8$bic[which.min(regsum.8$bic)], col = 6, pch = 16, cex = 2)
#Lowest point is 3, so you need three variables for having the list bic

plot(regsum.8$adjr2, type="l", col = 3, xlab = "# of variables", ylab = "Adjusted R squared (R^2)")
points(which.max(regsum.8$adjr2),regsum.8$adjr2[which.max(regsum.8$adjr2)], col = 3, pch = 17, cex = 2)
#Highest point is 4, so you need around 4 variables

#what is the best coefficient? What is the coefficient intercept?
coef(regfit.8,which.min(regsum.8$cp)) #penalty for complex problem
coef(regfit.8,which.min(regsum.8$bic))
#BIC:  y = 17.07 + x.xX1 + 0.6X2  - 2.44XC3 + 0.08X5
coef(regfit.8,which.max(regsum.8$adjr2))

# use mtcars data hp as the response
# mpg, wt, drat, gsec as predictors
# 32 observations on 11 (numeric) variables

#Define response variable
y = mtcars$hp

#define matrix of predictor variables
x = data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

#given so far

#solve the following

#Fit lasso regression model

#perform k-fold cross-validation to find optimal lambda value
cv_model = cv.glmnet(x, y, alpha = 1) #Alpha 1 specifies the lasso model-- change if using a different model

#find optimal lambda value that minimizes test MSE
best_lambda = cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#find coefficients of best model -- best model for this lasso method
best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#find r-squared of model on training data
y_predicted = predict(best_model, s = best_lambda, newx = x)

sst = sum((y - mean(y)) ^2)
sse = sum((y_predicted - y)^2)

rsq = 1 - sse/sst
rsq

#find ridge regression
cv_model = cv.glmnet(x, y, alpha = 0) #alpha = 0 for ridge regression
