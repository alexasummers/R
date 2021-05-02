#Libraries

library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library(leaps)
library(glmnet)
library(splines)
attach(Wage)
require(tree)
library(randomForest)

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

#Fit lasso regression model with CV to find the best lambda
# find coefficients of best model
# find the r squared (r^2) value

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

#fit lasso regression model with lambda value 20
#find coefficients of best model
test_model = glmnet(x, y, alpha = 1, lambda = 20)
coef(test_model)

#find r-squared of model on training data (R^2)
y_predicted = predict(best_model, s = best_lambda, newx = x)

sst = sum((y - mean(y)) ^2)
sse = sum((y_predicted - y)^2)

rsq = 1 - sse/sst
rsq #the higher the better

#find ridge regression model with CV to find the best lambda
#find coefficients of best model
#Find the R^2 value

cv_model = cv.glmnet(x, y, alpha = 0) #alpha = 0 for ridge regression

#find optimal lambda value that minimizes test MSE
best_lambda = cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#find R squared of model on training data
y_predicted = predict(best_model, s = best_lambda, newx = x)

sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)

rsq = 1 - sse/ sst
rsq

#attach(Wage) for chapter 7

agelims = range(age)
#create an age grid for the targeted values of the prediction
age.grid = seq(from = agelims[1], to = agelims[2])

#Q. In this question, we analyze the Wage data in order to illustrate the fact 
# that many of the complex non-linear fitting procedures.

# Plot x=age and y=wage relation
plot(age, wage, col="darkgray")

#Use a created grid of values for age at which you want predictions, and then
#call the generic predict() function
#Fit linear regression for the wage data (wage: response, age: predictor)
#and show it on the plot
fitz = lm(wage~age, data=Wage)
lines(age.grid, predict(fitz, list(age = age.grid)), lwd = 2, col = "blue")

#fit degree of 20 polynomial for the Wage data (wage: response, age: predictor)
#and show it on the plot
#This created a lot of wiggle around the edges, so we want to decrease the number of polynomials
fitx = lm(wage ~ poly(age, df = 20), data = Wage)
lines(age.grid, predict(fitx, list(age = age.grid)), col="blue", lwd = 2)

#fit degree of 6 polynomial for the Wage data (Wage: response, age: predictor)
# and show it on the plot
fitx = lm(wage~poly(age, df=6), data = Wage)
lines(age.grid,predict(fitx,list(age = age.grid)), col ="red", lwd = 2)

#fit cubic splines with 3 knots at 25, 40, 60 for the Wage data (wage: response, age: predictor)
#and show it on the plot.
plot(age,wage,col="darkgray") #to replot the points and get rid of lines from above questions
fit0 = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) #bs stands for a degree three spline
lines(age.grid, predict(fit0, list(age = age.grid)), col="green", lwd = 2)

#fit cubic splines with 2 knots at 40, 60 for the Wage data (wage: response, age: predictor)
#and show it on the plot
fit0 = lm(wage ~ bs(age, knots = c(40, 60)), data = Wage)
lines(age.grid, predict(fit0, list(age = age.grid), col = "red", lwd = 2))

#fit natural cubic splines with 3 knots at 25, 40, 60 for the Wage data (wage: response, age: predictor)
#and show it on the plot.
fit = lm(wage ~ ns(age, knots = c(25, 40, 60)), data = Wage)
lines(age.grid, predict(fit, list(age = age.grid)), col = "yellow", lwd = 2)

#fit smoothing splines with 30 degree of freedom for the Wage data (Wage : responder, age: Predictor)
#and show it on the plot
fit1 = smooth.spline(age, wage, df= 1)
lines(fit1, col = "blue", lwd = 2)

fit1 = smooth.spline(age, wage, df = 10)
lines(fit1, col = "green", lwd = 2)

fit1 = smooth.spline(age, wage, df = 30) #this degree of freedom caused much more ridges, so the smaller degree of freedom was better
lines(fit1, col = "red", lwd = 2)

#fit smoothing splines using cross-validation for the Wage data (wage: responder, age: predictor)
#find the best degree of freedom and show it on the plot.

plot(age,wage,col="darkgray")
fit2 = smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit2,col="blue",lwd = 2)

#THE BELOW DOES THE SAME THING AS THE ABOVE
#fit3 = smooth.spline(age, wage, df = fit2$df)
#lines(fit3, col = "green", lwd = 2)

#Chapter 8 (tree and bagging)

set.seed(104)
ages = sample(20:70, 300, replace = T)
years = sample(1:30, 300, replace = T)
salary = sample(50:150, 300, replace=T)
tax=sample(10:50, 300, replace=T)

Product = data.frame(ages, years, salary, tax)
attach(Product)
dim(Product)
print(Product)


#create trees for salary
#what is the number of the leaves? (terminal nodes)
#How many variables were used for the tree?
tree.Product = tree(salary~., data= Product)
tree.Product
summary(tree.Product) #can see which variables were used with this summary (in this example, just ages and tax)
plot(tree.Product) #prints the tree structure
text(tree.Product, pretty = 0) #prints the values on the tree

prune1 = prune.tree(tree.Product, best = 2)
summary(prune1)
plot(prune1)
text(prune1, pretty=0)

#given code for chapter 8
set.seed(1044)
ages = sample(20:70, 300, replace = T)
years = sample(1:30, 300, replace = T)
salary = sample(50:150, 300, replace=T)
A = sample(c("Y", "N"), 300, replace = TRUE)

Product=data.frame(ages, years, salary, A=as.factor (A))
attach(Product)
dim(Product)
print(Product)

#create trees for salary
#what is the number of leaves?
#how many variables were used for the tree?

tree.Product=tree(salary~., data = Product)
tree.Product
summary(tree.Product)
plot(tree.Product)
text(tree.Product, pretty=0)

prune.car = prune.tree(tree.Product, best = 4)
plot(prune.car)
text(prune.car, pretty = 0)

#If you want A as a responder:
tree.carseats = tree(A~., data = Product)
tree.carseats
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

#To find the best value
set.seed(1011)
cv.car = cv.tree(tree.carseats)
plot(cv.car$size, cv.car$dev, type = "b")
which.min(cv.car$dev)

prune.car = prune.tree(tree.Product, best = which.min(cv.car$dev))
plot(prune.car)
text(prune.car, pretty = 0)

#given Carseats data
#train = sample(1:nrow (Carseats), 250)
#carseat has 400 observations on 11 variables

#Q8.a) Split the data set into a training set (200) and a test set(200)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]
#given so far

