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
library(e1071)

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

#Q8.d: bagging --> use all 10 variables (mtry = 10) (there are 11 in the dataset, so we use the mtry as minus 1)
#predict with the test dataset
#find the mean value
# what are the important variables?
set.seed(1)
bag.car = randomForest(Sales~.,data=Carseats.train, mtry = 10, importance = TRUE)
yhat.bag = predict(bag.car, newdata= Carseats.test)
mean((yhat.bag-Carseats.test$Sales)^2)
importance(bag.car) #look at the ones that are the highest
varImpPlot(bag.car) #plot graph of the highest importance ones

#Q8.e: randomforest --> use partial number of variables (mtry = 5)
#predict with the test dataset
#find the mean value
# what are the important variables?
rf.carseats = randomForest(Sales ~., data = Carseats.train, mtry = 5, ntree = 500, importance = TRUE)
yhat.rf = predict(rf.carseats, newdata = Carseats.test)
mean((yhat.rf - Carseats.test$Sales)^2)

#in some cases, randomForest does not do better than bagging
importance(rf.carseats)
varImpPlot(bag.car)

#what if we have six variables?
rf.carseats = randomForest(Sales ~., data = Carseats.train, mtry = 6, ntree = 500, importance = TRUE)
yhat.rf = predict(rf.carseats, newdata = Carseats.test)
mean((yhat.rf - Carseats.test$Sales)^2)

importance(rf.carseats)
varImpPlot(bag.car)


#Q use the following boston data

set.seed(1011)
#bagging
dim(Boston)
#train and test
set.seed(2)
train = sample(1: nrow(Boston),200)

#given so far
#This is bagging because there are 14 rows, so my mtry is 13
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
boston.test = Boston [-train, "medv"]
yhat.bag = predict (bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)
importance(bag.boston)
varImpPlot(bag.boston)

#ntree = 25
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2) #the more trees you have, the better the results. I'm using 25 here instead of just 1

set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict (rf.boston, newdata = Boston [-train, ])
mean((yhat.rf - boston.test) ^2)
varImpPlot (rf.boston)

#Q Use the following boston data

set.seed(1011)
#bagging
dim(Boston)
#train and test
set.seed(2)
train = sample(1: nrow(Boston),200)

#error of all trees fitted
oob.err = double(13)
#mean squared test error
test.err = double(13)
test2.err=double(13)
boston.test = Boston [-train, "medv"]

Boston.rf = randomForest (medv ~., data = Boston, subset = train)
Boston.rf #displays number of variables, number of trees, MSE, type of random forest
plot(Boston.rf)

#mtry is number of variables randomly chosen at each splot
for(mtry in 1:13) {
  rf = randomForest(medv ~., data = Boston, subset = train, mtry = mtry, ntree = 400)
  oob.err[mtry] = rf$mse[400] #Error of all trees fitted
  
  pred = predict(rf, Boston[-train,]) #Predictions on test set for each tree
  test.err[mtry] = with(Boston[-train,], mean((medv - pred)^2)) #mean squared test error
  
  pred2 = predict(rf, newdata = Boston[-train,]) #Predictions on test set for each tree
  test2.err[mtry] = mean((pred2 - boston.test)^2) #mean squared test error
  
  cat(mtry, " ") #Printing the output to the console
}

test.err
test2.err
oob.err

plot(test.err)
which.min(test.err)
points(which.min(test.err), test.err[which.min(test.err)], col = 'red', cex = 2, pch = 19)

plot(oob.err)
which.min(oob.err)
points(which.min(oob.err), oob.err[which.min(oob.err)], col = 'red', cex = 2, pch = 19)

#plotting both the test error and out of bag error together
matplot(1:mtry, cbind(oob.err, test.err), pch = 19, col = c("red", "blue"), type = "b", ylab = "mean squared error", xlab="number of predictors considered at each split")
legend("topright", legend=c("out of bag error", "test error"), pch = 19, col = c("red", "blue"))

# Q. For the given dataset, find the best gamma and cost parameter for SVM radial kernal
#Plot SVM
#Find the prediction accuracy

set.seed(1)
x = matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] = x[1:100, ] + 2
x[101:150,] = x[101:150, ] -2
y = c(rep(1, 150), rep(2,50))
dat = data.frame(x = x, y=as.factor(y))
train = sample(200, 100)
plot(x, col = y)
#given

svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)

#increase the value of cost, we can reduce the number of training errors
#However this comes at the price of a more irregular decision boundary

svmfit = svm(y~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])
summary(svmfit)

#given the value, what is the best cost? Gives me the best paramater at the top of the list
set.seed(1)
tune.out = tune(svm, y~., data = dat[train, ] , kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
#The best choice of parameters invloves cost = 1 and gamma = 2

#What is the accuracy?
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train, ]))
(67+21)/100 #based on the matching 1:1 and 2:2 divided by 100 total observations

#What is the best cost?
tune.out$best.parameters$cost
#what is the best gamma?
tune.out$best.parameters$gamma
#Draw svm classification plot using the best values.
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = tune.out$best.parameters$gamma, cost = tune.out$best.parameters$cost)
plot(svmfit, dat[train,])

#use a linear kernel