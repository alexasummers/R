library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library(caret)
library(leaps) #forward or backwards stepwise, or sequential replacement (model selection by exhaustive search (default))
#install.packages("tidyverse")
library(glmnet)

#install.packages("caret")

summary(Hitters)

Hitters=na.omit(Hitters) #Omit empty hitters, empty salary
with(Hitters,sum(is.na(Salary)))

regfit.full=regsubsets(Salary~., data=Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19) #selecting 19 predictors of the 20 variables (1 is the responder)
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary) #explanation in "Value" of ?regsubsets

?regsubsets #to see what regsubsets does

plot(reg.summary$rss,xlab="Number of variables: ", ylab="rss") #plot the Number of variables by the RSS
which.min(reg.summary$rss) #what is the variable number that will return the smallest rss (19)
points(19,reg.summary$rss[19],pch=20,col="red") #mark the variable number returning the smallest rss
coef(regfit.full,19)

plot(reg.summary$cp, xlab="Number of variables: ", ylab="cp")
which.min(reg.summary$cp) #what is the variable number that will return the smallest cp (10)
points(10,reg.summary$cp[10],pch=20,col="red")
plot(regfit.full,scale="Cp")
coef(regfit.full,10)


plot(reg.summary$bic, xlab="Number of variables: ", ylab="bic")
which.min(reg.summary$bic) #what is the variable number that will return the smallest bic (6)
points(6,reg.summary$bic[6],pch=20,col="red")
plot(regfit.full,scale="bic")
coef(regfit.full,6) #return the six predictors that need to be used

plot(reg.summary$rsq, xlab="Number of variables: ", ylab="rsq")
which.max(reg.summary$rsq) #what is the variable number that will return the smallest rsq (1)
reg.summary$rsq[19]
points(19,reg.summary$rsq[19],pch=20,col="red")
coef(regfit.full,19) #return the six predictors that need to be used

#why for rsq/adjr2 starting on line 50 did he use max instead of min?
# Because the plot started at zero and went up (2^x)

plot(reg.summary$adjr2, xlab="Number of variables: ", ylab="adjr2")
which.max(reg.summary$adjr2) #what is the variable number that will return the smallest rsq (1)
points(11,reg.summary$adjr2[11],pch=20,col="red")
plot(regfit.full,scale="adjr2")
coef(regfit.full,11) #return the six predictors that need to be used

#why did he pick these two to analyze?
reg.summary$rsq[19]
reg.summary$adjr2[11]

#regular linear model
lm.full=lm(Salary~., data=Hitters)
summary(lm.full)

lm.errs=lm(Salary~Errors, data=Hitters) #not related-- low r^2 value
summary(lm.errs)

lm.hits=lm(Salary~Hits, data=Hitters) #more significant than the above-- better r^2 value
summary(lm.hits)

#training set of 180
dim(Hitters)
set.seed(2)
train=sample(seq(263), 180, replace=FALSE)
train
lm.train=lm(Salary~., data=Hitters[train,])
summary(lm.train)

#training set of 80
dim(Hitters)
set.seed(2)
train=sample(seq(263), 80, replace=FALSE)
train
testset = Hitters[-train,]
lm.train=lm(Salary~Hits, data = Hitters[train,])
summary(lm.train)

#forward (Greedy algorithm)

regfit.fwd=regsubsets(Salary~., data = Hitters, nvmax=19, method = "forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")

dim(Hitters)
set.seed(2)
train=sample(seq(263), 180, replace = FALSE)
train
regfit.fwd = regsubsets(Salary~., data = Hitters[train,], nvmax=19, method = "forward")
val.errors = rep(NA,19)
x.test = model.matrix(Salary~., data = Hitters[-train,])

#matrix multiplication
for (i in 1:19) {
  coefi = coef (regfit.fwd, id=i)
  pred=x.test[,names(coefi)] %*%coefi
  #calculate MSE
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}

plot(sqrt(val.errors),ylab="Root MSE", ylim=c(200,440), pch=19,type="b")
which.min(sqrt(val.errors))
points(8,sqrt(val.errors[8]), pch=20,col="red")
points(sqrt(regfit.fwd$rss[-1]/180),pch=19,col="blue",type="b")


#10-fold CV

#no predict method in regsubsets, so need to make one.
predict.regsubsets = function(object, newdata, id, ...) { #prediction functionality
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

dim(Hitters)
set.seed(11)
folds=sample(rep(1:10,length=nrow(Hitters))) #makes 10 even groups
folds
table(folds)
cv.errors=matrix(NA, 10, 19)
for (k in 1:10) {
  best.fit=regsubsets(Salary~., data=Hitters[folds!=k,], nvmax = 19, method = "forward")
  for (i in 1:19) {
    pred=predict(best.fit, Hitters[folds==k,], id=i)
    cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors, 2, mean)) #looking for the lowest error (9 in this case)
plot(rmse.cv, pch=19, type="b")
which.min(rmse.cv)
points(9,rmse.cv[9],pch=20,col="red") 

#all the above is for subset selection

#lm or aov or glm to fit a linear or a generalized linear model,
#the model matrix is created from the formula and data arguments automatically.

lm.full = lm(Salary~., data=Hitters)
summary(lm.full)
model.matrix(lm.full)


#we will use pack glmnet, which does not use the model formula language,
#so we will set up on x and y
#ridge regression(alpha = 0) and the Lasso (alpha =1, default)

x=model.matrix(Salary~.-1, data=Hitters) #Separate out the predictors
x
y = Hitters$Salary #Responder
y

fit.ridge=glmnet(x,y,alpha=0)
summary(fit.ridge)
plot(fit.ridge, xvar="lambda", label=TRUE) #Predictor number displayed across the top-- doesn't change. The lambda is getting bigger. As coefficients shrink, the predictors are the same number. 

#number of variables are 20 always
#k-fold cross-validation fior glmnet, produces a plot and returns a value for lambda
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge) #range of mse. 
coef(cv.ridge) #all the predictors get closer to zero as the mse value is less. 

#lasso
fit.lasso=glmnet(x,y)
summary(fit.lasso)
#number of variables decrease (more zero coefficients)
plot(fit.lasso,xvar="lambda", label=TRUE) #default is 1, so we don't need to set the alpha
#number of coefficients is going down as lambda gets bigger

plot(fit.lasso,xavr="dev", label = TRUE)

cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)

coef(cv.lasso) #all the blank numbers are where the coefficients became zero

lasso.tr=glmnet(x[train,], y[train]) #make a training set for lasso
lasso.tr

pred=predict(lasso.tr,x[-train,])
dim(pred) #83 rows and 70 columns

#create the mean squared error (RMSE)
rmse = sqrt (apply((y[-train]-pred)^2,2,mean))
print (rmse)
summary(rmse) #we want to look at the min value and see what lambda was to get the minimum
plot(log(lasso.tr$lambda),rmse,type="b", xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
min(rmse) #reiterates the lowest value displayed in the summary
lam.best
log(lam.best)

#find the lowest value
points(log(lam.best),rmse[20],pch=20,col="red")
coef(lasso.tr,s=lam.best)
