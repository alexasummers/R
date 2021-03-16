#name and ID
#create data sets

#If there is no relationship, we accept the null hypothesis!!
# Responder ~ Predictor

#KNN questions
library(MASS)
library(ISLR) #install.packages("ISLR")
library(class)
library(tidyverse)
library(boot)
library(Rcpp)
#install.packages("tidyverse")


X1 = c(0,2,0,0,-1,1,0,1)
X2 = c(3,0,1,1,0,1,0,-2)
X3 = c(0,0,3,2,1,1,0,1)
Y = c(0,0,0,1,1,0,0,0)
X4 = c(0,0,0,0,0,1,1,1)

dr = data.frame(X1, X2, X3, Y, X4)

print(df)
glimpse(df)

#loading data

head(df)

#Linear regressions P = X1, R = Y
plot(Y ~ X1, data = dr, col = c("green"))
fit = lm(Y ~ X1, data = dr)

summary(fit)
abline(fit, lwd=3, col="darkgreen")

fit = lm(Y ~ X1+X2+X3, data = dr)
summary (fit)

fit = lm(Y ~ ., data=dr)
summary(fit)

fit = lm(X4 ~ X1*X2, data = dr)
summary(fit)

# Make a linear regression using X1 as the responder and Y as the predictor
fit = lm(X1 ~ Y, data= dr)

#Make a linear regression using X1 as a responder and Y and X2 as a predictor
fit = lm (X1 ~ Y+X2, data = dr)

#Write the linear model
# Responder = Intercept estimate + variable2Estimate * variable2 + variable3Estimate * variable3...

#What is the coefficient of variable1?
# the estimate value for that variable

#sum of squared residuals (RSS)
deviance(fit)
sum(resid(fit)^2)
cor(dr) #If subtraction is needed: cor(subset(dr, select = -Y))

#Separate train and test sets
train = (dr$X4 < 1) #making the training set out of X4 and anything less than 1
train_set = dr[train, ]
test_set = dr[!train, ]

head(test_set)

#KNN K = 1 -- If K number is bigger, it is less flexible (low bias, high variance). Lower number is most flexible (high bias, low variance). 
set.seed(1)

#train.X = cbind(train_set$X1) #only one predictor
#test.x = cbind(test_set$X1)
train.X = cbind(train_set[,-3])
test.X = cbind(test_set[,-3])
train.Y = cbind(train_set$Y)
knn.pred = knn (train.X, test.X, train.Y, k = 3)
table(knn.pred, test_set$Y) #check to see if the test set works with the Y prediction

summary(knn.pred)

set.seed(2)

train.X = cbind(train_set$X1) #only one predictor
test.X = cbind(test_set$X1)
#train.X = cbind(train_set[,-3]) #instead of using all 3 predictors, I'm just using X1
#test.X = cbind(test_set[,-3])
train.Y = cbind(train_set$Y)
knn.pred = knn (train.X, test.X, train.Y, k = 3)
table(knn.pred, test_set$Y) #check to see if the test set works with the Y prediction

summary(knn.pred)

#new dataset
#create a new dataset
X1 = c(100, 11, 115, 112, 20, 40, 30, 300)
X2 = c(0.23, 0.1, 0.21, 1, 0, 1, 0, -2)
X3 = c(0,0,3,2,1,1,0,1)
Y = c("NO","NO","NO", "YES", "YES","NO","NO","YES")
X4 = c(0,0,0,0,0,1,1,1)

Y = factor(Y)
df = data.frame(X1, X2, X3, Y, X4)

update(df - Y)
print(df)
glimpse(df)

cor(df)

plot(X2~X1, data=df)

glm.fit = glm(X2~X1, data = df)
coef(glm.fit)

summary(glm.fit)

#draw errors for each degree up to 5
set.seed(1)
train = sample(8, 5)
print(train)

set.seed(2)
train = sample(8,5)
print(train)

#lm or glm, doesn't matter
glm.fit = glm(X2~X1, data=df, subset = train) #X2 is responder, X1 is predictor-- in the trianing set I just created
#Attach(Auto)
#Predict () to estimate the response for all 8
#mean() to calculate MSE of 5 validation set
#select not train set [-train]
head(df)
mean((X2 - predict(glm.fit, df))[-train]^2)

#to calculate the best degree
gen.error = rep(0,3)
degree=1:3
for(d in degree){
  glm.fit=glm(X2~poly(X1,d), ,data=df, subset = train)
  gen.error[d] = mean((X2-predict(glm.fit, df))[-train]^2)
}
print(gen.error)
plot(degree, gen.error, type="b")

set.seed(2)
train = sample(8,5)

gen.error = rep(0,3)
degree=1:3
for(d in degree){
  glm.fit=glm(X2~poly(X1,d), , data=df, subset = train)
  gen.error[d]=mean((X2-predict(glm.fit, df))[-train]^2)
}
print(gen.error)
lines(degree, gen.error, type="b", col="red")

set.seed(3)
train = sample(8,5)

gen.error = rep(0,3)
degree=1:3
for(d in degree){
  glm.fit=glm(X2~poly(X1,d), , data=df, subset = train)
  gen.error[d]=mean((X2-predict(glm.fit, df))[-train]^2)
}
print(gen.error)
lines(degree, gen.error, type="b", col="green")

#plots will not show up if the gen.error print is higher than the other one

set.seed(4)
train = sample(8,5)

gen.error = rep(0,3)
degree=1:3
for(d in degree){
  glm.fit=glm(X2~poly(X1,d), , data=df, subset = train)
  gen.error[d]=mean((X2-predict(glm.fit, df))[-train]^2)
}
print(gen.error)
lines(degree, gen.error, type="b", col="blue")

#Simple MSE example without any training separation
#LOOCV <---takes time
attach(df)
#fit s linear model
model = glm(X2~X1, data = df)
MSE_LOOCV = cv.glm(df,model)
##formula 5.1 and 5.2-- choose 5.1
MSE_LOOCV$delta[1]

MSE_LOOCV = NULL
for (i in 1:4) {
  model = glm(X2~poly(X1, i), data=df)
  MSE_LOOCV[i] = cv.glm(df,model)$delta[1]
}
MSE_LOOCV

plot(MSE_LOOCV, type="b")

#k-fold CV

MSE_10_cv = NULL
for(i in 1:4) {
  model = glm(X2~poly(X1, i), data=df)
  MSE_10_cv[i] = cv.glm(df, model, K = 4)$delta[1]
}
MSE_10_cv

lines(MSE_10_cv, type = "b", col="red")

MSE_10_cv = NULL
for(i in 1:3) {
  model = glm(X2~poly(X1, i), data=df)
  MSE_10_cv[i] = cv.glm(df, model, K = 2)$delta[1]
}
MSE_10_cv

lines(MSE_10_cv, type = "b", col="red")

MSE_10_cv = NULL #with something like this (a linear increase from 1 to 2 index along  the x axis, the best degree is 1)
for(i in 1:2) {
  model = glm(X2~poly(X1, i), data=df)
  MSE_10_cv[i] = cv.glm(df, model, K = 2)$delta[1]
}
MSE_10_cv

plot(MSE_10_cv, type = "b", col="red")




logmod = glm(Y~ X1+X2+X3, family = binomial, data = df)
summary(logmod)

#mistakes made by logistic regression
probs = predict(logmod, type="response")
print(probs)
preds = rep("NO", 8)
preds[probs > 0.7] = "YES"
table(preds, df$Y) #the error rate is going to be the mismatched ones

hist(probs, breaks = 100, col="darkred")
abline(v=mean(probs), lwd=2)

plot(probs, col=ifelse(df$Y=="YES", "red", "green"), pch=16)
abline(h=0.7, lwd=3)

#------------------------------------------------------------------------------------------------------------------------

train = (df$X4 < 1)
training.data = df[train, ]
test.data = df[!train, ]

head(test.data)

simpglm = glm(Y~X1+X2+X3, family = binomial, subset = train)

testprobs = predict(simpglm, type="response")
print(testprobs)
testdirs = df$Y[df$X4 > 0]
plot(testprobs, col = ifelse(df$Y[df$X4 > 0] =="YES", "red", "green"), pch=16)
abline(h=.05, lwd=3)

testpreds = rep("NO", 3)
testpreds[testprobs>0.5]="YES"
mean(testprobs)

table(testpreds, testdirs)

testpreds = rep("NO", 3)
testpreds[testprobs>0.3] = "YES"
mean(testprobs)

#LDA

lda.fit = lda(Y ~X1+X2+X3, data = training.data)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
table(lda.class, test.data$Y)



