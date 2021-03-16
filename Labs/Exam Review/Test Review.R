#name and ID
#create data sets

#If there is no relationship, we accept the null hypothesis!!
# Responder ~ Predictor

#KNN questions
library(MASS)
library(ISLR)
library(class)
library(tidyverse)
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

