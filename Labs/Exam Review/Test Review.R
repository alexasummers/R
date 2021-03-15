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
fit = lm (X1 ~ Y+X2. data = dr)

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
