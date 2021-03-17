library (ISLR)
library (tidyverse)
library(default)

set.seed(1)
fit.glm = glm(default ~income + balance, data = Default, family = "binomial")
summary(fit.glm)

#split validation
train = sample(dim(Default)[1], dim(Default)[1] /2)


#fit for training
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)

#prediction with test

probs = predict(fit.glm, newdata = Default[-train, ], type = "response")
summary(probs)
str(probs)
head(probs)
print(probs)
pred.glm = rep("No", length(probs)) #set no for all
pred.glm[probs > .5] = "Yes" #set yes for over .5
summary(pred.glm)

str(pred.glm)
head(pred.glm)
print(pred.glm)
glimpse(pred.glm)

#validation set err
mean(pred.glm != Default [-train, ] $default) #miss classified

#repeat
train = sample(dim(Default)[1], dim(Default)[1]/2)
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs = predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm=rep("No", length(probs))
mean(pred.glm != Default[-train, ]$default)
#test error rate can be variable, depending on observations in the training set

#Include a dummy val-- student
train = sample(dim(Default)[1], dim(Default)[1]/2)
fit.glm = glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
pred.glm = rep("No", length(probs))
probs = predict(fit.glm, newdata = Default[-train,], type = "response")
pred.glm[probs > .5] = "Yes"
mean(pred.glm != Default[-train, ] $default)
#no impact


#use bootstrap
set.seed(1)
attach(Default)

fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

boot.fn = function(data, index) {
  fit = glm(default ~ income + balance, data = data, family = "binomial", subset = index)
  return (coef(fit))
}

#find SE --> takes time for 1000 repeats
library(boot)
boot(Default, boot.fn, 1000)

#glm full set and boot results are very close

fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

attach(Boston)

#median value of owner-occupied homes in \$1000s
mu.hat = mean(medv)
mu.hat

#estimate SE
#Standard error gives us the accuracy of the estimate like how much the mean will vary if different sample was chosen
se.hat = sd(medv)/sqrt(dim(Boston)[1])
se.hat

#use bootstrap is close to

library(boot)

set.seed(1)
boot.fn = function(data, index) {
  mu = mean(data[index])
  return(mu)
}

set.seed(1)
boot(medv, boot.fn, 100)

#close to 95 CI from t-test
t.test(medv)
ci.mu = c(mu.hat-2*0.3988774,mu.hat+2*0.398874) #using standard error from the t test
ci.mu

#estimate median
med.hat = median(medv)
med.hat

#bootstrap
boot.fn = function(data, index) {
  mu = median(data[index])
  return(mu)
}

boot (medv, boot.fn, 1000)

#estimated median value is equal to the value obtained in the estimate median part above
#with a standard error which is relatively small compared to the median value

#10%
percent.hat = quantile(medv, c(0.1))
percent.hat

#bootstrap
boot.fn = function(data, index) {
  mu = quantile(data[index], c(0.1))
  return(mu)
}

boot(medv,boot.fn, 1000)

#estimated 10% value is equal to the value obtained in the percent.hat above
#with a standard error which is relatively small compared to the 10% value