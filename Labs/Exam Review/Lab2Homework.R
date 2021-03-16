library(ISLR)
library(MASS)
library(class)
library(tidyverse)

summary(Weekly)
plot(Today ~ Lag1, col="darkred", data= Weekly)
simplelm = lm(Today~Lag1, data=Weekly)
abline(simplelm, lwd=3, col="darkgreen")
pairs(Weekly)
cor(subset(Weekly, select = -Direction))
#year and volume appear to have a relationship. No other patterns are discernible.

logmod = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = "binomial", data=Weekly) #glm is logistic regression
summary(logmod)
#lag2 appears to have some statistical; significance with a pvalue of less than .05

#Let's make a new function
#Show confusion matrix from preicted class and observed class
show_model_performance = function(predicted_status, observed_status) {
  confusion_matrix = table(predicted_status,
                           observed_status,
                           dnn=c("Predicted Status", "Observed Status"))
  
  print(confusion_matrix)
  
  error_rate = mean(predicted_status != observed_status)
  
  cat("\n") #prints new line
  cat("Error rate:", 100* error_rate, "%\n")
  cat("Correctly predicted:", 100 * (1-error_rate), "%\n")
  cat("False positive rate:", 100 * confusion_matrix[2,1] / sum(confusion_matrix[,1]), "%\n")
  cat("False negative rate:", 100 * confusion_matrix[1,2] / sum(confusion_matrix[,2]), "%\n")
}

#get prediction as Up/Down direction - only needed for GLM models
predict_glm_direction = function(model, newdata = NULL) {
  predictions = predict(model, newdata, type="response")
  return(as.factor(ifelse(predictions<0.5,"Down", "Up")))
}

#mistakes by logistic regression
predicted_direction = predict_glm_direction(logmod)

show_model_performance(predicted_direction,Weekly$Direction)

#mistakes made by logistic regression
probs = predict(logmod, type="response")
preds = rep("Down", 1089)
preds[probs > 0.5] = "Up"
table(preds, Weekly$Direction)
#.5 is too much
hist(probs, breaks = 100, col= "darkred")
abline(v = mean(probs), lwd = 2)

plot(probs, col = ifelse(Weekly$Direction=="Down", "red", "green"), pch=16)
abline(h=0.5, lwd = 3)

#separate data into training set and testing set
training.data = Weekly[Weekly$Year<2009,]
test.data = Weekly[Weekly$Year>2008,]
simpglm = glm(Direction~Lag2, data=training.data, family = "binomial")
summary(simpglm)

#other ways

train = (Weekly$Year < 2009)
train_set = Weekly[train, ]
test_set = Weekly[!train, ]

logit_model = glm(Direction~Lag2, data = Weekly, family = binomial, subset = train)

predicted_direction = predict_glm_direction(logmod, test_set)
show_model_performance(predicted_direction, test_set$Direction)

test_probs = predict(simpglm, type="response", newdata = test.data)
testdirs = Weekly$Direction[Weekly$Year>2008]
plot(testprobs, col=ifelse(Weekly$Direction[Weekly$Year>2008]=="Down", "red", "green"), pch=16)
abline(h=0.5, lwd=3)

testpreds = rep("Down", 104)
testpreds[testprobs>.5] = "Up"
mean(probs)
#error rate 37.5%
table(testpreds, testdirs)

#LDA

lda.fit = lda(Direction~Lag2, data= training.data)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
table(lda.class, test.data$Direction)

#other way
lda_model = lda(Direction~Lag2, data = Weekly, subset = train)

predictions = predict(lda_model, test_set, type="response")
show_model_performance(predictions$class,test_set$Direction)

#QDA

qda.fit = qda(Direction~Lag2, data = training.data)
qda.fit

qda.pred = predict(qda.fit, newdata = test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$Direction)

qda_model = qda(Direction~Lag2, data = Weekly, subset=train)

predictions = predict(qda_model, test_set, type = "response")
show_model_performance(predictions$class, test_set$Direction)


#KNN
set.seed(1)

train.X = cbind(training.data$Lag2)
test.X = cbind(test.data$Lag2)
train.Y = cbind(training.data$Direction)
knn.pred = knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.data$Direction)

#KNN K = 3

knn3.pred = knn(train.X, test.X, train.Y, k=3)
table(knn3.pred, test.data$Direction)

#other way
library(class)

run_knn = function(train, test, train_class, test_class, k) {
  set.seed(12345)
  predictions = knn(train, test, train_class, k)
  
  cat("KNN: k = ", k,"\n")
  show_model_performance(predictions, test_class)
}

train_matrix = as.matrix(train_set$Lag2)
test_matrix = as.matrix(test_set$Lag2)

run_knn(train_matrix,test_matrix, train_set$Direction,test_set$Direction, k=1)

#multiple predictors
qda.fit2 =qda(Direction~Lag1+Lag2+Lag4, data = training.data)
qda.fit2

qda.pred2 = predict(qda.fit2, newdata=test.data,type="response")
qda.class2=qda.pred2$class
table(qda.class2, test.data$Direction)

lda.fit2 = lda(Direction~Lag1 + Lag2 + Lag4, data=training.data)
lda.fit2

lda.pred2 = predict(lda.fit2, newdata=test.data, type="response")
lda.class2 = lda.pred2$class
table(lda.class2, test.data$Direction)

#other combo lag1 * lag2

logit_model =glm(Direction~Lag1*Lag2, data = Weekly, family=binomial, subset=train)

predicted_direction = predict_glm_direction(logit_model, test_set)
show_model_performance(predicted_direction,test_set$Direction)

#LDA with lag1*lag2 intersection
lda_model = lda(Direction ~Lag1 * Lag2, data = Weekly, subset = train)
predictions = predict(lda_model, test_set, type="responsive")
show_model_performance(predictions$class, test_set$Direction)

#QDA with sqrt(abs(Lag2))
qda_model = qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset=train)
predictions = predict(qda_model, test_set,type="response")
show_model_performance(predictions$class, test_set$Direction)

#KNN k =10
run_knn(train_matrix, test_matrix, train_set$Direction, test_set$Direction, k = 10)

#KNN k =100
run_knn(train_matrix, test_matrix, train_set$Direction, test_set$Direction, k = 100)

#Lab 11

Auto$mpg01 = Auto$mpg > median(Auto$mpg)
head(Auto$mpg01, n=20)
summary(Auto$mpg01)

print(Auto$mpg01)
str(Auto$mpg01)
glimpse(Auto$mpg01)

auto = Auto
auto$mpg01 = rep(0, length(auto$mpg))
auto$mpg01 [ auto$mpg>median(auto$mpg)] = 1
head(auto)
print(auto$mpg01)

cor(subset(Auto, select = -name))
#cylinders, displacement, weight, horsepower and mpg itself seem most likely to be useful in predicting mpg01

par(mfrow=c(2,2))
plot(auto$year, auto$acceleration, col = ifelse(auto$mpg01==0, "red", "green"), pch=16)
plot(auto$year, auto$weight, col = ifelse(auto$mpg01==0, "red", "green"), pch=16)
plot(auto$year, auto$horsepower, col = ifelse(auto$mpg01==0, "red", "green"), pch=16)
plot(auto$year, auto$displacement, col = ifelse(auto$mpg01==0, "red", "green"), pch=16)

#split data
train = sample(nrow(Auto) *0.7)
train_set = Auto[train,]
test_set = Auto[!train,]

#another split
idxs = sample(1:dim(auto)[1],size=dim(auto)[1]*0.75)
training = auto[idxs,]
test = auto[-idxs,]

# LDA

lda_model = lda(mpg01~ cylinders+weight+displacement + horsepower, data = Auto, subset = train)
plot(lda_model)

predictions = predict(lda_model, test_set)
show_model_performance(predictions$class, test_set$mpg01)

#QDA

qda_model = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
predictions = predict(qda_model, test_set)
show_model_performance(predictions$class, test_set$mpg01)

#GLM

logit_model = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family= binomial, subset = train)
summary(logit_model)

predictions = predict(logit_model, test_set, type = "response")

show_model_performance(predictions > 0.5, test_set$mpg01)

par(mfrow=c(1,1))
plot (probs, col = ifelse(test_set$mpg01==0, "red", "blue"), pch = 16)
abline(h=.5, lwd=3)

