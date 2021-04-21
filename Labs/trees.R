library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library(randomForest)
library #for the boosting algorithm

require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
dim(Carseats)

High=ifelse(Sales<=8,"No", "Yes") #add another predictor for the decision tree factoring
print(High)
Carseats = data.frame(Carseats,High)
attach(Carseats)

dim(Carseats)
head(Carseats)

Carseats$High = as.factor(Carseats$High)

tree.carseats = tree(High~.-Sales,data=Carseats) #high is the responder, sales is predictor
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats

#split 400 into (250,150) #400 different stores; 250 into testing set and 150 into training set
set.seed(2)
train=sample(1:nrow(Carseats), 250) #250 sampling
tree.carseats = tree(High~.-Sales,Carseats,subset=train) #make tree with training set
plot(tree.carseats)
text(tree.carseats,pretty=0)
Carseats.test=Carseats[-train,] #create a test set from the stuff that doesn't belong to the training set
High.test=High[-train]
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

(75+40)/150 #the amount of no/no and yes/yes added together divided by the total amount of the training set. (76.67% correct)

#cross validation with pruning (they have provided a pruned tree)
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) #display misclassification
cv.carseats
names(cv.carseats)

par(mfrow=c(1,1))
plot(cv.carseats) #missclassification is shown at the lowest dip-- that's what we make the prune part
plot(cv.carseats$size,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.carseats, best=4)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

(69+44/150)
#by pruning simpler tree with similar error rate

#can we improve the single tree issue? We might want to use
#random forest and boosting

set.seed(1) 
dim(Boston)
train1=sample(1:nrow(Boston), 300) #create training set with 300 our of 506 rows
?Boston
rf.boston=randomForest(medv~.,data=Boston,subset=train) #chooses small number of predictors
rf.boston
plot(rf.boston) #when tree size gets bigger, the error gets smaller
# MSR and % var explained are based on OOB estimates.
# 4 = the number of variables randomly chosen at each split
# predictors = 13
# variance = 84.37

oob.err = double(13)
test.err = double(13)

#mtry is number of variables randomly chosen at each split
# 4 is selected at random out of 13 predictors
for(mtry in 1:13) {
  rf=randomForest(medv ~ . , data = Boston, subset = train, mtry = mtry, ntree=400)
  oob.err[mtry] = rf$mse[400] #error of all trees fitted
  
  pred = predict(rf,Boston[-train,]) # predictions on test set for each tree
  test.err[mtry] = with(Boston[-train,], mean ((medv - pred)^2))
  #mean squared test error
  
  cat(mtry, " ")
}

test.err
oob.err
#13 times 400 trees have been grown

matplot(1:mtry, cbind(oob.err,test.err), pch = 19, col=c("red","blue",type="b",ylab="Mean Squared Error", xlab="Number of predictors considered at eash split"))
legend("topright", legend=c("Put of bag error", "Test Error"),pch = 19, col=c("red","blue"))

#13 is bagging
# 1 is a single tree
# the error tends to be minimized at around mtry = 5

set.seed(42)

boost.boston = gbm(medv ~., data = Boston[train,], distribution = "gaussian", n.trees=10000,shrinkage=0.01, interaction.depth=4)
#use distribution="bernoulli" for classification
summary(boost.boston)
#rm and lstat are the most important variables
plot(boost.boston, i="lstat")
plot(boost.boston, i="rm")

n.trees = seq(from=100, to=10000, by=100)
#number of trees-- a vector of 100 values
#generating a prediction matrix for each tree
predmatrix = predict(boost.boston,Boston[-train,],n.trees= n.trees) #create a prediction matrix
dim(predmatrix) #dimensions of the prediction matrix
#calculating the mean squared test error
test.error = with(Boston[-train,],apply((predmatrix-medv)^2,2,mean))
head(test.error) #contains the mean squared test error for each of the 100 trees averaged
#plotting the test error vs number of trees
plot(n.trees,test.error,pch=19,col="blue",xlab="number of trees", ylab="test error", main="Performance of boosting on test set")
#adding the RandomForests minimum error line trained on same data and similar parameters
abline(h = min(test.err), col="red") #test.err is the test error of a random forest
legend("topright",c("Minimum test error line for random forests"), col="red", lty=1,lwd=1)
dim(predmatrix) #shows that it has less test error
