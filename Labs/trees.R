library(MASS)
library(ISLR)
library(class)
library(tidyverse)

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
train=sample(1:nrow(Carseats), 250)
tree.carseats = tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

(75+40)/150
set.seed(3)