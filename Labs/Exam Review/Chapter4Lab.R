library(MASS) #lda
library(ISLR)

names(Smarket)
?Smarket
dim(Smarket)
summary(Smarket)

pairs(Smarket)


#produces a matrix that contains all of the pairwise
#Correlations among the predictors in a data set
cor(Smarket[,-9])

#attach(Smarket)
plot(Volume)

#Logistic regression

#split dataset into training and testing datast
train = (Year <2005 )
train_data = Smarket[train, ]
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]
dim(Smarket.2005)


#to fit a logistic regression model in order to predict the direction
#Using Lag 1 - 5 and volume
#Using the subset of the observations that correspond to the dates before 2005

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train )

#test the model on the testing dataset:

glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "up"
table(glm.pred, Direction.2005)
mean(glm.pred != Direction.2005)

# LDA-- classifies data based on categorical variables )making a profit or not, buying a product or not, etc.

?lda

lda.fit = lda(Direction~Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

#The plot() function produces plots of the linear discriminants obtained by computing 
# LD1 for lag1 * lag1 - lda for lag 2 * lag2 for each of the training observations

plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005) #classification errors


# quadratic discriminant analysis
?qda()
?Smarket

qda.fit = qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.pred = predict(qda.fit, Smarket.2005)

qda.class = qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) #find the accuracy of the dataset

#KNN
library(class)

?knn()
#A matrix containing the predictors associated with the training data
train.X = cbind(Lag1, Lag2)[train,] #join two predictors in one matrix
#A matrix containing the predictors associated with the testing data
test.X = cbind(Lag1, Lag2)[!train,]
#A vector containing the class labels for the training observations
train.Direction = Direction[train]

knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) #check accuracy rate
