#Chapter 2

library(MASS)
library(ISLR)
library(tidyverse)

is.na(College) #is there any not available?
college = na.omit(College) #remove not available

?College #see something in the r package
is.na(college)
dim(college) #how many rows and collumns
dim(College)
head(College)
view(College) #can edit in this view

print(college)
str(College)
glimpse(college)

# the first col is gone in this data

rownames(college) = college$X
college$X = NULL
fix(college)

rownames(college) = Enroll$X #remove a column by specifying the column to remove
college$Enroll = NULL
fix(college)

glimpse(college)
print(college)

college = college[,-1] #removes a column at the beginning
fix(college)
glimpse(college)
print(college)
print(College)

head(college)
summary(College)
pairs(College[,1:10])
plot(Outstate ~ Private, data = College, col = c("green", "blue")) #boxplot

Elite=rep("No",nrow(college))
Elite[college$Top10perc > 50]="Yes"
Elite=as.factor(Elite) #make this a logical instead of characters
college=data.frame(college, Elite)

summary(college$Elite)
summary(college)
plot(Outstate ~Elite, data = college, col=c("green", "red")) #dependent = outstate, independent = Elite

summary(college)

par(mfrow=c(2,3))


hist(College$Top10perc, breaks = 5)

hist(College$Top10perc, breaks = 40)

hist(College$Apps, xlab = "Application received", main = "")
hist(college$perc.alumni, col = 2, xlab = "%of alumni who donate", main="")
hist(college$S.F.Ratio, col=3, breaks=10, xlab="Student/faculty ratio", main = "")
hist(college$Expend, breaks = 100, xlab = "Instructional expenditure per student", main="")

plot(college$Top10perc, college$Grad.Rate)

str(Auto)
dim(Auto)

auto = na.omit(Auto)
head(auto)
dim(auto)
summary(Auto)
levels(Auto$name) #shows the different levels
glimpse(Auto)
sum(is.na(Auto))

range_Auto = data.frame(sapply(Auto[ ,1:7], range)) #show min and max to the 7 digit values
rownames(range_Auto) = c("min:" , "max: ")
range_Auto

sapply(Auto[ .1:7], mean) #display mean
sapply(Auto[ ,1:7], sd) #display standard deviation

Auto_2 = Auto[-c(10:85)] #remove ten columns

range_Auto_2 = data.frame(sapply(Auto_2[ ,1:7], range))
rownames(range_Auto_2) = c("min: ", "Max:")
range_Auto_2

sapply(Auto_2[, 1:7], mean)
sapply(Auto_2[ ,1:7], sd) #display standard deviation

pairs(Auto[ ,1:7]) #many box plots

Auto$origin = factor(Auto$origin, labels = c("American", "European", "Japanese")) #change numeric values to have a label
fix(Auto)


labels(Auto$origin)


ggplot(Auto, aes(x = weight, y = acceleration)) + #graph the correlation
  geom_point() +
  theme(legend.position ="none") +
  scale_x_continuous(labels = scales::comma_format())
  labs(x = "Weight",
       y = "acceleration",
       title = "Correlation between weight and acceleration")

ggplot(Auto, aes(x = origin, y = mpg, fill = origin)) +
  geom_boxplot() +
  theme(legend.position = "None") +
  labs(title = "Origin vs. MPG - Boxplot",
       x = "origin",
       y = "mpg")


glimpse(Boston)
print(Boston)
sapply(Boston, class) #see data types-- there might be a factor
sapply(Boston, is.factor) #is there a factor?
all_correlations = cor(Boston) #error due to some factors that are non-numeric

all_correlations = cor(Boston[sapply(Boston, function(x) !is.factor(x))]) #create a correlation for those that are not factors

print(all_correlations[, 14])
dim(Boston)
?Boston

pairs(Boston)
summary(Boston)
glimpse(Boston)
str(Boston)
attach(Boston) #add changes to the data
pairs(Boston)

#high crime rates?
summary(Boston$crim)

sapply(Boston, class)
sapply(Boston, is.factor)
cor(Boston[,-c(3,4)]) #because there is no factor!

Boston.corr = cor(Boston[sapply(Boston, function(x) !is.factor(x))])
Boston.corr.crim = Boston.corr[-1,1]
print(
  Boston.corr.crim[order(abs(Boston.corr.crim), decreasing = T)] #absolute value sorting
)

par(mfrow = c(2,2))
#get the four most correlated variables
aux = names(Boston.corr.crim[order(abs(Boston.corr.crim), decreasing = T)] [1:4])
for(i in aux) {
  plot(get(i), crim, xlab = i)
}

summary(crim)

length(crim[crim>30])

print(crim[crim<30]) #crime rate more than 30

glimpse(crim[crim<30])

hist(tax) #whathas a tax rate higher than 500
length(tax[tax>500])

hist(ptratio)
length(ptratio[ptratio<14])

summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)

#how many suburbs are near the charles river (0 means not near, 1 means near)
table(chas)

median (ptratio)

sapply(Boston, median) #show the median

median(medv)
length(medv<median(medv))

subs.lw = which(medv<median(medv)) #find the number less than the medium value
print(subs.lw)
glimpse(subs.lw)
cor(Boston[subs.lw, ])

Boston.corr.subs.lw = cor(Boston[subs.lw, ])
corr.compare = data.frame('lower' = Boston.corr.subs.lw[ ,"medv"], 'all' = Boston.corr[ , "medv"])
corr.compare$diff = corr.compare$lower - corr.compare$all

hist(corr.compare$diff, xlab = "Correlation differences")
hist(abs(corr.compare$diff), xlab="Correlation differences")

main.diffs = head(corr.compare[order(abs(corr.compare$diff), decreasing = T), ], 5)

print (main.diffs)

hist(rm, main = "Distribution of rooms by dwelling", xlab="rooms")

summary(median(rm))

length(rm[rm>7]) #more than average room number than 7
length(rm[rm>8])

frm= as.factor(as.character(lapply(rm, function(x) ifelse(x>8, "]8, + INF [", ifelse(x>7,"]7 , 8]","[0,7")))))
plot(frm, medv, varwidth=T, xlab="Number ofe Rooms",
     ylab = "Median values by $1000s",
     title="Medial value of Owner-Occupied homes")

Boston[rm>8 & medv<30, ]#looking for outliers
Boston[rm>8, ]
glimpse(Boston[rm>8, ])


#Chapter 3

data(Auto)
fit = lm(mpg ~horsepower, data = Auto) ##responder = mpg, predictor = horsepower
summary(fit)

# a generic function for predictions from the results of various model fitting functions
predict(fit, data.frame(horsepower = 98), interval = "confidence")

#a range of values that is likely to contain the value of a single new obervation given
predict(fit, data.frame(horsepower = 98), interval = "prediction")

plot(Auto$horsepower, Auto$mpg, xlab="Horsepower", ylab = "Miles per gallon")
abline(fit, lwd = 3, col = "red") #linear regression line

par(mfrow = c(2,2)) #residuals vs. fitted and scale location should be flat. residuals vs. leverage should be clumpy. Normal qq should be close to the line
plot(fit)

pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

print(Auto$origin)

sapply(Auto, class)
cor(Auto[,-c(8,9)])
glimpse(Auto)

mpg.fit = lm(mpg ~ . - name, data = Auto) #all variables are predictors except the name
summary(mpg.fit)

par(mfrow = c(2,2)) #residuals vs. fitted and scale location should be flat. residuals vs. leverage should be clumpy. Normal qq should be close to the line
plot(mpg.fit)

#from the correlation matrix, we obtained the two highest correlated pairs
fit3 = lm(mpg ~ cylinders*displacement+displacement*weight, data = Auto[, 1:8]) #individuals and interactions
summary(fit3)

fit13 = lm(mpg ~ cylinders * displacement, data = Auto[, 1:8])
summary (fit13)

fit14 = lm(mpg ~ cylinders + cylinders : displacement, data = Auto[, 1:8]) #colon is for just two different ones
summary(fit14)

fit22 = lm(mpg ~ cylinders : displacement, data = Auto[, 1:8]) 
summary(fit22)

names(Auto)
fit15 = lm(mpg ~ poly(horsepower, 5), data = Auto[, 1:8]) #different polynomials-- see what it is significant to and then stop once it becomse less significant
summary(fit15)

par(mfrow = c(2,2)) #cooks distance means there is no leverage. Concentration means good. 
plot (fit15)

ggplot(Auto, aes(horsepower, mpg)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(X, 5, raw = TRUE))

#from the pvalues, we can see that the interaction between displacement and weight is significant
#while the interaction between cylinders and displacement is not

par(mfrow = c(2,3))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
plot (Auto$horsepower, Auto$mpg)

data(Carseats)
glimpse(Carseats)
sapply(Carseats, class)
fit4 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit4)

fit5 = lm(Sales ~ Price + US, data = Carseats)
summary(fit5)

fit15 = lm(Sales ~ Price + Urban, data = Carseats)
summary(fit15)

fit25 = lm(Sales ~ Price,data = Carseats)
summary(fit25)

fit35 = lm(Sales ~ poly(Price,5), data = Carseats)
summary(fit35)

plot(Carseats$Price, Carseats$Sales)
abline(fit25, lwd = 3, col = "red")

confint(fit25)

par(mfrow = c(2,2)) #normal distribution upper right
plot(fit5)
