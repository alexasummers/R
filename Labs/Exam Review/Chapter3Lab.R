library(MASS)

#Simple linear regression

names(Boston) #gives names of variables available in the dataset
?Boston #request extra details about the data set
plot(medv~lstat,data=Boston) #plot the medium values of the houses from lstat
fit1=lm(medv~lstat,data=Boston) #lm for Linear Model that can detect or predict the medium value of the stat
summary(fit1) #see what is saved inside fit1-- will contain residuals, coefficients, R^2 (the higher the R^2, the higher the accuracy)
abline(fit1,col="red") #add the line to our model that has been created

#Multiple linear regression

#Interaction terms

#Non-linear transformations of the predictors

#Qualitative predictors

library(MASS)