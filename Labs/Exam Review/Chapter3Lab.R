library(MASS)

# Interpretations:

# Residuals: Difference between observed response values and the response values the model predicted
# Look for a symmetrical distribution across these points on the mean value zero (0)-- if not symmetrical,
# the model is predicting points that fall far away from the observed points

# Coefficients: 
# Estimates: First row is the estimate--expected housing value based on the average of the dataset
#           Second row is slope-- basically the effect on housing cost each of these take.
#Standard Error: Average amount that the coefficient estimates vary from the actual average of our response variable
#           Basically the standard deviation
#t-value: How many standard deviations the coefficient estimate is away from zero. The further from zero, the better
#           chance we have to reject the null hypothesis (declare a relationship between things)


#Simple linear regression

names(Boston) #gives names of variables available in the dataset
?Boston #request extra details about the data set
plot(medv~lstat,data=Boston) #plot the medium values of the houses from lstat. Left of the ~ is the dependent variable, right are independent)
fit1=lm(medv~lstat,data=Boston) #lm for Linear Model that can detect or predict the medium value of the stat
summary(fit1) #see what is saved inside fit1-- will contain residuals, coefficients, R^2 (the higher the R^2, the higher the accuracy)
abline(fit1,col="red") #add the line to our model that has been created

#Multiple linear regression

fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,data=Boston) # . for all independent variables
summary(fit3)
fit4=update(fit3,~.-age-indus)
summary(fit4)
#Interaction terms

#Non-linear transformations of the predictors

#Qualitative predictors