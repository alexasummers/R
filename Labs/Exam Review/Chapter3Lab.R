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
#Pr(>|t|): Probability of observing any value equal or larger than t. Small P means it is unlikely we will observe a
#         relationship between the predictor and response due to chance (5% is a good cutoff point). Three * is highly significant. 
#         A small P value between the intercept and slope states we can reject null hypothesis-- declares a relationship between them.

# Residual standard error:
# Measure of the quality of a linear regression fit. RSE is the average amount that the response will deviate from the true regression line.
# RSE/mean = % of error
# Degrees of Freedom are the number of data points that went into the estimation of the parameters used after taking into account the restrictions.
# Degree of freedom-- Number of rows minus the amount of columns

# Multiple R-Squared:
# How well the model is fitting the actual data-- measure of the linear relationship between the predictor and the response (between 0 and 1).
# A R^2 closer to 0 represents a regression that does not explain the variance in the response variable well, a number closer to 1 does. 
# Basically, the R^2 percent says that percent of the variance found in the response variable can be explained by the predictor.
# The R^2 will always increase with more variables, so the R^2 adjusted is preferred because it adjusts for the number of variables considered.

# F-Static
# The further This is from 1, the better it is. When the number of data points is large, an F-static only a bit larger than 1 is enough to reject
# the null hypothesis. If there are only a few data points, a large F-static is needed.

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
fit4=update(fit3,~.-age-indus) # Update fit 3, remove age and indus
summary(fit4)

#Interaction terms



#Non-linear transformations of the predictors

#Qualitative predictors