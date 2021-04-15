library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library (splines)
library(gam)
#install.packages("gam")

#wage and other data for a group of 3000 male workers in the midadlantic region
#with 11 variables

?Wage
attach(Wage)
summary(Wage)
lm(wage~year+age,data=Wage)
str(Wage)
plot(Wage)
#polynomials focusing on single predictor age
fit=lm(wage~poly(age,4),data=Wage) #degree 4 polynomial-- find the significance
summary(fit)
coef(summary(fit))
#first two are very significant
#third is somewhat important
#4th is not-- cubic plot may be sufficient

#minimum and maximum values of age variable
agelims = range(age)
print(agelims)
print(agelims[1])
print(age)
age.grid= seq(from = agelims[1], to = agelims[2]) #put everything in order
print (age.grid)

#se=TRUE returns standard errors
preds=predict(fit, newdata = list(age = age.grid), se = TRUE)
#confidence intervals as estimate + and - 2 standard deviations
#an upper and lower bound of our estimate of y
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

#set margins to plot title in margins
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot (wage ~ jitter(age,2), xlim = agelims, cex = 0.5, col = "darkgrey", bty = "n", xlab = "age")

#overall plot window title
title("Degree -4 polynomial", outer = TRUE)
#line for mean estimate
lines(age.grid, preds$fit, lwd = 2, col = "blue")
#~95% confidence interval
matlines(age.grid, se.bands, lwd = 2, col = "red", lty =3)


fita = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
coef (fita)
#perfect straight line
plot(fitted(fit), fitted(fita))

fit.a = lm(wage ~education, data = Wage)
fit.b = lm(wage~education + age, data = Wage)
fit.c = lm(wage~education + poly(age,2), data = Wage)
fit.d = lm(wage~education + poly(age,3), data=Wage)

#variance (or deviance) tables for fitted model objects
anova(fit.a, fit.b, fit.c, fit.d)

#fit a polynomial logistic regression
#create a binary variable that is 1 if wage > 250 and 0 otherwise
fit = glm(I(wage > 250) ~poly(age,4), data = Wage, family=binomial)

summary(fit)
#predict latent y
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)

#send latent y through the link function
pfit = 1 / (1 + exp(-preds$fit))

#error bands calcuate on the latent y
#inverse logit mapping
se.bands.logit = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands = 1 / (1 + exp(-se.bands.logit))

plot(I(wage > 250) ~ age, xlim = agelims, type = "n", ylim = c(0, 0.2))
#add data to the plot
points(jitter(age), I((wage > 250)/5), cex = 1, pch = "|", col = "darkgrey")
#mean estimate
lines(age.grid, pfit, lwd = 2, col = "blue")
#95 confidence interval
matlines(age.grid, se.bands, lwd=2, col = "red", lty = 3)

#splines
#cubic splines


fitx = lm(wage ~ poly(age, df = 14), data = Wage) #line for the global polynomial one

fit0 = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) #cubic spline with 3 different knots
plot(age,wage,col="darkgrey")

lines(age.grid, predict(fitx, list(age = age.grid)), col="blue", lwd = 2)
lines(age.grid,predict(fit0,list(age = age.grid)), col = "green", lwd = 2)

fit = lm(wage ~ ns(age, knots = c(25, 40, 60)), data = Wage) #natural spline
summary(fit)

lines(age.grid,predict(fit, list(age = age.grid)), col = "red", lwd = 2)
abline(v=c(25, 40, 60), lty = 2, col = "yellow") #marking the knots

fit = lm(wage ~ bs(age, knots = c( 25, 40, 60)), data = Wage)
summary(fit)
plot(age,wage,col="dark grey")
lines(age.grid,predict(fit,list(age = age.grid)), col = "green", lwd = 2)
abline(v=c(25, 40, 60), lty = 2, col = "yellow")


#smoothing spline

fit1 = smooth.spline(age,wage,df=16) #degree of freedom value
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df #this is how we get the 6.8 on line 121

lines(fit1, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
title("Smoothing spline")
legend ("topright", legend = c("Cubic", "16 DF ", "6.8 DF"), col = c("green","red","blue"), lty = 1, lwd = 2, cex=.8)

#GAM

#s() is part of the gam library for a smoothing spline
#specify that the function of year should have 4 degrees of freedom,
#and that the function of age will have 4 degrees of freedom

gam1 = gam(wage ~s(year, df=4)+ s(age,df=4) + education, data=Wage) #smoothing spline for year and age and nothing for education
par(mfrow = c(1,3))
plot(gam1, se=TRUE, col = "blue")


gam.lr = gam(I(wage > 250) ~ s(age,df=4) + year + education, family = binomial, data = Wage)
par(mfrow = c(1,3))
plot(gam.lr)


gamx = gam(I(wage > 250) ~ s(age, df=4)+year+education, family = binomial, data=Wage)
anova(gamx,gam.lr,test="Chisq")

par(mfrow = c(1,3))
lm1=lm(wage~ns(year,4) + ns(age,5) + education, data=Wage)

plot.Gam(lm1, se=T, col="Green")
