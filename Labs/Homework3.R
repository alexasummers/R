library (MASS)
library (ISLR)

X = c(1,2,3,4,5,6,7,8,9,10)
Y = c(1.00, 2.00, 1.3, 3.75, 2.25, 4.5, 5.21, 4.98, 6.26, 5.4)

df = data.frame(X, Y)
print (df)

plot (Y ~ X, data = df)

plot(Y ~ X, data = df, col = c("green"))
cor(df)
fit = lm (Y ~ X, data = df)

#RSS (Sum of squared residuals)
deviance(fit)
sum(resid(fit)^2)

summary(fit)

X1 = c(1,2,3,4,5,6,7,8,9,10)
Y1 = c(1.21, 1.98, 4.76, 3.9, 6.2, 7.14, 9.35, 8.24, 10.16, 12.2)

df1 = data.frame(X1, Y1)
print (df1)

plot (Y1 ~ X1, data = df1)

plot(Y1 ~ X1, data = df1, col = c("green"))
cor(df1)
fit1 = lm (Y1 ~ X1, data = df1)

#RSS (Sum of squared residuals)
deviance(fit1)
sum(resid(fit1)^2)

summary(fit1)
