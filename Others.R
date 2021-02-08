#use rnorm() function to create two vecotrs of random numbers,
#lets say 10 observations each
x = rnorm (10)
print (x)
y = x + rnorm (10, mean = 50, sd = .1)
print (y)

#compute the correlation between x and y and plot x versus y
cor (x, y)
plot (x, y)

#----------------------------------------
#Print mean of y
mean (y)
#print  variance of y
var (y)
#print standard deviation of y
sd (y)
