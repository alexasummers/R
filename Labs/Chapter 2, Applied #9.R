#9A

Auto = read.csv("Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
dim ( Auto )
summary ( Auto )

#Quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin
#Qualitative: name

