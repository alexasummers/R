#9A

Auto = read.csv("Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
dim ( Auto )
summary ( Auto )

#Quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin
#Qualitative: name

#9B

sapply(Auto[,1:8], range)

#mpg cylinders displacement horsepower weight acceleration year origin
#[1,]  9.0         3           68         46   1613          8.0   70      1
#[2,] 46.6         8          455        230   5140         24.8   82      3

#9C
sapply(Auto[, 1:8], mean)

#mpg    cylinders displacement   horsepower       weight acceleration         year       origin 
#23.445918     5.471939   194.411990   104.469388  2977.584184    15.541327    75.979592     1.576531 

sapply(Auto[, 1:8], sd)

#mpg    cylinders displacement   horsepower       weight acceleration         year       origin 
#7.8050075    1.7057832  104.6440039   38.4911599  849.4025600    2.7588641    3.6837365    0.8055182

#9D
removedAuto = Auto[-(10:85),]
sapply(removedAuto[,1:8], range)

#mpg cylinders displacement horsepower weight acceleration year origin
#[1,] 11.0         3           68         46   1649          8.5   70      1
#[2,] 46.6         8          455        230   4997         24.8   82      3

sapply(removedAuto[,1:8], mean)

#mpg    cylinders displacement   horsepower       weight acceleration         year       origin 
#24.404430     5.373418   187.240506   100.721519  2935.971519    15.726899    77.145570     1.601266 

sapply(removedAuto[,1:8], sd)

#mpg    cylinders displacement   horsepower       weight acceleration         year       origin 
#7.867283     1.654179    99.678367    35.708853   811.300208     2.693721     3.106217     0.819910 

#9E
