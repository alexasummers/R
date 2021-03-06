#Importing a dataset into R using read.csv() function
#Using the option header = T to tell R that the first line
#of the file contains the variable names
#Using the option na.strings tells R that any time it
#Sees a particular ? it should be treated as a missing element

Auto = read.csv ("Auto.csv", header = T, na.strings ="?")

#To view dataset in a spreadsheet-like window

fix ( Auto)

#Use dim() function to find number of observations or rows, and number of 
#Variables or columns
dim ( Auto )

#Use names() to check the variable names
names ( Auto )

#To remove rows with missing values
Auto = na.omit ( Auto )
dim ( Auto )

#Use the plot() function to produce scatterplots of the quantitative variables
plot ( Auto$cylinders , Auto$mpg )

#use the attach() function in order to tell R to make the variables in
#this data frame available by name
attach ( Auto )
plot ( cylinders, mpg )

#Use hist() function to plot a histogram of single quantitative variable
hist(mpg)

#use summary() function to produce a numerical summary of each variable
summary ( Auto )

#produce a summary of just a single variable
summary ( mpg )
