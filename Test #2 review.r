#Libraries

library(MASS)
library(ISLR)
library(class)
library(tidyverse)
library(leaps)

#Stepwise selection(forward and backward)
#For the given data, use the regsubsets() function to perform forward stepwise
#or backward stepwise selection, using the argument method= "forward" or "backward"

#With the given data

set.seed(1)
x = rnorm(100)
#y =rnorm(100)

