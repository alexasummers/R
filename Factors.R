#Create two vectors as input for the factor.
directions = c("East", "West", "East", "North", "North", "East", "West", "West", "West", "East", "North")
print (directions)

gender = c("Male", "Male", "Female", "Female", "Male", "Female", "Male")
print (gender)

#Create two factors on directions and gender vectors and print levels
directionsFactor = factor(directions)
print (directionsFactor)
genderFactor = factor(gender)
print (genderFactor)

