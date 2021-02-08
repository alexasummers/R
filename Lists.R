#To create a list containing strings, numbers, vectors and logical values
myList = list("Apple", "Google", "Microsoft", c(11, 22, 44), TRUE, 30.67, 100)
print(myList)
length(myList)

#Access the first element of the list
print(myList[1])

#Access the fourth element of the list
print(myList[4])

#Manipulating List elements
#Add element at the end of the list.
myList[7] <- "My New Element"
print(myList)

#Remove the last element
myList[7] <- NULL
print(myList)

#Update third element
myList[3] <- "My updated element"
print(myList)

