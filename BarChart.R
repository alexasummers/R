#Create data for the graph
# Total of death cases caused by covid in top 5 states
deaths = c(43524, 41653,37760,26821,21866)
location = c("New York", "California", "Texas", "Florida", "Pennsylvania")

#Plot the chart
barplot(deaths, names.arg = location, xlab = "location", ylab= "Cases",
    main = "Total number of deaths caused by covid in top five states: ",
    col = rainbow(length(x)))

