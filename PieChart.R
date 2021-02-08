#Create data for the graph
# Total of death cases caused by covid in top 5 states
deaths = c(43524, 41653,37760,26821,21866)
location = c("New York", "California", "Texas", "Florida", "Pennsylvania")

#Plot the chart
pie(deaths, labels = deaths, main = "Total number of deaths caused by covid in top five states: ",
    col = rainbow(length(x)))
legend("topright", c("New York", "California", "Texas", "Florida", "Pennsylvania"),
       fill = rainbow(length(x)))