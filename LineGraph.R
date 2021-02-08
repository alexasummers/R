#Create the data for the chart
rainFall2019 = c(7,12,28,3,41,33,85,12,15,23,18,65)

#Give the chart file a name
# Plot the bar chart with single line
plot(rainFall2019,type = "o", col = "red",
     xlab = "Month", ylab = "Rain Fall",
     main = "Rain Fall Chart 2019")

rainFall2020 = c(3,12,18,9,52,26,93,22,26,17,4,25)

#Plot the bar chart with multiple lines
plot(rainFall2019,type="o", col= "red", xlab = "Month", ylab = "Rain Fall",
     main = "Rain Fall Chart 2020")

lines(rainFall2020, type = "o", col = "blue")

