read.csv("college.csv")

fix(college) #look at the data


rownames(College) = college[,1]
fix (college)

college=college[,-1]
fix(college)

summary(college)

pairs(college[,1:10]) #scatterplot of first ten columns

boxplot(Outstate~Private, data = College)


Elite = rep("No",nrow(College))
Elite[College$Top10perc>50] = "Yes" #Binning top10perc variable to determine whether the students coming from the top 10% of their hs class exceeds 50%
college.df = data.frame(College, Elite)
summary(college.df)

boxplot(Outstate~Elite, data = College)


#using different bin sizes

par(mfrow=c(2,2))
hist(College$Apps, xlim=c(0,25000), xlab = "Applications", main = "Apps using default bin sizes")
hist(College$Apps, xlim=c(0,25000), breaks=25, xlab = "Applications",
     main = "Apps using smaller bin sizes")
hist(College$Top10perc, breaks=25, xlab = "Pct. new students from top 10% of H.S. class",
     main="Top10Perc")
hist(College$Outstate, xlab="Out-of-state tuition",ylab="Amount",main="Outstate")

