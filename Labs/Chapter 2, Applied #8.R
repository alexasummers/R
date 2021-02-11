#Page 54, Applied #8

#A
college = read.csv ("College.csv")

#B
fix ( college )

rownames ( college ) = college[,1]
fix ( college )

college = college[,-1]
fix ( college )

#C(I) 
summary ( college ) 

#C(II)
college[,1] = as.numeric(factor(college[,1])) #converting Private from character to numeric
pairs ( college[,1:10] )


#C(III) 
plot(college$Private, college$Outstate)

#C(IV)
Elite = rep("No",nrow(college))
Elite[college$Top10perc>50]="YeS"
Elite=as.factor(Elite)
college=data.frame(college, Elite)
summary ( college$Elite )
plot ( college$Elite, college$Outstate)

#C(v)
par(mfrow=c(2,2))
hist(college$P.Undergrad, col=5, breaks=2)
hist(college$Top10perc, col=10)
hist(college$Terminal, breaks = 8)
hist(college$Outstate, col = 3, breaks = 2)
hist(college$Room.Board)
hist(college$Books)

#C(VI)
#Upon further investigation of the data, I have discovered that there is a more of a bell distribution of new 
#students coming from the top 25% in their high school than the top 10%. I also discovered that the majority of 
#colleges have more full-time undergrad students than part-time undergrad students.Lastly, students spend a 
#much wider range on room/board costs than they do on textbooks-- textbooks have a relatively median cost.

