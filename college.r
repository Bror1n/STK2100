#a
college <- read.csv("College.csv")
View(college)

#b 
rownames(college) <-  college[,1]
View(universities)
college <- college[,-1]
View(college)

View(college[1,])

#c i
print("----------------")
print("----------------")
print("----------------")
summary <- summary(college)
print(summary)

#c ii
View(college)
pairs(college[,2:12])

#iii
plot(as.factor(college$Private), college$Outstate,col=c("#f1b1bc","#b84b5e"))
?plot


#iv
View(college)

Elite <- rep("No",nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college,Elite)
View(college)

summary(Elite)
plot(Elite,college$Outstate)

#v
View(college)
par(mfrow =c(2,2))
?hist
hist(college$Apps,breaks=10)
hist(college$Accept,breaks=20)
hist(college$Top10perc)
hist(college$Top25perc)
#for (i in 2:6) {
#    print(i)
#    hist(as.numeric(college[i]))
#}


