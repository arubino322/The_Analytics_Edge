gerber = read.csv("gerber.csv")
str(gerber)

table(gerber$voting)/nrow(gerber)

tapply(gerber$voting,gerber$hawthorne,mean)
tapply(gerber$voting,gerber$civicduty,mean)
tapply(gerber$voting,gerber$neighbors,mean)
tapply(gerber$voting,gerber$self,mean)
table(gerber$voting,gerber$hawthorne)/nrow(gerber)

voted = subset(gerber, voting==1)
table(gerber$voting, c("gerber$hawthorne","gerber$civicduty","gerber$neighbors","gerber$self")










