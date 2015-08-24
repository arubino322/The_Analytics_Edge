airlines = read.csv("AirlinesCluster.csv")

#normalize data

library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("quantreg")
library(quantreg)
install.packages("caret")
library(caret)


preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)

#compute distance

distAirline = dist(airlinesNorm, method="euclidean")

#run hierarchical clustering algorithm

airHier = hclust(distAirline, method="ward.D")

#plot dendrogram

plot(airHier)

#split the data into 5 clustesrs

airClusters = cutree(airHier, k=5)

table(airClusters)

#use tapply to compare average values of each variable for 5 clusters
tapply(airlines$Balance, airClusters, mean)
tapply(airlines$QualMiles, airClusters, mean)
tapply(airlines$BonusMiles, airClusters, mean)
tapply(airlines$BonusTrans, airClusters, mean)
tapply(airlines$FlightMiles, airClusters, mean)
tapply(airlines$FlightTrans, airClusters, mean)
tapply(airlines$DaysSinceEnroll, airClusters, mean)

#k-means clustering

set.seed(88)

airKMC = kmeans(airlinesNorm, centers=5, iter.max=1000)

table(airKMC$cluster)







