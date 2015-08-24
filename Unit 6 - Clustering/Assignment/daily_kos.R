kos = read.csv("dailykos.csv")

str(kos)

distKos = dist(kos, method="euclidean")

kosHierClust = hclust(distKos, method="ward.D")

#plot dendrogram

plot(kosHierClust)

#use cutree to split data into 7 clusters

kosClusters = cutree(kosHierClust, k=7)

cluster1 = subset(kos, kosClusters == 1)
cluster2 = subset(kos, kosClusters == 2)
cluster3 = subset(kos, kosClusters == 3)
cluster4 = subset(kos, kosClusters == 4)
cluster5 = subset(kos, kosClusters == 5)
cluster6 = subset(kos, kosClusters == 6)
cluster7 = subset(kos, kosClusters == 7)

table(kosClusters)

#look at top 6 words in each cluster

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

#k-means

set.seed(1000)

kosKMC = kmeans(kos, centers=7)

KmeansCluster1 = subset(kos, kosKMC$cluster == 1)
KmeansCluster2 = subset(kos, kosKMC$cluster == 2)
KmeansCluster3 = subset(kos, kosKMC$cluster == 3)
KmeansCluster4 = subset(kos, kosKMC$cluster == 4)
KmeansCluster5 = subset(kos, kosKMC$cluster == 5)
KmeansCluster6 = subset(kos, kosKMC$cluster == 6)
KmeansCluster7 = subset(kos, kosKMC$cluster == 7)

tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

#compare hierarchical cluster with k-means cluster

table(kosClusters, kosKMC$cluster)



