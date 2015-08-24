#VIDEO 2: CLUSTING PIXELS

flower = read.csv("flower.csv", header=FALSE)
str(flower)

#change data type to matrix

flowerMatrix = as.matrix(flower)
str(flowerMatrix)

#we DON'T wanna be influenced by how the image looks like in our decision of the # of clusters we want to pick

#convert matrix of pixel intensities to a vector that contains all the intensity values

flowerVector = as.vector(flowerMatrix)
str(flowerVector)

#now start hierarchical clustering. The first step is to create the distance matrix, which computes the difference b/t every 2 intensity values in our flower vector

distance = dist(flowerVector, method = "euclidean")


#VIDEO 3: HIERARCHICAL CLUSTERING

clusterIntensity = hclust(distance, method = "ward")

#reminder: Ward's method is a min var method, which tries to find compact and spherical cluseters.

#plot the dendrogram

plot(clusterIntensity)

#it seems that having 2-3 clusters is reasonable in our case, since the partition has the most room to move vertically towards the top of the dendrogram

#we can visualize the cuts by plotting rectangles around the clusters on this tree

rect.hclust(clusterIntensity, k=3, border="red")

#split the data into these 3 clusters

flowerClusters = cutree(clusterIntensity, k=3)

flowerClusters
#we see that flowerClusters is actually a vector that assigns each intesnsity value in the flower vector to a cluseter (values 1,2, or 3)

#find mean intensity value of each of our cluseters

tapply(flowerVector, flowerClusters, mean)

#Let's see how the image was segmented. 0 = darkest, 1 = fairest
#First, convert it back to a matrix

dim(flowerClusters) = c(50,50)

#view flowerCluster

image(flowerClusters, axes=FALSE)

#Let's see how the original image looked like

image(flowerMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))


