#VIDEO 2: CLUSTERING PIXELS

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


#VIDEO 4: MRI IMAGE

healthy = read.csv("healthy.csv", header=FALSE)

healthyMatrix = as.matrix(healthy)
str(healthy)

image(healthyMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

#isoloate the gray matter, white matter, and cerebrospinal fluid using hierarchical clustering

healthyVector = as.vector(healthyMatrix)

#distance = dist(healthyVector, method="euclidean")
#TOO MUCH MEMORY!Use str to see what we obtain

str(healthyVector)
#365,636 elements! Holy toledo! How many pairwise distances are calculated?
#n*(n-1)/2. If n=365636, then we would calculate 67 bilion values
#bad news: can't use hierarchical clustering. how about k-means?


#VIDEO 5: K-MEANS CLUSTERING

#step 1: specify # of cluseters k. how do we choose k? depends on exactly what
#you're trying to extract from the image. for the sake of our example, let's
#choose k=5

k=5

set.seed(1)

#run k-means clustering algorithm (KMC)

KMC = kmeans(healthyVector, centers=k, iter.max=1000)

str(KMC)

#To output the segmented image, we need to extract this vector

healthyClusters = KMC$cluster

#obtain mean intensity value withint each of our 5 clusters
str(KMC)
#this info is under centers. OR if we wanna get the MIV for specific clusters:

KMC$centers[2]

#output the segmented image. first convert the vectory healthyClusters to a matrix

dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes=FALSE, col = rainbow(k))

#we can see that the k-means algorithm was able to segment the image into 5 diff clusters
#More refinement maybe  needs to be made to our clustering algorithm to appropriately capture all the anatomical structures,
#/but this seems like a good starting point


#VIDEO 6: DETECTING TUMORS


tumor = read.csv("tumor.csv", header=FALSE)

tumorMatrix = as.matrix(tumor)

tumorVector = as.vector(tumorMatrix)

#apply the k-means clustering results that we found using the healthy brain image
#/on the tumor vector. Aka, treat the healthy vector as a training set and the tumor vector as a test set

install.packages("flexclust")
library(flexclust)

#we need to convert the info from the clustering algorithm to an object of the class KCCA

KMC.kcca = as.kcca(KMC, healthyVector)

#cluster the pixels in tumorVector using the predict function

tumorClusters = predict(KMC.kcca, newdata=tumorVector)

#now tumorClusters is a vector that assigns a value 1 through 5 to each of the intensity values in the tumorVector, as predicted by the k-means algorithm

#to output, convert tumorClusters to matrix

dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes=FALSE, col=rainbow(k))

#I found the tumor.









