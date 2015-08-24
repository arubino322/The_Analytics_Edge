#VIDEO 6

movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
summary(movies)

#Add in column names yourself

colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

#let's remove the variables that we won't be using

movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

#there are a few duplicate entries that we want to remove too

movies = unique(movies)
str(movies)


#VIDEO 7 HIERARCHICAL CLUSTERING

#step 1: compute the distances b/t all data points
#step 2: cluster the points
#compute the distances on the genre variable, no the title variable

distances = dist(movies[2:20], method="euclidean")

#cluster

clusterMovies = hclust(distances, method="ward")

#ward method cares about the distance b/t clusters using centroid distance, and also the variance in each of the clusters

#plot the dendrogram

plot(clusterMovies)

#looking at the dendrogram, 2-4 clusters would be a good choice, but let's keep in mind our application too. We probably want more than 2-4 clusters of movies to make recommendations to users. There's a spot towards the bottom where it looks like there are about 10 clusters. This is probably better for our application.

#label each of the data points according to what cluster it belongs to

clusterGroups = cutree(clusterMovies, k=10)

#where k = number of clusters

#use tapply to compute percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)

#this divides our data points into the 10 clusters, and then computes the average value of the action variable for each cluster. by computing the average of this binary variable, we're computing the percentage of movies in that cluster that belong in that genre.

#now try romance

tapply(movies$Romance, clusterGroups, mean)

#Let's figure out what cluster Men in Black is in

subset(movies, Title=="Men in Black (1997)")

#MIB is the 257th row in our data. which cluster did row 257 go into?

clusterGroups[257]

#cluster 2, the action/adventure/sci-fi cluster

#create a dataset with only movies from cluster 2

cluster2 = subset(movies, clusterGroups==2)

#let's look at the first 10 titles in this cluster

cluster2$Title[1:10]

#exercise

clusterGroups2 = cutree(clusterMovies, k=2)

tapply(movies$Drama, clusterGroups2, mean)



