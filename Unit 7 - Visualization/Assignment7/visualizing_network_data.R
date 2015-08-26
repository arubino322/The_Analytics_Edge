#VISUALIZING NETWORK DATA

edges = read.csv("edges.csv")

users = read.csv("users.csv")

#what is the average number of friends per user

#Friend A is friends with B, but B is also friends with A. The pairs must be counted twice.

nrow(edges)*2/nrow(users)

table(users$school,users$locale)

table(users$gender, users$school)

#Creating a network

library(igraph)

g = graph.data.frame(edges, FALSE, users)

#Plot the graph with smaller vertices and no text labels

plot(g, vertex.size=5, vertex.label=NA)

#compute the degree of all the nodes in our graph

sort(degree(g))

#visually draw attention to the nodes with a lot of connections (a person with a lot of friends) by changing the size of the vertices

V(g)$size = degree(g)/2+2

plot(g,vertex.label=NA)

#Coloring vertices

#Let's change the color of the nodes based on gender of the user. Set the color to black for all vertices, then set it to red for the vertices with gender A and gray for vertices with gender B

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

#Now color the vertices based on schools

V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)
#WHY THE EFF ISN'T THIS WORKING

#Plot by locale

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)

#other plotting options















