#VIDEO 4: BASIC SCATTERPLOTS USING GGPLOT

WHO = read.csv("WHO.csv")

#plot Gross National Income to Fertility Rate, using Base R then ggplot

plot(WHO$GNI, WHO$FertilityRate)

library(ggplot2)

#First, let's create the ggplot object with the data and aesthetic mapping

scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))

#Now tell ggplot what geometric objects to put in the plot. Note: we could use bars, lines, points, etc. This is a big difference b/t ggplot and regular plotting in R. You can build different types of graphs by using the same ggplot object, there's no need to learn one function for bar graphs, different function for line graphs, etc.

#Let's create a straightforward scatterplot

scatterplot + geom_point()

#Notice that the axes labels got cleaned up, and we also have nice grid liens in the background and solid points that pop out from the background

#Create a linegraph (even tho it doesn't make sense for this graph)

scatterplot + geom_line()

#Redo our plot with blue triangles instead of circles

scatterplot + geom_point(color="blue", size=3, shape=17)
scatterplot + geom_point(color="dark red", size=3, shape=15)

#Add a title

scatterplot + geom_point(color="dark red", size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")

#Save plot to a file

fertilityGNIplot = scatterplot + geom_point(color="dark red", size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")

pdf("MyPlot.pdf")

print(fertilityGNIplot)

dev.off()

#Check your current folder, you'll see MyPlot.pdf in there.


#VIDEO 5: ADVANCED SCATTERPLOTS USING GGPLOT

#Let's color the points by region

ggplot(WHO, aes(x = GNI, y = FertilityRate, color=Region)) + geom_point()
#dope

#Let's now color the points according to the country's life expectancy

ggplot(WHO, aes(x = GNI, y = FertilityRate, color=LifeExpectancy)) + geom_point()

#Suppose we were interested in seeing whether the fertility rate of a country was a good predictor of the population under 15 (intuitively, we'd expect this vars to be highly correlated). Let's explore our data with a plot

ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

#this doesn't look like a linear relationship, but we suspect that a log transformation of Fertility Rate will be better

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

#Let's build a linear regression model to predict the % of population

model = lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)

#It looks like the log(Fertility Rate) is a great predictor of Under15. R2=0.9387

#Add regression line to our plot

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm")

#By default, ggplot will draw a 95% CI around our line. We can change this

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", level=0.99)

#We could get rid of the CI border altogether:

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE)

#Change color of regression line

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE, color="orange")

