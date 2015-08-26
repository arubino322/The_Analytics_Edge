#ELECTION FORECASTING REVISITED

library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

#Draw a map of the US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#Color states by predictions

polling = read.csv("PollingImputed.csv")
str(polling)

#Create training and test set

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

#logistic regression model

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data=Train, family=binomial)

TestPrediction = predict(mod2, newdata=Test, type="response")
#gives us predicted probabilities for each state

#Create a vector of Republican/Democrat predictions

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

#Put the predictions and statelabels in a data.frame so that we can use in ggplot

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)

#Coloring states by predictions

#Merge predictionDataFrame with statesMap like we did in lecture. First, convert Test.State var to lowercase so that it matches region var in statesMap

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

#merge

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#make sure the observations are in order

predictionMap = predictionMap[order(predictionMap$order),]

#Color this shit according to our binary predictions

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredictionBinary)) + geom_polygon(color="black")

#Replot the map with discrete outcomes, since the only 2 possible outcomes are 0 and 1. Also change the color scheme to blue and red to match respective parties

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredictionBinary)) + geom_polygon(color="black") + scale_fill_gradient(low="blue", high="red", guide="legend", breaks = c(0,1), labels = c("Democrat","Republican"), name="Prediction 2012") 

#plot probabilities instead of binary predictions

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPrediction)) + geom_polygon(color="black") + scale_fill_gradient(low="blue", high="red", guide="legend", breaks = c(0,1), labels = c("Democrat","Republican"), name="Prediction 2012") 

#Predicted probability for Florida

predictionDataFrame

#Parameter settings

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredictionBinary)) + geom_polygon(color="black", linetype=3) + scale_fill_gradient(low="blue", high="red", guide="legend", breaks = c(0,1), labels = c("Democrat","Republican"), name="Prediction 2012") 

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredictionBinary)) + geom_polygon(color="black", size=3) + scale_fill_gradient(low="blue", high="red", guide="legend", breaks = c(0,1), labels = c("Democrat","Republican"), name="Prediction 2012") 

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredictionBinary)) + geom_polygon(aes(alpha=3), color="black") + scale_fill_gradient(low="blue", high="red", guide="legend", breaks = c(0,1), labels = c("Democrat","Republican"), name="Prediction 2012") 
