#VIDEO 2

boston = read.csv("boston.csv")

str(boston)

plot(boston$LON, boston$LAT)

#We want to show all the points that lie along the Charles River in diff color

points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1], col="blue", pch=19)

#what if we wanna know where MIT is in this plot?
#the dude looked it up, MIT is in census tract 3531

points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531], col="red", pch=19)

summary(boston$NOX)

#lets plot the points with above average NOX (air pollution)

points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=19)

#the denser the area, the higher the air pollution (intuitively)

#lets make a new plot b/c this has too much going on
#let's look at prices vary over the area as well

plot(boston$LON, boston$LAT) #reset plot

summary(boston$MEDV)

#lets plot above average price points

points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)


#####VIDEO 3: GEOGRAPHICAL PREDICTIONS

plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

#both look nonlinear, but let's try fitting a linear regression anyways

latlonlm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)

plot(boston$LON, boston$LAT)

points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

#what does the linear regression model think is above median?

latlonlm$fitted.values
#this is what the model predicts for each of the 506 census tracts

points(boston$LON[latlonlm$fitted.values>=21.2], boston$LAT[latlonlm$fitted.values>=21.2], col="blue", pch="$")
#you can pass character values in the pch portion ($)

#we see that the plot has placed a $ for everytime it thinks the census tract
#/is above the median value.
#there's a sharp vertical line that divides areas with above median values,
#/and it's wrong! In a previous plot, we saw where there was a big non-red
#/spot in the middle of boston where the house prices were below median. The
#/linear regr model isn't doing a good job in this case


#############VIDEO 4: REGRESSION TREES

library(rpart)
library(rpart.plot)

#build regression tree

latlontree = rpart(MEDV ~ LAT + LON, data=boston)

#plot tree

prp(latlontree)

#in a classification tree, the leaves would be the classification we assign
#/that these splits would apply to. BUT in regression trees, we instead predict
#/the #. That # is the average of the median house

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

#we wanna predict what the tree thinks is above median

fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

#that's better! we've correctly left the low value area in boston out

#the tree was complicated. maybe we're overfitting. let's adjust minbucket size

latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)

plot(latlontree)
text(latlontree)
#plot still somewhat complicated

plot(boston$LON, boston$LAT)

#lets plot a vertical line at -71.01, the first split from the tree

abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)

#that area is the south boston area, where prices were a bit lower.
#maybe we can make that more clear by plotting the high value prices

points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)

#we see that the splits carves out that low value area in boston!!


###########VIDEO 5: PUTTING IT ALL TOGETHER

library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio=0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

#create linear regression model
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)

linreg.pred = predict(linreg, newdata=test)

linreg.sse = sum((linreg.pred - test$MEDV)^2)

#cool. let's build a tree
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)

#lat and lon not really important as far as the tree is concerned. RM appears 3 times in the tree. It's very nonlinear on
#/ the # of rooms. Things that were important for the linear regression that don't appear in our include pupil-teacher ratio

tree.pred = predict(tree, newdata=test)

tree.sse = sum((tree.pred - test$MEDV)^2)

#the SSE for the tree is higher than the SSE for the linear regression. Regression trees are not as good as lin reg for this prob
#What this says is given what we saw with lat and lon, is that lat and lon are nowhere near as useful for predicting as these
#/other vars are.


#########VIDEO 6:THE CP PARAMETER
#cp = complexity parameter
#Intuition: having too many splits is bad for generalization, so we should penalize the complexity
#S = # of splits
#lambda = penalty
#If we pick a large value of lambda, we won't make many splits b/c we pay a big rice for every additional split that outweighs the 
#/decrease in "error". If we pick a small (or zero) value of lambda, we'll make splits until it no longer decreases error

#the def of cp is closely related to lambda

#BASICALLY, when you're using cp in R code, smaller numbers of cp's encourage large trees, and large values of cp's encourage
#/ small trees.


##########VIDEO 7: CROSS VALIDATION

install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
library(lattice)
library(ggplot2)
library(caret)
install.packages("e1071")
library(e1071)

tr.control = trainControl(method="cv", number=10)

#Let's choose the values of cp that caret will try

cp.grid = expand.grid(.cp = (0:10)*0.001)

#let's store the results of the cross validation fitting in a variable called tr

tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train, method="rpart", trControl=tr.control, tuneGrid=cp.grid)
tr

#0.008 and 0.009 are the best b/c it has the lowest RMSE
#the best solutions are always close to zero, even if you make the range of cp larger values

#Let's see what the tree that value of cp corresponds to is

best.tree = tr$finalModel
prp(best.tree)

best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse

#this sse is better than the last tree sse, so we have improved the model. However, the lin regression did better than this still.
#cross validation did improve performance, but not as good as linear model. Always do cross validation to improve a CART model no matter what tho,
#/to see if a you can create a better model than lin regression






