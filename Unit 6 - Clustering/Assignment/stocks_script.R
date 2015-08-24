stocks = read.csv("StocksCluster.csv")

table(stocks$PositiveDec)/nrow(stocks)

#correlation

cor(stocks)

summary(stocks)

#initial logistic regression model

set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)

predictTrain = predict(StocksModel, type="response")

table(stocksTrain$PositiveDec, predictTrain > 0.5)

table(stocksTest$PositiveDec)/nrow(stocksTest)

#clustering stocks. Remove the dependent variable

limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

#preprocess and normalize the training set

library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#run k-means

set.seed(144)

km = kmeans(normTrain, centers=3)

table(km$cluster)

#use flexclust package to obtain training set and test set cluster assignments for our observations

library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

#Build subsets for each of the clusters from stockTrain and stockTEst

stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)

stocksTest1 = subset(stocksTest, clusterTest==1)
stocksTest2 = subset(stocksTest, clusterTest==2)
stocksTest3 = subset(stocksTest, clusterTest==3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

#make test set predictions

PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

#To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)

