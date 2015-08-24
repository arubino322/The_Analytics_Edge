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















