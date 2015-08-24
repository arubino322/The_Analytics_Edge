loans <- read.csv("loans.csv")
str(loans)
summary(loans)

table(loans$not.fully.paid)

nas = subset(loans, is.na(pub.rec) | is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths | is.na(delinq.2yrs)))
summary(nas)
table(nas$not.fully.paid)
#12 weren't fully paid out of 62 of the missing, which is enough data to skew the end result. we want those values

#redownloaded the imputed data just to ensure that the data is the same
loans2 = read.csv("loans_imputed.csv")

#split data into train and test
set.seed(144)
library(caTools)
split = sample.split(loans2$not.fully.paid, SplitRatio = 0.7)
train = subset(loans2, split == TRUE)
test = subset(loans2, split == FALSE)

#model1
str(train)
mod1 <- glm(not.fully.paid ~., data = train, family=binomial)
summary(mod1)

#predict
test$predicted.risk = predict(mod1, newdata=test, type="response")

table(test$not.fully.paid, test$predicted.risk > 0.5)

#baseline
table(test$not.fully.paid)
2413/(2413+460)

#compute test set AUC
library(ROCR)
pred = prediction(test$predicted.risk,test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

#model 2
bivariate = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(mod1)
summary(bivariate)
cor(train$int.rate,train$fico)

#test set prediction for bivariate model
predict.bivariate = predict(mod2, newdata = test, type="response")
summary(predict.bivariate)
table(test$not.fully.paid, predict.bivariate > 0.5)

#auc of bivariate
pred.biv = prediction(predict.bivariate, test$not.fully.paid)
as.numeric(performance(pred.biv, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
which.max(test$profit)
test$profit[1780]
#OR
summary(test$profit)

highInterest <- subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)

