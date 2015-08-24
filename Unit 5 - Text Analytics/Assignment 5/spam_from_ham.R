emails = read.csv("emails.csv", stringsAsFactors=FALSE)
summary(emails)
str(emails)

table(emails$spam)

emails$text[2]

max(nchar(emails$text))

which.min(nchar(emails$text))

#preprocess this shit

library(tm)

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

length(stopwords("english"))
dtm

#sparsewords

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

#make data frame
emailsSparse = as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) = make.names(colnames(emailsSparse))

sort(colSums(emailsSparse))

#add outcome variable

emailsSparse$spam = emails$spam

sort(colSums(subset(emailsSparse, spam == 0)))

sort(colSums(subset(emailsSparse, spam == 1)))

#machine learning models

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl==TRUE)
test = subset(emailsSparse, spl==FALSE)

#logistic regression model

spamLog = glm(spam ~ ., data=train, family=binomial)

#CART model
library(rpart)
library(rpart.plot)

spamCART = rpart(spam ~ ., data=train, method="class")

#random forest model
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

predLog = predict(spamLog, type="response")
predCART = predict(spamCART)[,2]
predRF = predict(spamRF, type="prob")[,2]

table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

summary(spamLog)

prp(spamCART)

table(train$spam, predLog >= 0.5)

#compute AUC
library(ROCR)
predTrainLog = prediction(predLog, train$spam)
as.numeric(performance(predTrainLog, "auc")@y.values)

table(train$spam, predCART > 0.5)
predTrainCART = prediction(predCART, train$spam)
as.numeric(performance(predTrainCART, "auc")@y.values)

table(train$spam, predRF > 0.5)
predTrainRF = prediction(predRF, train$spam)
as.numeric(performance(predTrainRF, "auc")@y.values)

#spamLog is our best model because it has the highest accuracy and AUC

#obtain predicted probabilities for the testing set of each of the models
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]

table(test$spam, predTestLog > 0.5)
predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)

table(test$spam, predTestCART > 0.5)
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)

table(test$spam, predTestRF > 0.5)
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)


#Part 2

wordCount = rowSums(as.matrix(dtm))
wordCount

hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount~emailsSparse$spam)

train2 = subset(emailsSparse, spl==TRUE)
test2 = subset(emailsSparse, spl==FALSE)

spam2CART = rpart(spam ~ ., data=train2, method="class")

set.seed(123)
spam2RF = randomForest(spam ~ ., data=train)

prp(spam2CART)

predCART2 = predict(spam2CART, newdata=test2)[,2]
predRF2 = predict(spam2RF, newdata=test2, type="prob")[,2]

table(test2$spam, predCART2 > 0.5)
predictionCART2 = prediction(predCART2, test2$spam)
as.numeric(performance(predictionCART2, "auc")@y.values)

table(test2$spam, predRF2 > 0.5)
predictionRF2 = prediction(predRF2, test2$spam)
as.numeric(performance(predictionRF2, "auc")@y.values)