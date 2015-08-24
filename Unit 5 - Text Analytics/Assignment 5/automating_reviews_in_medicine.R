trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)
str(trials)

which.max(nchar(trials$abstract))
nchar(trials$abstract[664])
#3708
#OR
max(nchar(trials$abstract))

sum(nchar(trials$abstract)==0)
#OR
table(nchar(trials$abstract)==0)

which.min(nchar(trials$title))
trials$title[1258]

#let's create some corpera for title and abstract and do all the preprocessing
#/that we do

library(tm)
library(SnowballC)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

#make them lowercase
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

#remove punctuations
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

#remove english stop words
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords,stopwords("english"))

#stem the words
library(SnowballC)
corpusTitle = tm_map(corpusTitle, stemDocument, language="english")
corpusAbstract = tm_map(corpusAbstract, stemDocument, language="english")

#build a document term matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

#sparsness
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

#convert to data frames
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

length(stopwords("english"))
str(dtmTitle)
str(dtmAbstract)

#most frequent word stem across all abstracts
sort(colSums(dtmAbstract))

#combine dtmTitle and dtmAbstract into single dataframe

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)

#add outcome variable
dtm$trial = trials$trial

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl==TRUE)
test = subset(dtm, spl==FALSE)

table(train$trial)

#CART model

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

predTrain = predict(trialCART)

predTrain[1:10,]

pred.prob = predTrain[,2]
max(pred.prob)
#or
summary(pred.prob)

table(train$trial, pred.prob>=0.5)
sensitivity = 441/(441+131)
specificity = 631/(631+99)

predTest = predict(trialCART, newdata=test)
pred.prob.test = predTest[,2]
table(test$trial, pred.prob.test >= 0.5)

#plot ROC curce

library(ROCR)

predROCR = prediction(pred.prob.test, test$trial)
performance(predROCR, "auc")@y.values

perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)



