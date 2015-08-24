wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
summary(wiki)
wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)

library(tm)

corpusAdded = Corpus(VectorSource(wiki$Added))

#remove English language stop words

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]

#stem the words

corpusAdded = tm_map(corpusAdded, stemDocument)

#build DocumentTermMatrix

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#convert sparseAdded to dataframe
wordsAdded = as.data.frame(as.matrix(sparseAdded))

#prepend all the words with the letter A

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#preprocess for Removed words too

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)
ncol(wordsRemoved)

#combine 2 data frames
wikiWords = cbind(wordsAdded, wordsRemoved)

#add dependent variable
wikiWords$Vandal = wiki$Vandal

set.seed(123)
library(caTools)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl==TRUE)
test = subset(wikiWords, spl==FALSE)

table(test$Vandal)

#build CART model 

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~., data=train, method="class")
prp(wikiCART)

pred.wiki = predict(wikiCART, newdata=test)
pred.wiki[1:10,]
pred.prob = pred.wiki[,2]

table(test$Vandal, pred.prob >= 0.5)
accuracy = (618+12)/(618+12+533)

#OR YOU COULDVE DONE IT THIS WAY
pred.wiki = predict(wikiCART, newdata=test, type="class")
table(test$Vandal, pred.wiki)

#bag of words didn't work very well in this case. aka, the words were not useful

#copy of dataframe

wikiWords2 = wikiWords

#new column to determine if "http" was in Added

wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed=TRUE),1,0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal ~., data=wikiTrain2, method="class")
pred.wikiCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, pred.wikiCART2)
accuracy = (609+57)/(609+488+57+9)

#sum rows
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#another CART model
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

wikiCART3 = rpart(Vandal ~., data=wikiTrain3, method="class")
pred.wikiCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, pred.wikiCART3)

#make another copy
wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCART4 = rpart(Vandal ~., data=wikiTrain4, method="class")
pred.wikiCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, pred.wikiCART4)
prp(wikiCART4)




