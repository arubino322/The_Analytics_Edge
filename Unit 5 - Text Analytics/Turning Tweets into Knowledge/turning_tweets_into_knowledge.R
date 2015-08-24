##VIDEO 5: PRE-PROCESSING IN R

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
#you'll always need to add this extra argument when working on a text analytics problem so that the text
#/is read in properly

str(tweets)
 
tweets$Negative = as.factor(tweets$Avg <= -1)
#This will set tweets$Negative equal to true if the average sentiment score is less than or equal to negative 1
#/and will set tweets$Negative equal to false if the avg sent score is greater than negative 1

table(tweets$Negative)

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

#Let's convert our tweets to a corpus, or a collection of documents, for preprocessing. tm can creative a corpus in 
#/different ways, but let's create it from the tweet column of our data frame using functions Corpus and VectorSource

corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]

#Now let's preprocess our data. Let's first change all the text in our tweets to lowercase

corpus = tm_map(corpus, tolower)
corpus[[1]]

#convert all documents in the corpus to the PlainTextDocument type

corpus = tm_map(corpus, PlainTextDocument)

#remove all punctuations

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

#Remove the stop words. tm provides a list of stop words for the English language

stopwords("english")[1:10]
#this lists the first 10 stopwords

#we wanna remove the word apple too, since they appear in every tweet and aren't much help

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

#Lastly, we want to stem our document

corpus = tm_map(corpus, stemDocument, language="english")
corpus[[1]]


##VIDEO 6: BAG OF WORDS IN R


#DocumentTermMatrix: generates a matrix where the rows correspond to documents, in our case tweets, & the columns correspond to words in those tweets. The values in the matrix are the # of times that word appears in each document

frequencies = DocumentTermMatrix(corpus)

#inspect the matrix. we'll look at documents 1000-1005, and words 505-515

inspect(frequencies[1000:1005,505:515])

#this data is sparse. let's look at the most popular words

findFreqTerms(frequencies, lowfreq=20)

#How do we explain this? Out of the 3289 words in our matrix, only 56 words appear at least 20 times in our tweets. Aka, we'll probably h ave a lot of terms that will be pretty useless for our prediction model

#let's remove some terms that don't appear very often

sparse = removeSparseTerms(frequencies, 0.995)

#we are telling sparse that we only wanna keep terms that appear in 0.5% or more of the tweets, aka about 6 or more tweets

#convert the sparse matrix into a dataframe

tweetSparse = as.data.frame(as.matrix(sparse))

#let's run makes.names function to make sure all of our words are appropriate var names

colnames(tweetSparse) = make.names(colnames(tweetSparse))

#add dependent var to the dataset

tweetSparse$Negative = tweets$Negative

#split data into train and test set

library(caTools)
set.seed(123)
split = sample.split(tweetSparse$Negative, SplitRatio=0.7)
trainSparse = subset(tweetSparse, split == TRUE)
testSparse = subset(tweetSparse, split == FALSE)


###VIDEO 7: PREDICTING SENTIMENT

#use CART to create a predictive model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

#plot tree

prp(tweetCART)

#how do we explain this? if the word freak is in the tree, then we predict TRUE (negative sentiment). If freak isn't, but hate is, then predict true. if neither of these words are, but wtf is, then predict true.

#evaluate the numerical performance of our model by making predictions on the test set

predictCART = predict(tweetCART, newdata=testSparse, type="class")

#Let's see how we did using a confusion matrix

table(testSparse$Negative, predictCART)

accuracy = (294+18)/(294+18+37+6)
#0.878

#Let's compare this to a simple baseline model that always predicts non-negative

table(testSparse$Negative)
#accuracy = 300/355 = 0.845

#our CART model does better than the simple baseline model

#Let's try out a random forest model

install.packages("randomForest")
library(randomForest)

set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

#this might take a while, on account of there being so many independent variables (we have about 300 words in our dataset, each one an ind. var.)

#let's make predictions on our test set

predictRF = predict(tweetRF, newdata=testSparse)

#confusion matrix

table(testSparse$Negative, predictRF)
accuracy = (293+21)/(293+21+34+7) #.885

#this is a little bit better than our CART model, but due to the interpretability of our CART model, we should probably stick with that. If you were to use cross-validation to pick the cp parameter for the CART model, the accuracy would increase to about the same as the RF model

tweetLOG = glm(Negative ~ ., data=trainSparse, family="binomial")
predictions = predict(tweetLOG, newdata=testSparse, type="response")
table(testSparse$Negative, predictions > 0.5)



