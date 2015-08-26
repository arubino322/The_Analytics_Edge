#VISUALIZING TEXT DATA USING WORD CLOUDS

tweets = read.csv("tweets (1).csv", stringsAsFactors=FALSE)

#Preprocess the data, but don't stem the words in the document or remove sparse terms

library(tm)

library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))

#lowercase

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

#remove punctuations

corpus = tm_map(corpus, removePunctuation)

#remove all english stopwords

stopwords("english")

corpus = tm_map(corpus, removeWords, stopwords("english"))

#build a document-term matrix

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

#Building a wordcloud

library(wordcloud)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.5))

#remove the word "apple" from our data

tweets = read.csv("tweets (1).csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.5))

#Size and color

#Negative tweet wordcloud

negativeTweets = subset(allTweets, tweets$Avg <= -1)

wordcloud(colnames(negativeTweets), colSums(negativeTweets))

#random order will plot the words with the largest frequencies first (in the center)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.5), random.order=FALSE, min.freq=5)

#rot.per is the proportion of words with 90 degree rotation (10% by default)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.5), random.order=FALSE, min.freq=5, random.color=FALSE)


#selecting a color palette

library(RColorBrewer)

?brewer.pal
display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.5), random.order=FALSE, min.freq=3, random.color=FALSE, colors=brewer.pal(9,"Blues")[c(1,2,3,4)])

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.5), random.order=FALSE, min.freq=3, random.color=FALSE, colors=brewer.pal(9,"Reds"))


