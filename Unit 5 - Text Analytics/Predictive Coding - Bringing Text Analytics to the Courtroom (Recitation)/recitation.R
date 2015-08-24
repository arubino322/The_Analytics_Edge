#VIDEO 2: THE DATA

emails = read.csv("energy_bids.csv")

#first email
emails$email[1]

#to wrap the text in order to read it easier
strwrap(emails$email[1])

#this is fine but let's look at an email that is responsive

emails$responsive[1]

strwrap(emails$email[2])

#this is a forwarded email with the email reading "report attached". This attachment had to do with Enron's electricity bids in California, and therefore is responsive to our query

emails$responsive[2]

table(emails$responsive)


#VIDEO 3: PRE-PROCESSING

#construct and preprocess the corpus

library(tm)

corpus = Corpus(VectorSource(emails$email))

#output the first email in the corpus
strwrap(corpus[[1]])

#preprocess the corpus using tm_map
#convert the corpus to lowercase

corpus = tm_map(corpus, tolower)

#remove punctuations

corpus = tm_map(corpus, removePunctuation)

#remove stopwords

corpus = tm_map(corpus, removeWords, stopwords("english"))

#stem the document

corpus = tm_map(corpus, stemDocument)

#Let's take a look at the first email again

strwrap(corpus[[1]])


#VIDEO 4: BAG OF WORDS

#Let's build the document-term matrix for our corpus

dtm = DocumentTermMatrix(corpus)
dtm
#Even tho we have only 855 emails in the corpus, we have about 22,000 terms that showed up at least once, whic is clearly too many variables for the number of observations we have

#We wanna remove the terms that don't appear too often in our data set, as follows:

dtm = removeSparseTerms(dtm, 0.97)
dtm
#^^ we determined the sparsity so that we remove any term that doesn't appear in at least 3% of the documents. To do that, we passed 0.97 to 

#build a data frame called labeledTerms out of thid document-term matrix

labeledTerms = as.data.frame(as.matrix(dtm))

#this dataframe is only including the frequencies of the words that appeared in at least 3% of the documents.

#let's add the outcome variable (whether the email was responsive or not)

labeledTerms$responsive = emails$responsive
str(labeledTerms)


#VIDEO 5: BUILDING MODELS

library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

#Let's build a CART model using default parameters

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ ., data=train, method="class")
prp(emailCART)

#Intepret: If California appears at least twice in an email, we're going to predict the document is responsive. It's unsurprising that California showed up b/c we know Enron had a heavy involvement in the CA energy markets.


#VIDEO 6: EVALUATING THE MODEL

#Create an object that has the predicted probabilities for each class from our CART models

pred = predict(emailCART, newdata=test)

pred[1:10,]

#we want to extract the predicted probability of the document being responsive (the rightmost column)

pred.prob = pred[,2]

table(test$responsive, pred.prob >= 0.5)
accuracy = (195+25)/(195+25+20+17) #.856

#compare to baseline
table(test$responsive)
accuracy = 215/(215+42) #.837

#We see only a slight improvement in the CART model over the baseline
#Typically, a human will still have to manually review all of the predicted responsive docs to make sure they are actually responsive. Therefore, if we have a false positive, in which a non-responsive document is labeled as reponsive, the mistake translates to a bit of additional work in the manual review process but nofurther harm, since the manual review process will remove this erroneous result. But on the other hand, if we have a false negative, in which a responsive doc is labeled as non-responsive, by our model, we will miss the document entirely in our predictive coding process. Therefore, we're going to assign a higher cost to false negatives than to false positives, which makes this a good time to look at other cut-offs on our ROC curve.


#VIDEO 7: THE ROC CURVE

library(ROCR)

build ROCR prediction object

predROCR = prediction(pred.prob, test$responsive)

#plot the ROC curve

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

#We favor cutoffs that give us a high sensitivity b/c we want to identify a large # of the responsive documents
#We can see from the blue color on the right of this plot at this particular location that we're looking at a threshold around maybe 0.15 or so, significantly lower than our 0.5 cutoff, which is what we would expect since we favor false positives to false negatives

#calculate AUC value

performance(predROCR, "auc")@y.values

#AUC ~= 79.4% which means that our model can differentiate b/t a randomely selected responsive and non-responsive document about 80% of the time.
