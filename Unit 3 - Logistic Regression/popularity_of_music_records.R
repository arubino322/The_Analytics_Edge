songs = read.csv("songs.csv")

SongsTrain = subset(songs, year < 2010)
SongsTest = subset(songs, year == 2010)

#take out factor and year to create glm model

nonvars = c("year","songtitle","artistname","songID","artistID")

#remove the nonvars from SongsTrain

SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest) %in% nonvars)]

#first model

SongsLog1 = glm(Top10 ~., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness, SongsTrain$energy)

#model 2: keep energy, omit loudness (since multicollinearity exists b/t them)

SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain, family=binomial)
#you can do the -loudness b/c loudness is a numeric variable, you wouldn't be able to do this with other vars
summary(SongsLog2)

#model 3: keep loudness, omit energy
SongsLog3 = glm(Top10 ~ . -energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

Let's predict our accuracy
predictTest = predict(SongsLog3,newdata = SongsTest, type="response")
table(SongsTest$Top10, predictTest > 0.45)
accuracy = (309+19)/(309+5+40+19)

#create baseline model, aka predict the most frequent outcome for all observations
#in this case, an easier model would be to pick the most frequent outcome (a song is not a Top 10 hit)
table(SongsTest$Top10)
baseline = 314/(314+59)

#model 3 gives us a small improvement over our baseline model
sensitivity = 19/(19+40)
specificity = 309/(309+5)

