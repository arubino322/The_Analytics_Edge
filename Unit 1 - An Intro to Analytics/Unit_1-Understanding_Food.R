USDA = read.csv("USDA.csv")

#####DATA ANALYSIS DISCUSSION
#Sodium max is high as fuck, let's investigate that further
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
#We want description
USDA$Description[265] #table salt!!! fuckin A!!!


#lets create a subset for foods with high sodium content
HighSodium <- subset(USDA, Sodium > 10000)
#How many observations satisfy this subset?
nrow(HighSodium) #10
#what foods have high sodium?
HighSodium$Description
#what about caviar? Let's find it
match("CAVIAR", USDA$Description) #4154 in dataset
USDA$Sodium[4154] #1500 mgs
USDA$Sodium[match("CAVIAR", USDA$Description)] #this is the same as the line before
#lets compare this information to the mean
summary(USDA$Sodium)
#Let's take sd too
sd(USDA$Sodium, na.rm = T) #remove NA's


#####CREATING PLOTS IN R
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
#Let's make a histogram, and remember histograms can only plot the frequency of one variable at a time
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vit C levels")
Let's set a limit on the x axis so that we can look at the histogram more closely
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vit C levels", xlim = c(0,100))
#still need to break up the cell into smaller pieces
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vit C levels", xlim = c(0,100), breaks = 100)
#why do we only see 5 cells? remember the hist went up to 2000, so 2000/100, each cell is 20 mg long. Make the breaks = 2000
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vit C levels", xlim = c(0,100), breaks = 2000)
#About 5000 foods have less than a mg of vitC
#Boxplot for sugar
boxplot(USDA$Sugar, main = "Boxplot of Sugar levels", ylab = "Sugar (g)")
#the median is really low, but we have a lot of outliers with extremely high levels


#####ADDING VARIABLES
#We want to add a variable that takes value 1 if the food has high sodium than average, and 0 otherwise
#Lets check where the first food in the data stands
USDA$Sodium[1] > mean(USDA$Sodium, na.rm=T) #boolean
HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm=T) #boolean
str(HighSodium)
#we need to change the data type of HighSodium to numeric
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
#We wanna add this variable to our dataframe now
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
str(USDA)
#Now we can do the same for proteins, carbs, fat, etc
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=T))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=T))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=T))
USDA$HighIron = as.numeric(USDA$Iron > mean(USDA$Iron, na.rm=T))
str(USDA)
#How do we find relationships between new variables and old variables? Stay tuned


#####SUMMARY TABLES
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat) #row is sodium, column is fat
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = T)
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm = T)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = T)




