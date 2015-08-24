################ANALYTICAL DETECTIVE###############
#Problem 1 & 2
getwd()
set("/Users/andrewrubino/Desktop/MySlideRule Data Path")
mvt <- read.csv("mvtWeek1.csv")
str(mvt)
summary(mvt)

max(mvt$ID)

#####Problem 2
head(mvt$Date)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
min(mvt$Date)
table(mvt$Month)
table(mvt$Weekday)
table(mvt$Month, mvt$Arrest)

###Problem 3
hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest)
#3.3
#We can just make a table without create a subset and find the proportion by looking at 2001 bro
table(mvt$Arrest, mvt$Year)

#####Problem 4
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL")
summary(Top5)
str(Top5)
table(Top5$LocationDescription)
#too messy, let's make the tables nicer to read
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription, Top5$Arrest)
table(Top5$LocationDescription, Top5$Weekday)


################STOCK DYNAMICS###############
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

str(IBM)
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
#summaries
sd(ProcterGamble$StockPrice) #SD of PG stock price

##Problem 2
plot(CocaCola$Date,CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date,ProcterGamble$StockPrice, col="blue")
#add a vertical line at around year 2000 with abline
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-03-01")), lwd=2)







