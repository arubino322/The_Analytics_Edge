##CLIMATE CHANGE

climate <- read.csv("climate_change.csv")
summary(climate)
str(climate)


training <- subset(climate, Year < 2007)
testing <- subset(climate, Year > 2006)

model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training)
summary(model1)

model2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = training)
summary(model2)

model1Step = step(model1)

stepPredict = predict(model1Step, newdata = testing)
SSE = sum((stepPredict - testing$Temp)^2)
SST = sum((mean(training$Temp) - testing$Temp)^2)
R2 = 1 - SSE/SST



###READING TEST SCORES
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

#remove observations with any missing value
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

str(pisaTrain)

#include unordered factors in regression models
#Set the reference level for raceeth (the most common factor aka white)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore <- lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

SSE_pt = sum(lmScore$residuals^2)
RMSE_pt = sqrt(SSE_pt/nrow(pisaTrain))

summary(lmScore)


predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)
SSE_t = sum((predTest - pisaTest$readingScore)^2)
SST_t = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
RMSE_t = sqrt(SSE_t/nrow(pisaTest))

mean(pisaTrain$readingScore)

1 - SSE_t/SST_t



#####DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA#######
FluTrain <- read.csv("FluTrain.csv")
summary(FluTrain)
which.max(FluTrain$ILI) #this finds the row of the max ILI
FluTrain$Week[303] #this outputs the week for the row with highest ILI

which.max(FluTrain$Queries)
FluTrain$Queries[303]

hist(FluTrain$ILI)

plot(log(FluTrain$ILI), FluTrain$Queries)

#find the best models
modelx <- lm(log(ILI) ~ Queries, data = FluTrain)
modely <- lm(Queries ~ log(ILI), data = FluTrain)
summary(modelx)
summary(modely)
#modelx is the best

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

cor(log(FluTrain$ILI),FluTrain$Queries)
#correlation^2 = R2

FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

which(FluTest$Week == "2012-03-11 - 2012-03-17") #11
PredTest1[11]

FluTest$ILI[11]

SSE_fluTest = sum((PredTest1 - FluTest$ILI)^2)
RMSE_flutest = sqrt(SSE_fluTest/nrow(FluTest))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

summary(FluTrain$ILILag2)

plot(FluTrain$ILILag2, log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

#add ILILag2 to test data set
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

which(FluTest$ILILag2 == "NA")
FluTest$ILILag2

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[1] #1.852736

FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[2] #2.12413

PredTest2 = exp(predict(FluTrend2, newdata = FluTest))
SSE_fluTest2 = sum((PredTest2 - FluTest$ILI)^2)
RMSE_flutest2 = sqrt(SSE_fluTest2/nrow(FluTest))



###################################OPTIONAL#######################################
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
str(statedata)
plot(statedata$x, statedata$y)

tapply(statedata$HS.Grad, statedata$state.region, mean)
boxplot(statedata$Murder ~ statedata$state.region)

northeast <- subset(statedata, state.region == "Northeast")
summary(northeast)
which.max(northeast$Murder)
northeast$state.name[6]

stateModel <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(stateModel)

plot(statedata$Income, statedata$Life.Exp)

#test more models by removing variables
stateModel2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(stateModel2)
#it would be up to you to leave population, but we'll leave it cuz it has just enough statistical relevancy to be okay

predState <- predict(stateModel2)
summary(predState)
#find lowest Life.Exp
sort(predict(stateModel2))

which.min(statedata$Life.Exp)
statedata$state.abb[40]

which.max(statedata$Life.Exp)
statedata$state.abb[11]

SSE = sum((predState - statedata$Life.Exp)^2)
sort(abs(stateModel2$residuals))


###########FORECASTING ELANTRA SALES##############
elantra = read.csv("elantra.csv")
elantraTrain = subset(elantra, Year < 2013)
elantraTest = subset(elantra, Year > 2012)

elantraModel <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(elantraModel)

emodel2 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = elantraTrain)
summary(emodel2)

elantraTrain$MonthFactor = as.factor(elantraTrain$Month)
emodel3 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + MonthFactor, data = elantraTrain)
summary(emodel3)
cor(elantraTrain$CPI_energy, elantraTrain$Month)
cor(elantraTrain$CPI_energy, elantraTrain$Unemployment)
cor(elantraTrain$CPI_energy, elantraTrain$Queries)
cor(elantraTrain$CPI_energy, elantraTrain$CPI_all)
#OR A BETTER WAY TO WRITE THIS
cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

#remove variables from our model to make it more significant
emodel3 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + MonthFactor, data = elantraTrain)

elantraTest$MonthFactor = as.factor(elantraTest$Month)
elantraPredict = predict(emodel3, newdata = elantraTest)
SSE = sum((elantraPredict - elantraTest$ElantraSales)^2)
mean(elantraTrain$ElantraSales) #baseline
SST = sum((mean(elantraTrain$ElantraSales) - elantraTest$ElantraSales)^2)

R2 = 1 - SSE/SST

max(abs(elantraPredict - elantraTest$ElantraSales))
which.max(abs(elantraPredict - elantraTest$ElantraSales))
elantraTest$Month[5]
elantraTest$Year[5]