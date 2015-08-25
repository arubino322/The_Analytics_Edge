#VIDEO 3: A LINE PLOT

mvt = read.csv("mvt.csv", stringsAsFactors=FALSE)

#first, convert the date variable to a format that R will recognize so that we can extract the day of the week and hour of the day

mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")

mvt$Weekday = weekdays(mvt$Date)

mvt$Hour = mvt$Date$hour

str(mvt)

#Let's make a basic line plot. Plot the total number of crimes on each day of the week

table(mvt$Weekday)

#save this table to a dataframe, the pass it to ggplot for our data

WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)
#Var1 = Weekday, Freq = Total Crimes

library(ggplot2)

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1))
#the group=1 just groups our data all into 1 line since we want 1 line in our plot

#our days of the week are out of order b/c ggplot put the days of the wek in alphabetical order. Let's put it in chronological order.

#Make the Var1 variable an ordered factor variable

WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1))

#Lastly, change the labels to the axes

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1)) + xlab("Day of Week") + ylab("Total Motor Vehicle Thefts")


#VIDEO 4: A HEATMAP

#Include the hour of the day in the line plot

table(mvt$Weekday, mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

#Let's convert the 2nd variable, Var2, to actual numbers and call it hour

DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

#plot this bitch by Day of the Week (Var1)

ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1))

#Let's change the colors of the line to correspond with days of the week

ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

#while more interpretable, still difficult to interpret with 7 lines. Let's use a heatmap

#First, we'll need to fix the order of the days so that they'll show up in chronological order instead of alphabetical order

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) 

#Now make heatmap

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq))

#change the label on the legend and get rid of the y label to make the plot nicer

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())

#Change the color scheme

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())


#VIDEO 5: GEOGRAPHICAL HOT SPOT MAP

library(maps)

library(ggmap)

#Load a map of Chicago into R

chicago = get_map(location ="chicago", zoom=11)

ggmap(chicago)
#whaaaaaaaat

#lets plot the first 100 motor vehicle thefts in our data on this map

ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x = Longitude, y = Latitude))

#plotting all 190000 observations would yield a big black box in our data, but we still wanna see what areas show a higher number of mvt's

#Let's round our long's and lat's to 2 digits of accuracy and create a crime counts data frame for each area

LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
#this gives us the total crimes at every point on grid
str(LatLonCounts)

#Convert our Lat and Long variables to numbers

LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))

LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

#Plot these points on our map, making the size and color of these points depend on the total number of mvt's

ggmap(chicago) + geom_point(data=LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq))

#change color scheme

ggmap(chicago) + geom_point(data=LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low="yellow", high="red")

#Use geom_tile to make a more traditional heatmap

ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

#Let's get rid of the squares that were plotted out in the water

LatLonCounts2 = subset(LatLonCounts, Freq > 0)

ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")


#VIDEO 6: A HEATMAP ON THE UNITED STATES

murders = read.csv("murders.csv")

#A map of the US is included in R, so let's load it

statesMap = map_data("state")

str(statesMap)

#use ggplot to plot the map

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill="white", color="black")

#Before we can plot our data on this map, we need to make sure that the states names are the same in the murders dataframe and in the statesMap data frame

#They're not, so let's create a new variable called region in our murders data frame to match the state name variable to the statesMap data frame

murders$region = tolower(murders$State)

#Join the statesMaps data frame to the murders data frame by using the merge function

murderMap = merge(statesMap, murders, by="region")
str(murderMap)

#Plot the number of murders on our map of the US

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

#It looks like Cali and Texas have the most number of murders, but is that because they're the most populous states? Let's check.

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

#Looks exactly the same of our murders map. We need to plot murder RATE instead of just murders

#Let's create a new variable for the murder rate per 100,000 population

murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000

#Redo plot by murder rate

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")

#Now it's mostly maroon colored, and this won't highlight Washington DC, which is small but has a really high murder rate. Redo our plot to remove any observations with murder rates above 10, which we know will only exclude Wash DC

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,10))

#Better. One more map, looking at gun ownership

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")