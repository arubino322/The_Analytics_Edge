#VIDEO 3: BAR CHARTS IN R

library(ggplot2)

intl = read.csv("intl.csv")
str(intl)

#barplot with ggplot

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))

#At first glance, this is strange. The labels are lying over the top of the columns, the regions aren't ordered in any way that's useful (probably would be better to have them ordered in descending order as opposed to alphabetical order, but alphabetical order is ggplot default).

#What is the stat="identity"? Geometry bar has mulitple modes of operation, and stat="identity" says, use the value of the y variable as is, which is what want. The height of the bar is the value of the y variable.

#Make region an ordered factor (unordered -> ordered) in decreasing order by PercentOfIntl

intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

#We didn't like that the numbers were between 0 and 1, so let's multiple all the values by 100

intl$PercentOfIntl = intl$PercentOfIntl*100
str(intl)

#Fix the overlying text and the x-axis being all bunched up next

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity", fill="dark blue") + geom_text(aes(label=PercentOfIntl), vjust=-0.4) + ylab("Percent of Intl Students") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

#geom_text: label equals the value of the column, vjust moves the labels up a little bit. Negative moves it up, positive moves it down

#ylab: adds y-axis label

#theme: first part removes x-axis label, 2nd part makes the label at 45 degrees, and hjust=1 moves it sideways a little bit.


#VIDEO 5: WORLD MAPS IN R

library(ggmap)

intlall = read.csv("intlall.csv", stringsAsFactors=FALSE)
head(intlall)

#Some NA's in here, but they should be 0's. Let's change that

intlall[is.na(intlall)] = 0
head(intlall)

#load world map

world_map = map_data("world")
str(world_map)

#join the intlall data frame and world_map together. Basically like a SQL join, but using the function merge

world_map = merge(world_map, intlall, by.x="region", by.y="Citizenship")
str(world_map)

#plot da map

ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")

#geom_polygon:Countries filled in white, borders in black.
#coord_map: mercator projection (???)

#Okay WTF is this. Countries looks like blobs b/c sometimes the merge can reorder the data. The world_map data frame is actually a list of latitude and longitude points that define the border of each country. If we accidentally reorder the data frame, they no longer make sense.

#Reorder the data in the correct order. First order the rows based on group (equivalent to the country), then the order variable, which is the correct order for the border points

world_map = world_map[order(world_map$group, world_map$order),]

#replot

ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")

#Better, but now some countries are missing (USA obviously b/c not international). But what about Russia and China? What the hell

#China is missing b/c it has a different name in the MIT data frame than in the world_map data frame. When we merged them, it was dropped from the data set b/c it didnt' match up.

table(intlall$Citizenship)
#China (Peopel's Republic Of). In world_map data frame, simply called "China". Change the MIT data frame to match.

intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"

table(intlall$Citizenship)

#merge again

world_map = merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")

#re-reorder

world_map = world_map[order(world_map$group, world_map$order),]

#plot, this time the fill being a color that's proportional to the total # of students

ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") + coord_map("mercator")

#Russia is missing for similar reasons, but you get the idea

#Looks pretty cool, but still a little confusing. Canada, China, and India make up a big percentage. However, b/c Europe is made up of a many small countries, it doesn't look that impressive. Would require further work

#This is a Mercator projection, but let's also look at an orthographic projection

ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") + coord_map("ortho", orientation=c(20,30,0))

#orientation=c(lat,lon,0)


#VIDEO 7: USING LINE CHARTS INSTEAD

library(ggplot2)

households = read.csv("households.csv")
str(households)

#It might not be obvious, but given this structure of the data frame, what would we put in the aesthetic for our ggplot command? ggplot needs it in the form of: year, group, and fraction. We must use the melt function to reshape our data

library(reshape2)

#The melt package in reshape2 will take a 2-dimensional data frame and convert it into exactly the right form we need for ggplot2

households[,1:2]

head(melt(households, id="Year"))

#Each value of MarriedWChild has turned into its own row in the new data frame

households[,1:3]

melt(households, id="Year")[1:10,]

#Every value in our data frame now corresponds to a new row in our melted data frame

#Now plot melted dataframe

ggplot(melt(households, id="Year"), aes(x=Year, y=value, color=variable)) + geom_line(size=2) + geom_point(size=5) + ylab("Percentage of Households")

#Basically, we melted the data so that we could categorize each line item. Once categorized and placed on it's own row, we can input in the color=variable part of ggplot


