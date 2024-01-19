help('read.csv')
getwd()
setwd("C:/DataAnalyticsThilnk")
getwd()
data1 <- read.csv('2010EPI_data.csv', header = T)

data1
head(data1)

activeValues <- subset(data1,subset=data1$GDPCAP07!="..")
activeValues
head(activeValues)
class(activeValues$GDPCAP07)
intActive <- as.integer(activeValues$GDPCAP07)
class(intActive)

fivenum(intActive)

summary(intActive)
boxplot(intActive)
hist(intActive)

summary(data1)

        