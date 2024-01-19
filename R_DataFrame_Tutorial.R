days <- c('Mon', 'Tue', 'Wed','Thur','Fri','Sat','Sun') #days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed1 <- c('T','T','F','F','T','T','F')
snowed
class(snowed)
snowed2 <- c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE)
class(snowed2)

help("data.frame")
RPI_Weather_Week1 <- data.frame(days, temp, snowed1)
RPI_Weather_Week2 <- data.frame(days, temp, snowed2)


RPI_Weather_Week1
RPI_Weather_Week2
head(RPI_Weather_Week2) #first 6 rows of data frame

str(RPI_Weather_Week) #look at structure of data frame using str() function
summary(RPI_Weather_Week)

str(RPI_Weather_Week2)
summary(RPI_Weather_Week2)


RPI_Weather_Week[1,] #shows 1st row and all columns
RPI_Weather_Week[,1] #shows 1st col and all rows

RPI_Weather_Week2[,'snowed2']
RPI_Weather_Week2[,'days']
RPI_Weather_Week2[,'temp']
RPI_Weather_Week2[1:5,c("days","temp")]
RPI_Weather_Week2[1:5,c('days','snowed2')]
RPI_Weather_Week2$temp
RPI_Weather_Week2$days
subset(RPI_Weather_Week2,subset=snowed2==TRUE)

sorted.snowed <- order(RPI_Weather_Week2$snowed2)
sorted.snowed
RPI_Weather_Week2[sorted.snowed,]

dec.snow <- order(RPI_Weather_Week2$temp)
dec.snow

empty.DataFrame <- data.frame()
v1 <-1:10
v1
letters
v2 <- letters[1:10]
v2
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df

write.csv(df,file='saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2



help("read.csv")
data1 <- read.csv(file.choose(), header = TRUE)
