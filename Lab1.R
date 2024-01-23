library(XLS)
library(gdata)
library(readxl)
library(car)

setwd("C:/DataAnalyticsThilnk")
getwd()

EPI_data <- read_excel('2010EPI_data.xls', sheet = 'EPI2010_onlyEPIcountries')
head(EPI_data)
class(EPI_data)
head(EPI_data[1,])

summary(EPI_data)
#help(distributions)

View(EPI_data)  
attach(EPI_data) #sets as default object
fix(EPI_data) #data editor
EPI #lets us see EPI_data$EPI

tf <- is.na(EPI)
E <- EPI[!tf]

summary(EPI)
fivenum(EPI)
stem(EPI) #stem and leaf plot
#help(stem)
hist(EPI)
hist(EPI, seq(30.,95.,1.0), probability = TRUE)
lines(density(EPI, na.rm=TRUE,bw=1.))
lines(density(EPI, na.rm=TRUE,bw="SJ"))
rug(EPI)
#help(rug)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t 
dsn")
qqline(x)
#help(qqnorm)

summary(DALY)
fivenum(DALY)
stem(DALY)
hist(DALY)
hist(DALY, seq(0,100,2.2), probability = TRUE)
plot(ecdf(DALY), do.points=FALSE, verticals = TRUE)

summary(WATER_H)
fivenum(WATER_H)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0,104,10.), probability = TRUE)
plot(ecdf(WATER_H), do.points=FALSE, verticals = TRUE)
#insert page 16 work

boxplot(EPI,DALY)
qqplot(EPI, DALY)
#abline(lm(sort(EPI) ~ sort(DALY)), col = "lightblue", lwd = 2, lty = 2)
abline(0,1, col="darkblue")

boxplot(DALY, WATER_H)
qqplot(DALY, WATER_H)
abline(0,1,col="lightblue", lwd=2)

EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30.,95.,1.0), prob=TRUE)
lines(density(ELand, na.rm=TRUE,bw=1.))
lines(density(ELand, na.rm=TRUE,bw="SJ"))
rug(ELand)

#Landlock #check landlock area
#Country[!Landlock] #countries not landlocked
#Country[Landlock==1] #countries that are landlocked


