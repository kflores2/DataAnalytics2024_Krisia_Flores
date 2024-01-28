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

#EPI info
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

#DALY info
summary(DALY)
fivenum(DALY)
stem(DALY)
hist(DALY)
hist(DALY, seq(0,100,2.2), probability = TRUE)
plot(ecdf(DALY), do.points=FALSE, verticals = TRUE)
qqnorm(DALY, main = "Normal Q-Q Plot for DALY"); qqline(DALY)

#WATER_H info
summary(WATER_H)
fivenum(WATER_H)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0,104,10.), probability = TRUE)
plot(ecdf(WATER_H), do.points=FALSE, verticals = TRUE)
qqnorm(WATER_H, main = "Normal Q-Q Plot for WATER_H");qqline(WATER_H)

#EPI & DALY
boxplot(EPI,DALY)
qqplot(EPI, DALY)
#abline(lm(sort(EPI) ~ sort(DALY)), col = "lightblue", lwd = 2, lty = 2)
abline(0,1, col="darkblue")

#DALY and WATER_H
boxplot(DALY, WATER_H)
qqplot(DALY, WATER_H)
abline(0,1,col="lightblue", lwd=2)

#ENVHEALTH
summary(ENVHEALTH)
fivenum(ENVHEALTH)
stem(ENVHEALTH)
hist(ENVHEALTH, seq(0.,100.,2.5), probability = TRUE)
lines(density(ENVHEALTH, na.rm=TRUE,bw=2.5))
lines(density(ENVHEALTH, na.rm=TRUE,bw="SJ"))
rug(ENVHEALTH)
plot(ecdf(ENVHEALTH), do.points=FALSE, verticals = TRUE)
qqnorm(ENVHEALTH, main = "Normal Q-Q Plot for ENVHEALTH");qqline(ENVHEALTH)

#ECOSYSTEM
summary(ECOSYSTEM)
fivenum(ECOSYSTEM)
stem(ECOSYSTEM)
hist(ECOSYSTEM, seq(0.,98.,2.), probability = TRUE)
lines(density(ECOSYSTEM, na.rm=TRUE,bw=2.))
lines(density(ECOSYSTEM, na.rm=TRUE,bw="SJ"))
rug(ECOSYSTEM)
plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals = TRUE)
qqnorm(ECOSYSTEM, main = "Normal Q-Q Plot for ECOSYSTEM");qqline(ECOSYSTEM)

#AIR_H
summary(AIR_H)
fivenum(AIR_H)
stem(AIR_H)
hist(AIR_H, seq(0.,100.,2.5), probability = TRUE)
lines(density(AIR_H, na.rm=TRUE,bw=2.5))
lines(density(AIR_H, na.rm=TRUE,bw="SJ"))
rug(AIR_H)
plot(ecdf(AIR_H), do.points=FALSE, verticals = TRUE)
qqnorm(AIR_H, main = "Normal Q-Q Plot for ");qqline()

#AIR_E
summary(AIR_E)
fivenum(AIR_E)
stem(AIR_E)
hist(AIR_E, seq(5.,90.,2.), probability = TRUE)
lines(density(AIR_E, na.rm=TRUE,bw=2.))
lines(density(AIR_E, na.rm=TRUE,bw="SJ"))
rug(AIR_E)
plot(ecdf(AIR_E), do.points=FALSE, verticals = TRUE)
qqnorm(AIR_E, main = "Normal Q-Q Plot for AIR_E");qqline(AIR_E)

#WATER_E
summary(WATER_E)
fivenum(WATER_E)
stem(WATER_E)
hist(WATER_E, seq(0.,100.,2.5), probability = TRUE)
lines(density(WATER_E, na.rm=TRUE,bw=2.5))
lines(density(WATER_E, na.rm=TRUE,bw="SJ"))
rug(WATER_E)
plot(ecdf(WATER_E), do.points=FALSE, verticals = TRUE)
qqnorm(WATER_E, main = "Normal Q-Q Plot for WATER_E");qqline(WATER_E)

#BIODIVERSITY ** (subject to possible filtering)
BIODIVERSITY
summary(BIODIVERSITY)
fivenum(BIODIVERSITY)
stem(BIODIVERSITY)
hist(BIODIVERSITY, seq(0.,100.,2.5), probability = TRUE)
lines(density(BIODIVERSITY, na.rm=TRUE,bw=2.5))
lines(density(BIODIVERSITY, na.rm=TRUE,bw="SJ"))
rug(BIODIVERSITY)
plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals = TRUE)
qqnorm(BIODIVERSITY, main = "Normal Q-Q Plot for BIODIVERSITY");qqline(BIODIVERSITY)

#EPI & EnvHealth
boxplot(EPI, ENVHEALTH)
title("EPI & ENVHEALTH Boxplot")
qqplot(EPI, ENVHEALTH);title("EPI & ENVHEALTH QQPlot")
abline(0,1,col="lightblue", lwd=2)

#EPI & ECOSYSTEM
boxplot(EPI, ECOSYSTEM);title("EPI & ECOSYSTEM Boxplot")
qqplot(EPI, ECOSYSTEM);title("EPI & ECOSYSTEM QQPlot")
abline(0,1,col="lightblue", lwd=2)

#EPI & AIR_H
boxplot(EPI, AIR_H);title("EPI & AIR_H Boxplot")
qqplot(EPI, AIR_H);title("EPI & AIR_H QQPlot");abline(0,1,col="lightblue", lwd=2)

#EPI & AIR_E
boxplot(EPI, AIR_E);title("EPI & AIR_E Boxplot")
qqplot(EPI, AIR_E);title("EPI & AIR_E QQPlot");abline(0,1,col="lightblue", lwd=2)

#EPI & WATER_E
boxplot(EPI, WATER_E);title("EPI & WATER_E Boxplot")
qqplot(EPI, WATER_E);title("EPI & WATER_E QQPlot");abline(0,1,col="lightblue", lwd=2)

#EPI & BIODIVERSITY
boxplot(EPI, BIODIVERSITY);title("EPI & BIODIVERSITY Boxplot")
qqplot(EPI, BIODIVERSITY);title("EPI & BIODIVERSITY QQPlot");abline(0,1,col="lightblue", lwd=2)


#Filtering Section
EPILand <- EPI[!Landlock] #epi of no landlocked countries
ELand <- EPILand[!is.na(EPILand)] #makes sure there are no empty values
hist(ELand)
hist(ELand, seq(30.,95.,1.0), prob=TRUE)
lines(density(ELand, na.rm=TRUE,bw=1.))
lines(density(ELand, na.rm=TRUE,bw="SJ"))
rug(ELand)

#Landlock #check landlock area
#Country[!Landlock] #countries not landlocked
#Country[Landlock==1] #countries that are landlocked

#EPI of Countries with  surface water
No_surface_water
Country[No_surface_water==1]
EPInoWater <- EPI[!No_surface_water]
EnoWater <- EPInoWater[!is.na(EPInoWater)]
stem(EnoWater)
summary(EnoWater)
hist(EnoWater)
hist(EnoWater, seq(30.,95.,1.), prob=TRUE)
lines(density(EnoWater, na.rm=TRUE,bw=1.))
lines(density(EnoWater, na.rm=TRUE,bw="SJ"))
rug(EnoWater)

#Desert 
Desert
Country[Desert==1]
EPInoDesert <- EPI[!Desert]
EnoDesert <- EPInoDesert[!is.na(EPInoDesert)]
stem(EnoDesert)
summary(EnoDesert)
hist(EnoDesert)
hist(EnoDesert, seq(30.,95.,1.), prob=TRUE)
lines(density(EnoDesert, na.rm=TRUE,bw=1.))
lines(density(EnoDesert, na.rm=TRUE,bw="SJ"))
rug(EnoDesert)

#High_Population_Density
High_Population_Density
Country[High_Population_Density==1]
EPIlowPop <- EPI[!High_Population_Density]
ElowPop <- EPIlowPop[!is.na(EPIlowPop)]
stem(ElowPop)
summary(ElowPop)
hist(ElowPop)
hist(ElowPop, seq(30.,95.,1.), prob=TRUE)
lines(density(ElowPop, na.rm=TRUE,bw=1.))
lines(density(ElowPop, na.rm=TRUE,bw="SJ"))
rug(ElowPop)
#with low pop density
EPIhighPop <- EPI[High_Population_Density==1]
EhighPop <- EPInoWater[!is.na(EPIhighPop)]
stem(EhighPop)
summary(EhighPop)
hist(EhighPop)
hist(EhighPop, seq(30.,95.,1.), prob=TRUE)
lines(density(EhighPop, na.rm=TRUE,bw=1.))
lines(density(EhighPop, na.rm=TRUE,bw="SJ"))
rug(EhighPop)

#Filtering by SubRegion
EPI_regions
EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia
GEO_subregion
EPI2_SouthAsia <- EPI[GEO_subregion=="South Asia"]
EPI2_SouthAsia

gpw3 <- read.csv('GPW3_GRUMP_SummaryInformation_2010.csv', header = T)
head(gpw3)
class(gpw3)
summary(gpw3)
head(gpw3[1,])

View(gpw3)  
attach(gpw3) #sets as default object
pop <- as.integer(Population07[Population07 != "NA"])
pop
class(pop)
integer
summary(pop)
stem(pop)
hist(pop)
lowpop <- subset(pop, subset = pop<=9565000)
lowpop
summary(lowpop)
hist(lowpop)
lines(density(lowpop, na.rm=TRUE,bw="SJ"))
rug(lowpop)

