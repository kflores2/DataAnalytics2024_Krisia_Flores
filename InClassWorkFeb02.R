library(readxl)
setwd("C:/DataAnalyticsThilnk")
#Read the CSV file
multivariate <- read.csv("multivariate.csv")
attach(multivariate)
names(multivariate)
multivariate

#Create some scatterplots
plot(Income, Immigrant, main = "Scatterplot")
plot(Immigrant, Homeowners, main = "Immigrant v Homeowner Scatterplot")

#Fitting Linear Models using "lm" functions
help(lm)
mm <- lm(Homeowners~Immigrant)
mm
plot(Immigrant, Homeowners)
abline(mm)
abline(mm, col=2, lwd=3)

summary(mm)
attributes(mm)
mm$coefficients


