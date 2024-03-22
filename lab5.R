#Cook's Distance example using mtcars
mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~ cyl + wt, mtcars)
model1
help("cooks.distance")
plot(model1, pch=18, col='red', which = c(4))

#we can use the cooks.distance() function to identify the Cook's distance to each observation
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)

#Now we will round off the values to 5 decimal points so that it is easy to read
#we can use the round() function to round off values in R
round(CooksDistance, 5)
sort(round(CooksDistance, 5))

#Outlier detection using Cook's Distance
#Multivariate Regression Using Cook's Distance
#Cook's Distance is an estimate of the influence of a data point
#Cook's distance is a measure of how much a regression model changes when the ith observation is removed from the data
library(ISLR)
library(dplyr)
#Let's look at the baseball hitters dataset in ISLR package
head(Hitters)
dim(Hitters)
is.na(Hitters)#check for the missing values
#Now we will remove the missing values using the na.omit() function 
HittersData <- na.omit(Hitters)
dim(HittersData)#checking the dimensions after removing NA values
glimpse(HittersData)
head(HittersData)

#Now we will implement a multivariate regression model using all the features in the dataset to predict the salary of a baseball player
SalaryPredictModel1 <- lm(Salary ~., data = HittersData)
summary(SalaryPredictModel1)
#Multiple r^2: 0.5461, adjusted r^2: 0.5106

#Cook's Distance
cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm = TRUE)))]
influential
#We see that 18 players have a Cook's Distance greater than 3x the mean
#Let's exclude these 18 players and rerun the model to see if we have a better fit for the regression model
names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]
outliers
Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

#Model 2 without Outliers
SalaryPredictModel2 <- lm(Salary ~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)
#Multiple R-squared:  0.6721,	Adjusted R-squared:  0.6445 
#Improved adjusted r^2 from .5106 to .6445 with the removal of just 18 observations

#Shapiro-Wilk Test
#Anderson-Darling Test
#Normality Test
#Normal Distribution
#Read the documentation of the random distribution function
help("rnorm")
set.seed(10)
data1 <- rnorm(50)
set.seed(30)
data2 <- rnorm(50)
#Shapiro-Wilk Normality Test
#Read the documentation of Shapiro-Wilk Test
help("shapiro.test")
#As the test returns a p-value less than 0.05, we reject the null hypothesis and conclude that the data is not normally distributed
shapiro.test(data1)
hist(data1, col='green')
#the histogram shows that the curve is slightly left skewed in nature
shapiro.test(data2)
hist(data2, col='steelblue')#histogram shows that curve is normally distributed in nature
#we set the seed to make the example distributable
set.seed(0)
#create dataset of 100 random values generated from a normal distribution
data <- rnorm(100)
#Perform Shapiro-Wilk test for normality
shapiro.test(data)
#The p-value of the test turns out to be 0.6303
#Since the values is not less than 0.05, we can assume the sample data comes from a population that is not normally distributed
#The result shouldn't be surprising since we generated the sample using the rnorm() function, which generates random values from a normal distribution with mean = 0 and standard deviation = 1

#Example 2: Shapiro-Wilk Test on Non-Normal Data
#The following code shows how to perform a Shapiro-Wilk test on a dataset with sample size n=100 in which the values are randomly generated from a Poisson distribution
#we set the seed to make the example reproducible 
set.seed(0)
#Poisson Distribution
help("rpois")
#create dataset of 100 random values generated from a Poisson distribution
dataP <- rpois(n=100, lambda = 3)
#perform shapiro-wilk test for normality
shapiro.test(dataP)
#The p-value of the test turns out to be 0.0003393
#Since this value is less than 0.05 we have sufficient evidence to say that the sample data doesn't come from a normally distributed population
#This result shouldn't be surprising since we generated the sample data using the rpois() function, which generates random variables from a Poisson distribution
hist(dataP, col = "yellow")
#We can see that the distribution is right-skewed and doesn't have typical bell-shape, so it is not normal distribution
#Histogram matches the results of the Shapiro-Wilk test and confirms that our sample data does not come from a normal distribution
#Anderson-Darling Test for normality
library(nortest)
help("ad.test")
#To conduct an Anderson-Darling Test in R, we can use the ad.test() function within the nortest library
#The following examples shows how to conduct an Anderson-Darling test to check wheter or not a vector of 100 values follows a normal distribution:
#Make sure to install and load "nortest" library
#we use the set seed function to make the example reproducible
set.seed(1)
#defined vector of 100 values that are normally distributed
x <- rnorm(100,0,1)
#conduct Anderson-Darling Test to test for normality
ad.test(x)
#data:  x
#A = 0.16021, p-value = 0.9471
#This test returns 2 values: A: the test statistic, p-value: the correspondnig value of the test statistic
#The null hypothesis for the A-D test is that the data is normally distributed
#Thus, if our p-val is less than our significance level (commmon choices are 0.05 and 0.10), then we can reject the null hypothesis and conclude we have enough evidence to state data is not normally distributed
#In this case, p-val is 0.9471, not below our significance level of 0.05 and will not reject null hypothesis
#Therefore, it is safe to say data follows normal distribution, which makes sense considering we generated 100 values that follow a normal value distribution with a mean of 0 and standard deviation of 1 using the rnorm() function in R

#Suppose instead we generate a vector of 100 values that follow a uniform distribution between 0 and 1
#We can conduct an A-D test once again to see if this data follows a normal distribution
set.seed(1)
#The Uniform Distribution
#defined vector of 100 values that are uniformly distributed
help("runif")
x <- runif(100,0,1)

#conduct A-D test to test for normality
ad.test(x)
#data:  x
#A = 1.1472, p-value = 0.005086
#Our test statistic A equals 1.1472 and the corresponding value is 0.005086
#Since the p-value is less than 0.05, we can reject the null hypothesis and conclude we have sufficient evidence to say this data is not normally distributed
#This matches the result we expected since our data was generated to follow a uniform distribution
