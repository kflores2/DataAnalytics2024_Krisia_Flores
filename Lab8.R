#PCA with Iris dataset
data("iris")
head(iris)
#create another dataset from iris dataset that contains colums 1 to 4
irisdata1 <- iris[,1:4]
irisdata1
head(irisdata1)
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
#In the summary, you can see there are four principal components bc the input data has four different features
#Using the plot() function, we can plot the principal components
plot(principal_components)
#We can also plot the using the a line in plot() function
plot(principal_components, type = "l")
#Using rwh biplot() function, we can plot the components
biplot(principal_components)

#We will conduct PCA on wine dataset from UCI
# Read the data using the read.table()
# Read the documentation for the UCI wine dataset, in the documentation, 
# Cvs stands for the "cultivars" (varieties) of the class of the wine,
# cultivar are similar to wine classes Pinot Noir,Shiraz,Muscat
# Goal is to identify the membership of the wine in 1 of 3 cultivars.
# There are 13 variables in the dataset such as Alcohol, Malic Acid, Ash, Alkalinity of Ash, Magnesium, ...
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
# Header row is not available in the data, therefore, we need to add the variable names
head(wine_data)

#The first variable is the cultivar which is used to identify the Cv1, Cv2, Cv3
#Cv1 represents cultivar1, and so on
nrow(wine_data) #There are 178 rows

#Add the variable names to the data
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data) # Now you can see the header names.

#Using the heatmap() function, we can check the correlation
#In the heatmap, dark color represent correlated, light color represent not or less correlated
heatmap(cor(wine_data), Ronv = NA, Colv = NA)

#Our goal is to identify the 3 variates based on the chemical data of the wine dataset
#In order to make it easy to identify the 3 cultivar variates, we will declare 3 classes that represent each cultivar (Cv1, Cv2, Cv3) by using the factor() function in R
cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes

#Now we will use PCA to determine the factors causing the most variability
#We will normalize the wine data to a common scale using scale() function so that the PCA process will not overweight the variables that happen to have larger values
#We will not normalize the Cvs variable, so we exclude that column with the -1
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
#We can use the summary function on wine_data_PCA to see the cumulative proportion of each principal component contributes
summary(wine_data_PCA)
#We can see that PC1 gives the 36.2% cumulative contribution, which tells us that PC1 represents 36.2% variance of the data.
#You can see that we can choose to have 8 variables from the total of 13 (choosing 8 out of 13) with only about 8% of loss of cumulative contribution value


#Practice Plots
library(gcookbook)
ggplot(BOD, aes(x=Time, y=demand))+geom_line()
BOD
BOD1 <- BOD #make copy of dataset
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) +geom_line()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0)
#Adding points to a line graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
#same with log-y axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10()
