#Exercise 1: Heatmap, Image, Hierarchical Cluster
#creating a matrix with random #s and plotting it using image() function
#you will see, it does not have a real pattern with the plot
set.seed(12345)
help(par)
#par can be used to set or query graphical parameters
#parameters can be set by specifying them as arguments
#to par in tag = value form, or by passing them as a list of tagged values
par(mar = rep(0.2,4))
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
help(heatmap)
par(mar = rep(0.2,4))
heatmap(data_Matrix)
help(rbinom)
set.seed(678910)
for(i in 1:40){
  #flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  #if the coin is heads, add a common pattern to that row
  #five of the columns will have a mean of 0, half of 3
  if(coin_Flip){
    data_Matrix[i,] <- data_Matrix[i,] + rep(c(0,3), each=5)
  }
}
#re-run image and heatmap code to see pattern matrix
par(mar = rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
par(mar = rep(0.2,4))
heatmap(data_Matrix)

#Let's take a closer look at the patterns in rows and columns 
#by looking at the marginal means of rows and columns.
#ten different columns mean and forty different rows mean
hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch=19)
#left plot has original data reordered according to the hierarchical cluster analysis of the rows
#middle plot had mean of each of the rows (40 rows, so 40 dots representing the mean)
#right plot has mean of each of the columns (10 columns, so 10 dots representing the mean)

#Exercise 2: Classification
#use other measurements in the abalone dataset to predict age
# abalone dataset from UCI repository
# reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight','rings' )
summary(abalone)
names(abalone)
str(abalone)
#will do predictions by young, adult and old classification
abalone$rings <- as.numeric(abalone$rings) #since all other data types are numeric, turn rings into numeric too
max(abalone$rings) #max age of abalone is 29
abalone$rings <- cut(abalone$rings, breaks = c(-1,8,11,35), labels = c("Youngins", "Adults", "Oldies")) #classify into categories using cut
abalone$rings <- as.factor(abalone$rings) #turn from vector into factor type

#data clean sex variable from abalone dataset since it is a non numeric factor and will throw KNN error
data_Abalone <- abalone #create new df to work on
names(data_Abalone)
data_Abalone$sex <- NULL #clean sex to null
head(data_Abalone$sex)
#normalize data, as this is a standard and recommended practice in KNN processing
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
length(data_Abalone)
data_Abalone[1:7] <- as.data.frame(lapply(data_Abalone[1:7], normalize)) #choose 1:7 to only normalize independent variables
summary(data_Abalone)
#split into training and testing datasets
indices <- sample(2, nrow(data_Abalone), replace = TRUE, prob = c(0.7,0.3))
dataTrain <- data_Abalone[indices==1,]
dataTest <- data_Abalone[indices==2,]
sqrt(2918)
# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852  round it to 55 and use k = 55 # We usually take an Odd number for k value, 
library(class)
KNNpred <- knn(train = dataTrain[1:7], test = dataTest[1:7], cl = dataTrain$rings, k = 55)
KNNpred
table(KNNpred)

#Exercise 3: Clustering
library(ggplot2)
data(iris)
iris
names(iris)
str(iris)
iris_noSpecies <- iris[,-5]
head(iris_noSpecies)
set.seed(21)
k <- 3
kMeansResult <- kmeans(iris_noSpecies, centers = k, nstart = 1000)
table(iris$Species, kMeansResult$cluster)