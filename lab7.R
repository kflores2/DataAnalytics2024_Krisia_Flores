#PCA on Wide dataset from UCI repo
# There are 13 variables in the dataset such as Alcohol, Malic Acid, Ash, Alkalinity of Ash, Magnesium, ...
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
#Header row is not available in the data, therefore, we need to add variable names
head(wine_data)

#The first variable, which is the ultivar that is used to identify the Cv1, Cv2, Cv3
#Cv1 represent the cultivar1, Cv2 represent the cultivar2, and Cv3 represent the cultivar3
nrow(wine_data) #178 rows
dim(wine_data) #178 rows, 14 cols

#Adding the variable names 
colnames(wine_data) <- c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
head(wine_data)#Now you can see the header names

#Using the heatmap() function, we can check the correlations
#In the Heatmap(), the "Dark Colors" represent the "Correlated"
#In the Heatmap(), the "Light Colors" represent the "Not or Less Correlated"
help("heatmap") #Read the heatmap() function to show the correlation among variables
#Now we will use the heatmap function to show the correlation among variables
help("corr")
heatmap(cor(wine_data), Rowv = NA, Colv = NA) #rowv and colv variables 

#Our goal is to identify the 3 variates based on the chemical data on the wine dataset
#In order to make it easy to identify the 3 cultivars, we will declare 3 classes that represent each cultivar (Cv1, Cv2, Cv3) by using the factor() function in R
#Read the documentation in Rstudio for the factor() function
help("factor")
#declaring the cultivar_classes using the factor() function for each cultivar Cv1, Cv2, Cv3
cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes

#Now we will use PCA
#The default built in function in R for PCA is prcomp() function
#Read the documentation of prcomp() function in Rstudio
help("prcomp")

#We will normalize the wine data to a common scale using scale() function so that the PCA process will not overweight variables that happen to have the larger values
#Read the documentation of scale() function in Rstudio
help("scale")
#We will not normalize the Cvs variable (1st column) so we exclude the Cvs column with -1
wine_data_PCA <- prcomp(scale(wine_data[,-1]))

#We can use the summary() function on wine_data_PCA to see the cumulative proportion that each principal component contributes
summary(wine_data_PCA)

#Interpretation
#We can see that PC1 gives 36.2% cumulative contribution, which tells us that PC1 represents 36.2% variance of the data
#You can also see that we can choose to have 8 variables from the total of 13 (choosing 8 out of 13) with only about 8% of loss of the cumulative contribution value