#Dimensionality Reduction / PCA with IRIS data set
data("iris")
head(iris)
#creating another data set from iris data set that contains columns form 1 to 4
irisdata1 <- iris[,1:4]
head(irisdata1)
#Read the documentation for the princomp() function in RStudio
help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
#cor = a logical value indicating whether the calculation should use the correlation matrix or the covariance matrix
#(The correlation matrix can only be used uf there are no constant variables)
#score = a logical value indicating whether the score on each principal component should be calculated
summary(principal_components)
#Proportion of Variance 0.7296245 0.2285076 0.03668922 0.005178709
#Component 1 represents 72.96% of the variation in the dataset
#Component 2 represents 22.85% of the variation in the dataset
#There are 4 components, but the first 2 components together collectively represent 95.7%
#using the plot() function, we can plot the principal components
plot(principal_components)
#plotting the principal_components using the line in plot() functions
plot(principal_components, type = "l")
#using the biplot() function we can plot the components
#read the documentation for biplot() function in RStudio
help("biplot")
biplot(principal_components)

data(Boston, package = "MASS")
#read the documentation of boston dataset in rstudio to understand the dataset
help("Boston")
#Principal Component Analysis
#the prcomp() function computes the principal components and we have turned on scaling
#read the documentation for prcomp() function in rstudio
help("prcomp")
pca_out <- prcomp(Boston, scale. = T)
#pca_out shows the loadings that were used
pca_out
plot(pca_out)

#plotting using the biplot()
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
#boston_pc has the Principal Components having the same number of rows in the original dataset
head(boston_pc)
summary(boston_pc)

#PCA on USArrests data set, part of R base package
#The row of the data set contains the 50 states in alphabetical order
#We will use the USAArrest data that is available on RStudio
data("USArrests")
help("USArrests") #violent crime rates by US state: murder, assault, urban population, rape
states = row.names(USArrests)
states
#The columns of the data set contain the four variables
names(USArrests)

#We first briefly examine the data. We notice that the variables have vastly different means
#Note that the apply() function allows us to apply a function - in this case the mean() function to each row or column of the data set
#The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns, 2
#We see that there are on average three times as many rapes a murders and more than right times as many assaults as rapes
apply(USArrests, 2, mean)
#Murder Assault UrbanPop    Rape 
#7.788  170.760   65.540   21.232 
#We can also examine the variances of the four variables using the apply() function
apply(USArrests, 2, var)
#Murder    Assault   UrbanPop       Rape 
#18.97047 6945.16571  209.51878   87.72916
#Not surprisingly, the variables also have vastly different variances: the UrbanPop variable measures the percentage of the population
#in each state living in an urban area, which is not a comparable number to the number of rapes in each state per 100,000 individuals
#If we failed to scale the variables before performing PCA, then most of the principal components that we observed would be driven by the Assault variable,
#since it has by fat the largest mean and variance. Thus, it is important to standardize the variables to have mean zero and standard deviation one before performinc PCA

#We now perform principal components analysis using the prcomp() function, which is one of several functiona in R that perform PCA
#By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, we scale the variables to have STD one
#The output from prcomp() contains a number of useful quantities
pr.out = prcomp(USArrests, scale. = TRUE)
names(pr.out)
#[1] "sdev"     "rotation" "center"   "scale"    "x" 
#The center and scale components correspond to the means and STDs of the variables that were used for scaling prior to implementing PCA
pr.out$center 
#7.788  170.760   65.540   21.232
pr.out$scale 
#4.355510 83.337661 14.474763  9.366385

#The r0tation matrix provides the principal component loadings; each column of pr.out$rotation contains the corresponding principal component loading vector
#We see that there are four distinct principal components
pr.out$rotation
#                PC1        PC2        PC3         PC4
#Murder   -0.5358995 -0.4181809  0.3412327  0.64922780
#Assault  -0.5831836 -0.1879856  0.2681484 -0.74340748
#UrbanPop -0.2781909  0.8728062  0.3780158  0.13387773
#Rape     -0.5434321  0.1673186 -0.8177779  0.08902432

#Using the prcomp() function, we do not need to explicitly multiply the data by the principal component loading vectors in order to obtain the principal component score vectors
#Rather the 50x4 matrix x has as its columns the principal component score vectors
#That is, the kth column is the kth principal component score vector
dim(pr.out$x)
#[1] 50  4

#We can plot the first two principal components as follows
biplot(pr.out, scale = 0)
#The scale = 0 argument to biplot() ensures that the arrows are scaled to represent the loadings; other values for scale give slightly different biplots w/ different interpretations
#The prcomp() function also outputs the standard deviation of each principal component
#For isntance, on the USArrests data set, we can access these STDs as follows
pr.out$sdev
#[1] 1.5748783 0.9948694 0.5971291 0.4164494

#The variance explained by rach principle component is obtained by square std
pr.var = pr.out$sdev^2
pr.var
#[1] 2.4802416 0.9897652 0.3565632 0.1734301

#To compute the proportion of variance explained by each principal component, we simply devide the variance explained by each principal compontent 
#by the total variance explained by all four principal components
pve = pr.var/sum(pr.var)
pve
#[1] 0.62006039 0.24744129 0.08914080 0.04335752
#We can see the first principal component explain 62.00% of the variance in the dat
#The next principal component explains 24.74% of the variance, etc.