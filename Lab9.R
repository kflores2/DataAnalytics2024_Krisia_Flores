#Fitting regression trees
library(MASS)
library(tree)
set.seed(1)
head(Boston)
help("sample")
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)
#Note that the output summary() indicates that only four of the variables have been used for constructing the tree
#In the context of a regression tree, the deviance is simply the sum of squared errors for the tree

#Regression Tree
tree(formula = medv~., data = Boston, subset = train)
#We can now plot the tree
plot(tree.boston)
text(tree.boston, pretty = 0)
#The variable "lstat" measures the percentage of the individuals with lower socioeconomic status
#The tree indicates that the lower values of lstat correspond to more expensive houses

#We can us the cv.tree() function to see whether pruning the tree will improve performance
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, typ = "b")

#In this case, the most complex tree is selected by cross-validation
#However, if we wish to prune the tree, we could do as follows using the prune.tree() function

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
#In keeping with the cross validation results, we use the unpruned tree to make predictions on the test set
yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test)
#adding the abline()
abline(0,1)
mean((yhat-boston.test)^2)
#In other words, the test set MSE associated with the regression tree is 35.29
#The square root of the MSE is therefore around 5.941, indicating that the model leads to test predictions that are within $5,941 of the true median home value for the suburb

