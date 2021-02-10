# An Introduction to Statistical Learning with Applications in R
# Ch.8: Tree-Based Methods

#========================================================================#
# 8.3.1 Fitting Classification Trees
#========================================================================#

# Load "tree" and "ISLR" library
library(tree)
library(ISLR)

# We use "Carseats" data set in the "ISLR" library
attach(Carseats)

# Create a binary variable from a continuous variable "Sales"
# Caution: If a response variable is "character" variable, 
#          "tree" function returns warning and "summary" function cannot be applied.
High = as.factor(ifelse(Sales <= 8, "No", "Yes"))
#High = ifelse(Sales <= 8, 0, 1)

# Merge "High" with the rest of the Carseats data
Carseats2 = data.frame(Carseats, High)

# Fit a classification tree
tree.carseats = tree(High ~ . - Sales, data = Carseats2)
summary(tree.carseats)

# Display the tree structure
# pretty = 0: Display category names for qualitative predictors, rather than letters like a, b
plot(tree.carseats)
text(tree.carseats, pretty = 0) # add node labels

# Split observations into training and test data
set.seed(2)
train = sample(1:nrow(Carseats2), 200)
Carseats.test = Carseats2[-train, ]    # Test X
High.test = High[-train]               # Test Y

# Fit a classification tree using the training data
tree.carseats2 = tree(High ~ . - Sales, data = Carseats2, subset = train)

# Class prediction using the test data
tree.pred = predict(tree.carseats2, Carseats.test, type = "class")
table(tree.pred, High.test) # Caution: Results are different from the book (due to the R version?)

# Test error rate
mean(tree.pred != High.test)

# Pruning the tree
set.seed(3)
cv.carseats = cv.tree(tree.carseats2, FUN = prune.misclass)

# Plot the cross-validation error rate as a function of both "size" and "k"
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Prune the tree to obtain the best tree
prune.carseats = prune.misclass(tree.carseats2, best = 21)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Class prediction using pruned tree
tree.pred2 = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred2, High.test)

# Test error rate
mean(tree.pred2 != High.test)

#========================================================================#
# 8.3.2 Fitting Regression Tree
#========================================================================#

# Load "MASS" library
library(MASS)

# We use "Boston" data set in the "MASS" library
attach(Boston)

# Create a training data
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Fit a regression tree using training data
tree.boston = tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston) # Caution: Results are different from the book (due to the R version?)

# Plot the tree
plot(tree.boston)
text(tree.boston, pretty = 0)

# Pruning the tree and plot the sum of squared errors as a function of "size"
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

# Prune the tree to obtain the best tree (In this case, same as "tree.boston")
prune.boston = prune.tree(tree.boston, best = 7)
plot(prune.boston)
text(prune.boston, pretty = 0)

# Prediction using test data
yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]

# Plot observations and predictions
plot(yhat, boston.test)
abline(0, 1)            # Line "y=x"

# Test set MSE
mean((yhat - boston.test)^2)

#========================================================================#
# 8.3.3 Bagging and Random Forests
#========================================================================#

# Load "randomForest" library
library(randomForest)

# Perform bagging using "Boston" data set
#===============================================#
# mtry: Number of predictors considered for each split of the tree (= m)
# Number of trees is set to 500 as default
set.seed(1)
bag.boston = randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)

# Prediction using test data
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])

# Plot observations and predictions
plot(yhat.bag, boston.test)

# Test set MSE
mean((yhat.bag - boston.test)^2)

# Change the number of trees grown from 500 to 25
bag.boston2 = randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag2 = predict(bag.boston2, newdata = Boston[-train, ])
mean((yhat.bag2 - boston.test)^2)

# Perform random forest with m = 6
#===============================================#
set.seed(1)
rf.boston = randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)

# View the importance of each variable and plot them
importance(rf.boston)
varImpPlot(rf.boston)

#========================================================================#
# 8.3.4 Boosting
#========================================================================#

# Load "gbm" library
library(gbm)

# Fit a boosted regression tree using "Boston" data set
# Shrinkage parameter is set to 0.001 as default
set.seed(1)
boost.boston = gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

# Partial dependence plots for the variables "rm" and "lstat"
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# Prediction using the test data
yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)

# Test set MSE
mean((yhat.boost - boston.test)^2)

# Change the shrinkage parameter from 0.001 to 0.2
boost.boston2 = gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = FALSE)
yhat.boost2 = predict(boost.boston2, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost2 - boston.test)^2)
