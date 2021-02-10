# An Introduction to Statistical Learning with Applications in R
# Ch.4: Classification

# Load "ISLR" libraries
library(ISLR)

# We use "Smarket" data set in the "ISLR" library
attach(Smarket)

#========================================================================#
# 4.6.2 Logistic Regression
#========================================================================#

# Fit a logistic regression model
# to predict "Direction" using "Lag1" through "Lag5" and "Volume"
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)

# Predict the probability that the market will go up given values of the predictors
# type = "response" tells R to output probabilities as opposed to other info such as logit.
# If no data set is supplied to the "predict()" function, 
# then the probabilities are computed for the training data used to fit the model
glm.probs = predict(glm.fits, type = "response")

# Class predictions based on whether "glm.probs" > 0.5 or not
glm.pred = rep("Down", dim(Smarket)[1])
glm.pred[glm.probs > 0.5] = "Up"

# Confusion matrix to determine how many observations were correctly or incorrectly classified
table(glm.pred, Direction)

# Split the data into training and test data
# Training: Observations from 2001 to 2004
# Test:     Observations from 2005
train = (Year < 2005) # train: Boolean vector (= TRUE if Year <2005, = FALSE otherwise)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]

# Fit a logistic regression model using training data and 
# predict the probability that the market will go up using test data
glm.fits2 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs2 = predict(glm.fits2, Smarket.2005, type = "response")

# Class predictions
glm.pred2 = rep("Down", dim(Smarket.2005)[1])
glm.pred2[glm.probs2 > 0.5] = "Up"
table(glm.pred2, Direction.2005)

# Refit the logistic regression model using just "Lag1" and "Lag2"
glm.fits3 = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs3 = predict(glm.fits3, Smarket.2005, type = "response")
glm.pred3 = rep("Down", dim(Smarket.2005)[1])
glm.pred3[glm.probs3 > 0.5] = "Up"
table(glm.pred3, Direction.2005)

# Prediction the probability that the market will go up 
# given specific values of "Lag1" and "Lag2"
predict(glm.fits3, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

#========================================================================#
# 4.6.3 Linear Discriminant Analysis
#========================================================================#

# Load "MASS" libraries
library(MASS)

# Fit an LDA model using training data
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit # Display the estimation results

# Produce plots of the linear discriminants
plot(lda.fit)

# Prediction using test data
lda.pred = predict(lda.fit, Smarket.2005)

# LDA and logistic regression predictions are almost identical
lda.class = lda.pred$class
table(lda.class, Direction.2005) # Classification from LDA
table(glm.pred3, Direction.2005) # Classification from logistic regression

# Predictions with posterior probabilities (50% threshold)
# This is identical to the above table
sum(lda.pred$posterior[, 1] >= 0.5) # Number of "Down" predictions
sum(lda.pred$posterior[, 1] < 0.5)  # Number of "UP" predictions

# Predictions with posterior probabilities (90% threshold)
sum(lda.pred$posterior[, 1] > 0.9) # Number of "Down" predictions

#========================================================================#
# 4.6.4 Quadratic Discriminant Analysis
#========================================================================#

# Fit a QDA model using training data
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

# Prediction using test data
# "predict()" function works in exactly the same fashion as for LDA
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

#========================================================================#
# 4.6.5 K-Nearest Neighbors
#========================================================================#

# Load "class" libraries
library(class)

# Combine "Lag1" and "Lag2" to create predictor matrix
train.X = cbind(Lag1, Lag2)[train, ] # For training data
test.X = cbind(Lag1, Lag2)[!train, ] # For test data

# Create a class label vector associated with the training data
train.Direction = Direction[train]

# Predict the market's movement using KNN (K = 1) for test data
set.seed(1)
knn.pred1 = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred1, Direction.2005)

# Predict the market's movement using KNN (K = 3) for test data
knn.pred3 = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred3, Direction.2005)

# Compare model's accuracy on test data
mean(glm.pred3 == Direction.2005) # Logistic regression
mean(lda.class == Direction.2005) # LDA
mean(qda.class == Direction.2005) # QDA
mean(knn.pred3 == Direction.2005) # KNN

#------------------------------------------------------------------------#
# [OPTIONAL]
# KNN test error rate by K
#------------------------------------------------------------------------#
set.seed(1)
ter = numeric(10)
for(k in 1:10){
	knn.pred = knn(train.X, test.X, train.Direction, k = k)
	ter[k] = mean(knn.pred != Direction.2005)
}

# Visualize
library(ggplot2)
xax = 1:10
data = data.frame(xax, ter)
ggplot(data, aes(x = xax, y = ter)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 10)) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  labs(x = "K", y = "Test error rate")

#========================================================================#
# 4.6.6 An Application to Caravan Insurance Data
#========================================================================#

# We use "Caravan" data set in the "ISLR" library
attach(Caravan)

# Standardization
# The KNN classifier highly depends on the scale of the variables
standardized.X = scale(Caravan[, -86]) # Column 86 is the response variable "Purchase"

# Split the observations into test and training data
# Test: the first 1,000 observations
# Training: the remaining observations
test = 1:1000
train.X = standardized.X[-test, ] # Training X
train.Y = Purchase[-test]       # Training Y
test.X = standardized.X[test, ]   # Test X
test.Y = Purchase[test]         # Test Y

# Fit a KNN model on the training data with K = 1, 3, and 5
set.seed(1)
knn.pred1 = knn(train.X, test.X, train.Y, k = 1)
knn.pred3 = knn(train.X, test.X, train.Y, k = 3)
knn.pred5 = knn(train.X, test.X, train.Y, k = 5)

# Test error rate
mean(test.Y != knn.pred1)
mean(test.Y != knn.pred3)
mean(test.Y != knn.pred5)

# Suppose that the fraction of individuals that are correctly predicted to buy insurance 
# is of interest, not the overall error rate
table(knn.pred1, test.Y)
table(knn.pred3, test.Y)
table(knn.pred5, test.Y)

# Comparison: Fit a logistic regression model with cut-off prob. 0.5 and 0.25
glm.fits = glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fits, Caravan[test, ], type = "response")
glm.pred.5 = rep("No", 1000)
glm.pred.5[glm.probs > 0.5] = "Yes"
glm.pred.25 = rep("No", 1000)
glm.pred.25[glm.probs > 0.25] = "Yes"
table(glm.pred.5, test.Y)
table(glm.pred.25, test.Y)