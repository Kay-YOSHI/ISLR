# An Introduction to Statistical Learning with Applications in R
# Ch.9: Support Vector Machines

# Load "e1071" library
library(e1071)

#========================================================================#
# 9.6.1 Support Vector Classifier
#========================================================================#

# Generate two class observations
set.seed(1)
x = matrix(rnorm(20 * 2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
x[y == 1, ] = x[y == 1, ] + 1

# Check whether the classes are linearly separable
# They are not linearly separable
plot(x, col = (3 - y))

# Fit the support vector classifier
#==================================================

# Encode the response as a factor variable
dat = data.frame(x = x, y = as.factor(y))

# Fitting
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)

# Plot the support vector classifier obtained
plot(svmfit, dat)

# Identify support vectors
svmfit$index

# Some basic information about the fitted model
summary(svmfit)

# Change the cost parameter to smaller one
svmfit2 = svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit2, dat)
svmfit2$index

# Cross-validation for the cost parameter
#==================================================
# By default, "tune" function performs ten-fold CV
set.seed(1)
tune.out = tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

# Access CV errors
summary(tune.out)

# Extract the best model stored in "tune.out"
bestmod = tune.out$best.model
summary(bestmod)

# Prediction using a test data
#==================================================

# Generate a test data set
xtest = matrix(rnorm(20 * 2), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))

# Predict the class labels using the best model and the test data
ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

# What if we had instead used cost = 0.01?
svmfit3 = svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred3 = predict(svmfit3, testdat)
table(predict = ypred3, truth = testdat$y)

# Linearly separable case
#==================================================

# Generate Linearly separable data
x2 = x
x2[y == 1, ] = x2[y == 1, ] + 0.5
plot(x2, col = (y + 5)/2, pch = 19)

# Fit the support vector classifier
dat2 = data.frame(x = x2, y = as.factor(y))
svmfit4 = svm(y ~ ., data = dat2, kernel = "linear", cost = 1e+05)
summary(svmfit4)
plot(svmfit4, dat2)

# Try a smaller value of cost
svmfit5 = svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit5)
plot(svmfit5, dat2)

#========================================================================#
# 9.6.2 Support Vector Machine
#========================================================================#

# Generate data with a non-linear class boundary
set.seed(1)
x3 = matrix(rnorm(200 * 2), ncol = 2)
x3[1:100, ] = x3[1:100, ] + 2
x3[101:150, ] = x3[101:150, ] - 2
y2 = c(rep(1,150), rep(2, 50))
dat3 = data.frame(x = x3, y = as.factor(y2))

# Plot the data
plot(x3, col = y2)

# Fit SVM with a radial kernel using training data
#==================================================
# Assume gamma = 1 
train = sample(200, 100)
svmfit6 = svm(y ~ ., data = dat3[train, ], kernel = "radial", gamma = 1, cost = 1)
summary(svmfit6)
plot(svmfit6, dat3[train, ])

# Change the cost parameter to larger one
svmfit7 = svm(y ~ ., data = dat3[train, ], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit7, dat3[train, ])

# Cross-validation for "gamma" and "cost"
#==================================================
set.seed(1)
tune.out2 = tune(svm, y ~ ., data = dat3[train, ], kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out2)

# Predictions with best gamma and cost values
table(true = dat3[-train, "y"], pred = predict(tune.out2$best.model, newdata = dat3[-train, ]))

#========================================================================#
# 9.6.3 ROC Curves
#========================================================================#

# Load "ROCR" library
library(ROCR)

# Write a function to plot an ROC curve
rocplot = function(pred, truth, ...){
	predob = prediction(pred, truth)
	perf = performance(predob, "tpr", "fpr")
	plot(perf, ...)
}

# Fit SVM with the best parameters
svmfit.opt = svm(y ~ ., data = dat3[train, ], kernel = "radial", gamma = 0.5, cost = 1, decision.values = TRUE)

# Obtain the fitted values
fitted.opt = attributes(predict(svmfit.opt, dat3[train, ], decision.values = TRUE))$decision.values

# Produce the ROC plot
par(mfrow = c(1, 2))
rocplot(fitted.opt, dat3[train, "y"], main = "Training Data")

# Increase gamma
svmfit.flex = svm(y ~ ., data = dat3[train, ], kernel = "radial", gamma = 50, cost = 1, decision.values = TRUE)
fitted.flex = attributes(predict(svmfit.flex, dat3[train, ], decision.values = TRUE))$decision.values
rocplot(fitted.flex, dat3[train, "y"], add = TRUE, col = "red")

# ROC curves on the test data
fitted.opt.test = attributes(predict(svmfit.opt, dat3[-train, ], decision.values = TRUE))$decision.values
rocplot(fitted.opt.test, dat3[-train, "y"], main = "Test Data")
fitted.flex.test = attributes(predict(svmfit.flex, dat3[-train, ], decision.values = TRUE))$decision.values
rocplot(fitted.flex.test, dat3[-train, "y"], add = TRUE, col = "red")

#========================================================================#
# 9.6.4 SVM with Multiple Classes
#========================================================================#

# Generate data with multi class
set.seed(1)
x4 = rbind(x3, matrix(rnorm(50 * 2), ncol = 2))
y3 = c(y2, rep(0, 50))
x4[y3 ==0, 2] = x4[y3 == 0, 2] + 2
dat4 = data.frame(x = x4, y = as.factor(y3))

# Plot the data
par(mfrow = c(1, 1))
plot(x4, col = (y3 + 1))

# Fit an SVM
svmfit.mult = svm(y ~ ., data = dat4, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit.mult, dat4)

#========================================================================#
# 9.6.5 Application to Gene Expression Data
#========================================================================#

# Load "ISLR" library
library(ISLR)

# We use "Khan" data set
# ・83 tissue samples
# ・y: Four distinct types of small round blue cell tumors
# ・x: Gene expression measurements for 2,308 genes
dat.gene = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))

# Fit an SVM with linear kernel
out.gene = svm(y ~ ., data = dat.gene, kernel = "linear", cost = 10)
summary(out.gene)

# Predictions on the test data
dat.gene.test = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.gene.test = predict(out.gene, newdata = dat.gene.test)
table(pred.gene.test, dat.gene.test$y)
