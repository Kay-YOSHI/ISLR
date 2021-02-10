# An Introduction to Statistical Learning with Applications in R
# Ch.6: Linear Model Selection and Regularization

#========================================================================#
# 6.5.1 Best Subset Selection
#========================================================================#

# Load "ISLR" library
library(ISLR)

# We use "Hitters" data set in the "ISLR" library
attach(Hitters)

# Check the number of missing observations in "Salary" variable
fix(Hitters)
sum(is.na(Salary))

# Remove all of the rows that have missing values in any variable
Hitters2 = na.omit(Hitters)

# Check the number of missing values in "Hitters2" data set
sum(is.na(Hitters2))

# Perform best subset selection using "regsubsets()" function in the "leaps" library
library(leaps)
regfit.full = regsubsets(Salary ~ ., data = Hitters2)
summary(regfit.full)

# Fit up to a 19-variable model by specifying "nvmax" option
# By default, "regsubsets()" only reports results up to the eight-variable model
regfit.full19 = regsubsets(Salary ~ ., data = Hitters2, nvmax = 19)
reg.summary = summary(regfit.full19)

# "summary()" function also returns R^2, RSS, adjusted R^2, C_p, and BIC
names(reg.summary)

# Check R^2 for the best model of each size
reg.summary$rsq

# Plot RSS, adjusted R^2, C_p, and BIC
# For each figure, plot a red dot to indicate the model with the largest(smallest) metric
par(mfrow = c(2, 2))

# 1. RSS
plot(reg.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
points(which.min(reg.summary$rss), reg.summary$rss[which.min(reg.summary$rss)], col = "red", cex = 2, pch = 20)

# 2. Adjusted R^2
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 20)

# 3. Cp
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)

# 4. BIC
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l") 
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "red", cex = 2, pch = 20)

# Display the selected variables for the best model with a given number of predictors
# using a built-in "plot()" function in "regsubset()" function
par(mfrow = c(2, 2))
plot(regfit.full19, scale = "r2")
plot(regfit.full19, scale = "adjr2")
plot(regfit.full19, scale = "Cp")
plot(regfit.full19, scale = "bic")

# Coefficient estimates associated with the best model in terms of BIC
coef(regfit.full19, which.min(reg.summary$bic))

#========================================================================#
# 6.5.2 Forward and Backward Stepwise Selection
#========================================================================#

# Perform forward and backward stepwise selection
# using "regsubsets()" function by specifying "method" option

# Forward stepwise selection
regfit.fwd = regsubsets(Salary ~ ., data = Hitters2, nvmax = 19, method = "forward")
summary(regfit.fwd)

# Backward stepwise selection
regfit.bwd = regsubsets(Salary ~ ., data = Hitters2, nvmax = 19, method = "backward")
summary(regfit.bwd)

#========================================================================#
# 6.5.3 Choosing Among Models 
#       Using the Validation Set Approach and Cross-Validation
#========================================================================#

# 1. Validation Set Approach
#============================================

# Split the observations into a training and a test set
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters2), rep = TRUE)
test = (!train)

# Performa best subset selection using the training set
regfit.best = regsubsets(Salary ~ ., data = Hitters2[train, ], nvmax = 19)

# Make a model matrix ("X" matrix) from the test data
test.mat = model.matrix(Salary ~ ., data = Hitters2[test, ])

# Compute the validation set error
val.errors = rep(NA, 19)
for(i in 1:19){
	coefi = coef(regfit.best, id = i)         # Coefficient estimates for model size "i"
	pred = test.mat[, names(coefi)] %*% coefi # Form the predictions multiplying coefficients into the appropriate columns of "X" matrix
	val.errors[i] = mean( (Hitters2$Salary[test] - pred)^2 )
}

# The best model size is the one that achieves the lowest "val.errors"
# The best model is 7-variable model (Different from the textbook result)
which.min(val.errors)

# Write our own "predict" function
predict.regsubsets = function(object, newdata, id, ...){
	form = as.formula(object$call[[2]]) # Estimation equation
	mat = model.matrix(form, newdata)   # "X" matrix
	coefi = coef(object, id = id)       # Coefficient estimates for model size "id"
	xvars = names(coefi)                # Variable names in "X" corresponding to "coefi"
	return(mat[, xvars] %*% coefi)      # Return predictions
}

# Perform best subset selection on the full data set and select the best model
regfit.best.final = regsubsets(Salary ~ ., data = Hitters2, nvmax = 19)
coef(regfit.best.final, id = which.min(val.errors))

# 2. Cross-Validation Approach
#============================================

# Create a vector that allocates each observation to one of k = 10 folds
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters2), replace = TRUE)

# Create a matrix in which we will store the results
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# Perform cross-validation
for(j in 1:k){
	best.fit = regsubsets(Salary ~ ., data = Hitters2[folds != j, ], nvmax = 19)
	for(i in 1:19){
		pred = predict(best.fit, Hitters2[folds == j, ], id = i)
		cv.errors[j, i] = mean( (Hitters2$Salary[folds == j] - pred)^2 )
	}
}

# Cross-validation error for each model size
mean.cv.errors = apply(cv.errors, 2, mean)

# Plot the cross-validation error by model size
# Cross-validation selects a 10-variable model (Different from the textbook result)
plot(mean.cv.errors, type = "b")
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], col = "red", cex = 2, pch = 20)

# Perform best subset selection on the full data set to obtain the 10-variable model
regfit.best.final2 = regsubsets(Salary ~ ., data = Hitters2, nvmax = 19)
coef(regfit.best.final2, id = 10)

#========================================================================#
# 6.6.1 Ridge Regression
#========================================================================#

# Load "glmnet" library
library(glmnet)

# Create a "X" matrix and response "Y"
x = model.matrix(Salary ~ ., data = Hitters2)[, -1] # "Intercept" is removed
y = Hitters2$Salary

# Fit a ridge regression model
# alpha = 0: ridge regression model is fit
# alpha = 1: Lasso model is fit
grid = 10^seq(10, -2, length = 100) # grid of values for lambda (100 candidates)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

# We obtain 20 coefficient estimates for each value of lambda
dim(coef(ridge.mod))

# Coefficient estimates are smaller(larger) when lambda is large(small)
coef(ridge.mod)[, 50] # lambda = 11497.57
coef(ridge.mod)[, 60] # lambda = 705.48

# We can obtain the coefficient estimates for a new value of lambda, say 50 using "predict()" function
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

# Split the observations into a training and a test set
#==========================================================
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

# Fit a ridge regression model using the training set
ridge.mod2 = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)

# 1. Predictions on the test set using lambda = 4
ridge.pred2 = predict(ridge.mod2, s = 4, newx = x[test, ])

# Evaluate test MSE
# Different result from the textbook
mean( (ridge.pred2 - y.test)^2 )

# 2. Predictions on the test set using lambda = 10^10 (null model containing only an intercept)
# Different result from the textbook
ridge.pred3 = predict(ridge.mod2, s = 1e10, newx = x[test, ])
mean( (ridge.pred3 - y.test)^2 )
#mean( (mean(y[train]) - y.test)^2 )

# 3. Predictions on the test set using lambda = 0 (least squares regression)
# Different result from the textbook
ridge.pred4 = predict(ridge.mod2, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean( (ridge.pred4 - y.test)^2 )

# Choose the tuning parameter "lambda" using cross-validation
#==========================================================
# By default, "cv.glmnet()" function performs ten-fold cross-validation.
# This can be changed using "nfolds" option

# Perform cross-validation
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)

# Plot MSE by lambda
plot(cv.out)

# Best lambda that achives smallest MSE
# Different result from the textbook
bestlambda = cv.out$lambda.min

# Test MSE associated with the best lambda
# Different result from the textbook
ridge.pred5 = predict(ridge.mod2, s = bestlambda, newx = x[test, ])
mean( (ridge.pred5 - y.test)^2 )

# Refit a ridge regression model on the full data set using "bestlambda"
# Different result from the textbook
out.ridge = glmnet(x, y, alpha = 0)
predict(out.ridge, type = "coefficients", s = bestlambda)[1:20, ]

#========================================================================#
# 6.6.2 The Lasso
#========================================================================#

# Fit a lasso model using the training set
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

# Plot the standardized lasso coefficients as a function of L1 norm
plot(lasso.mod)

# Perform cross-validation
set.seed(1)
cv.out2 = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out2)
bestlambda2 = cv.out2$lambda.min

# Compute the test error associated with the best lambda
# Different result from the textbook
lasso.pred = predict(lasso.mod, s = bestlambda2, newx = x[test, ])
mean( (lasso.pred - y.test)^2 )

# Lasso's resulting coefficient estimates are sparse 
# Eight of the 19 coefficient estimates are exactly zero
out.lasso = glmnet(x, y, alpha = 1, lambda = grid)
predict(out.lasso, type = "coefficients", s = bestlambda2)[1:20, ]

#========================================================================#
# 6.7.1 Principal Components Regression
#========================================================================#

# Load "pls" library
library(pls)

# Perform principal components regression (PCR)
# scale = TRUE : Standardizing each predictor
# validation = "CV" : Compute the ten-fold CV error for each possible value of M, the number of principal components used
set.seed(2)
pcr.fit = pcr(Salary ~ ., data = Hitters2, scale = TRUE, validation = "CV")
summary(pcr.fit)

# Plot the CV errors (MSE) as a function of the number of principal components
validationplot(pcr.fit, val.type = "MSEP")

# Perform PCR on the training data
# The lowest CV error occurs when M = 5 (Different from the textbook result)
set.seed(1)
pcr.fit1 = pcr(Salary ~ ., data = Hitters2, subset = train, scale = TRUE, validation = "CV")
summary(pcr.fit1)
validationplot(pcr.fit1, val.type = "MSEP")

# Compute the test MSE when M = 5
pcr.pred = predict(pcr.fit1, x[test, ], ncomp = 5)
mean( (pcr.pred - y.test)^2 )

# Fit PCR on the full data set using M = 5
pcr.fit.full = pcr(y ~ x, scale = TRUE, ncomp = 5)
summary(pcr.fit.full)

#========================================================================#
# 6.7.2 Partial Least Squares
#========================================================================#

# Implement partial least squares (PLS)
# The lowest CV error occurs when M = 1 (Different from the textbook result)
set.seed(1)
pls.fit = plsr(Salary ~ ., data = Hitters2, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# Compute the test MSE when M = 1
pls.pred = predict(pls.fit, x[test, ], ncomp = 1)
mean( (pls.pred - y.test)^2 )

# Fit PLS on the full data set using M = 1
pls.fit.full = plsr(Salary ~ ., data = Hitters2, scale = TRUE, ncomp = 1)
summary(pls.fit.full)
