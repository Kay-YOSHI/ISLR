# An Introduction to Statistical Learning with Applications in R
# Ch.5: Resampling Methods

#========================================================================#
# 5.3.1 The Validation Set Approach
#========================================================================#

# Load "ISLR" library
library(ISLR)

# We use "Auto" data set in the "ISLR" library
attach(Auto)

# Set a seed for random number generator
# If we specify different seed, we will obtain different results!
set.seed(1)

# Random sampling from 1 to 392 
# for splitting the set of observations into training and test set
# train : Observation NO. for training set
train = sample(x = 1:392, size = 196)

# 1. Linear Regression
#===============================================#

# Fit a linear regression model using training set
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

# Predict the response for all 392 observations
mpg.lm.pred = predict(lm.fit, Auto)

# Calculate the test MSE of the 196 observations in the test set
# "-train" indicates selecting only the observations not in the training set
lm.mse = mean((mpg - mpg.lm.pred)[-train]^2)

# 2. Quadratic Regression
#===============================================#
qu.fit = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mpg.qu.pred = predict(qu.fit, Auto)
qu.mse = mean((mpg - mpg.qu.pred)[-train]^2)

# 3. Cubic Regression
#===============================================#
cu.fit = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mpg.cu.pred = predict(cu.fit, Auto)
cu.mse = mean((mpg - mpg.cu.pred)[-train]^2)

#========================================================================#
# 5.3.2 Leave-One-Out Cross-Validation (LOOCV)
#========================================================================#

# Load "boot" library
library(boot)

# Fit a linear regression model : Regress "mpg" on "horsepower"
glm.fit = glm(mpg ~ horsepower, data = Auto)

# Calculate LOOCV test MSE
# "cv.glm" perform k-fold cross-validation (Default : n-fold CV)
# The returned value is a list containing K, delta, and seed.
# delta's  : a vector of length two
#  1st component : raw cross-validation estimate of prediction error
#  2nd component : adjusted cross-validation estimate
cv.err = cv.glm(data = Auto, glmfit = glm.fit)

# Iteratively fit Polynomial regression models with order from 1 to 10
cv.error = rep(0, 10) # For storing CV errors
for(i in 1:10){
	glm.fit2 = glm(mpg ~ poly(horsepower, i), data = Auto)
	cv.err2 = cv.glm(data = Auto, glmfit = glm.fit2)
	cv.error[i] = cv.err2$delta[1]
}

#------------------------------------------------------------------------#
# [OPTIONAL] Replicate left-hand figure of FIGURE 5.4 in page 180 
#------------------------------------------------------------------------#

# Load "ggplot2" library
library(ggplot2)

# Dataframe for visualization
data.lfig = data.frame(DoP = 1:10, cv.error)

# Visualize
lfig = 
  ggplot(data.lfig, aes(x = DoP, y = cv.error)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "LOOCV", x = "Degree of Polynomial", y = "Mean Squared Error") + 
  scale_x_continuous(breaks = seq(2, 10, by = 2), limits = c(1, 10)) + 
  scale_y_continuous(breaks = seq(16, 28, by = 2), limits = c(16, 28))

#========================================================================#
# 5.3.3 k-Fold Cross-Validation
#========================================================================#

# Set a random seed
set.seed(17)

# Initialize a vector to store the CV errors
cv.error.10 = rep(0, 10)

# Iteratively fit Polynomial regression models with order from 1 to 10
for(i in 1:10){
	glm.fit3 = glm(mpg ~ poly(horsepower, i), data = Auto)
	cv.err3 = cv.glm(data = Auto, glmfit = glm.fit3, K = 10)
	cv.error.10[i] = cv.err3$delta[1]
}

#------------------------------------------------------------------------#
# [OPTIONAL]
# Replicate right-hand figure of FIGURE 5.4 in page 180 using "ggplot2"
# and combine two figures to replicate FIGURE 5.4
#------------------------------------------------------------------------#

# Run 10-fold CV nine separate times
cv.error.rep = matrix(0, nrow = 10, ncol = 9)
for(i in 1:9){
	for(j in 1:10){
		glm.fit4 = glm(mpg ~ poly(horsepower, j), data = Auto)
		cv.err4 = cv.glm(data = Auto, glmfit = glm.fit4, K = 10)
		cv.error.rep[j, i] = cv.err4$delta[1]
	}
}

# Load "reshape2" library
library(reshape2)

# Dataframe for visualization
rownames(cv.error.rep) = 1:10
colnames(cv.error.rep) = c(1,2,3,4,5,6,7,8,9)
data.rfig = melt(cv.error.rep)
colnames(data.rfig) = c("DoP", "Trial", "cv.error")

# Visualize
rfig = 
  ggplot(data.rfig, aes(x = DoP, y = cv.error, color = factor(Trial))) + 
  geom_line() + 
  labs(title = "10-fold CV", x = "Degree of Polynomial", y = "Mean Squared Error") + 
  scale_x_continuous(breaks = seq(2, 10, by = 2), limits = c(1, 10)) + 
  scale_y_continuous(breaks = seq(16, 28, by = 2), limits = c(16, 28)) + 
  theme(legend.position = "none")

# Load "patchwork" library
library(patchwork)

# Combine two figures to replicate FIGURE 5.4
lfig + rfig + plot_layout(ncol = 2)

#========================================================================#
# 5.3.4 The Bootstrap
#========================================================================#

# Estimating the Accuracy of a Statistic of Interest
#=============================================================#

# Create a function to estimate alpha in page 187
alpha.fn = function(data, index){
	X = data$X[index]
	Y = data$Y[index]
	return( (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y)) )
}

# Estimate of alpha using all 100 observations
alpha.est = alpha.fn(Portfolio, 1:100)

# 1. Implement a bootstrap using boot() function
#==================================================#
boot(data = Portfolio, statistic = alpha.fn, R = 1000)

# 2. Implement a bootstrap without boot() function
#==================================================#

# Number of bootstrap estimates produced
R = 1000 

# For storing estimates of alpha
alpha = rep(0, R)

# Iterate estimating alpha R times
for(i in 1:R){
	alpha[i] = alpha.fn(Portfolio, sample(1:100, 100, replace = TRUE))
}

# Mean of alpha
alpha.mean = sum(alpha)/R

# Standard error of alpha hat using (5.8) in page 189
alpha.stderr = sqrt(sum((alpha - alpha.mean)^2)/(R-1))

# Estimating the Accuracy of a Linear Regression Model
#=============================================================#

# 1. Fitting a linear model
#==================================================#

# Create a function 
# that returns intercept and slope estimates for the linear regression model
boot.fn = function(data, index){
	return( coef(lm(mpg ~ horsepower, data = data, subset = index)) )
}

# Estimate the model using the full set of 392 observations
beta.est = boot.fn(Auto, 1:392)

# Bootstrap standard error estimates for the intercept and slope terms
set.seed(1)
boot(data = Auto, statistic = boot.fn, R = 1000)

# By standard formulas for linear regression model
summary(lm(mpg ~ horsepower, data = Auto))$coef

# 2. Fitting a quadratic model
#==================================================#

# Create a function
boot.fn2 = function(data, index){
	return( coef(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index)) )
}

# Bootstrap standard error estimates for the intercept and slope terms
set.seed(1)
boot(data = Auto, statistic = boot.fn2, R = 1000)

# By standard formulas for linear regression model
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
