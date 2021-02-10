# An Introduction to Statistical Learning with Applications in R
# Ch.3: Linear Regression

# Load "MASS" and "ISLR" libraries
library(MASS)
library(ISLR)

# We use "Boston" data set in the "MASS" library
# Attaching a data frame enables us to refer to the variables by their names alone.
# Ex. "medv" instead of "Boston$medv"
attach(Boston)

#========================================================================#
# 3.6.2 Simple Linear Regression
#========================================================================#

# Fit a simple linear regression model
# Regress "medv" on "lstat"
lm.fit = lm(medv ~ lstat, data = Boston)

# Obtain estimation results
summary(lm.fit)

# Extract coefficient estimates
coef(lm.fit)

# Obtain confidence interval
confint(lm.fit)

# Produce confidence and prediction intervals 
# for the prediction of "medv" for a given value of "lstat"
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence") # confidence interval
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction") # prediction interval

# Plot "medv" and "lstat" along with the least squares regression line
plot(lstat, medv, col = "red", pch = 20) # scatter plot
abline(lm.fit, lwd = 3)                  # regression line

# Diagnostic plots
par(mfrow = c(2, 2))
plot(lm.fit)         # Four diagnostic plots are automatically produced

# Residual plots
plot(predict(lm.fit), residuals(lm.fit)) # fitted values vs. residuals
plot(predict(lm.fit), rstudent(lm.fit))  # fitted values vs. studentized residuals

# Plot leverage statistics
plot(hatvalues(lm.fit))

#========================================================================#
# 3.6.3 Multiple Linear Regression
#========================================================================#

# Regress "medv" on "lstat" and "age"
lm.fit2 = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit2)

# Regress "medv" on all 13 predictors
lm.fit3 = lm(medv ~ ., data = Boston)
summary(lm.fit3)

# Compute variance inflation factors(VIF)
library(car)
vif(lm.fit3)

# Regress "medv" on all precidtors except "age"
lm.fit4 = lm(medv ~ .-age, data = Boston)
summary(lm.fit4)

# Using "update" function produces the same result as "lm.fit4"
lm.fit5 = update(lm.fit3, ~ .-age)
summary(lm.fit5)

#========================================================================#
# 3.6.4 Interaction Terms
#========================================================================#

# Regress "medv" on "lstat", "age", and "lstat×age"
lm.fit6 = lm(medv ~ lstat * age, data = Boston)
summary(lm.fit6)

# The following produces the same result as "lm.fit6"
lm.fit7 = lm(medv ~ lstat + age + lstat:age, data = Boston)
summary(lm.fit7)

#========================================================================#
# 3.6.5 Non-linear Transformation of the Predictors
#========================================================================#

# Regress "medv" on "lstat" and "lstat^2"
lm.fit.nonl = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit.nonl)

# Hypothesis test comparing two models
# H0: two models fit the data equally well
# H1: the full model is superior
lm.fit.l = lm(medv ~ lstat, data = Boston) # Linear fit
anova(lm.fit.l, lm.fit.nonl)

# Higher-order polynomial regression using "poly" function
lm.fit8 = lm(medv ~ poly(lstat, 5))
summary(lm.fit8)

# Log transformation : Regress "medv" on "log(rm)"
lm.fit9 = lm(medv ~ log(rm), data = Boston)
summary(lm.fit9)

#========================================================================#
# 3.6.6 Qualitative Predictors
#========================================================================#

# We use "Carseats" data set in the "ISLR" library
attach(Carseats)

# Multiple regression including some interaction terms
# R generates dummy variables automatically given qualitative variables
lm.fit10 = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit10)

# Check the coding that R uses for the dummy variables
contrasts(ShelveLoc)

#========================================================================#
# 3.6.7 Writing Functions
#========================================================================#

# Create a function "LoadLibraries()" that reads in the "ISLR" and "MASS" libraries
LoadLibraries = function(){
	library(ISLR)
	library(MASS)
	print("The libraries have been loaded.")
}

# Call the function "LoadLibraries"
LoadLibraries()
