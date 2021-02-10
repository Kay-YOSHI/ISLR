# An Introduction to Statistical Learning with Applications in R 
# Ch.2: Statistical Learning

#========================================================================#
# Usefull Commands 
#========================================================================#
# Clear console window : ctrl+l
# Display the previous commands : Hitting the "up" arrow multiple times
# Open a new help file window about the function "funcname" : ?funcname
# Check the current directory : getwd()
# Move to a specific directory : setwd()
# "#" : Comment out
# ls() : Display a list of all of the objects, such as data and functions, that we have saved so far
# rm(x, y) : Delete objects "x" and "y"
# rm(list = ls()) : Remove all objects at once
# q() : Quit R
# savehistory() : Save a reccord of all of the commands that we typed in the most recent R session
# loadhistory() : Load the history saved by "savehistory()"
#========================================================================#

#========================================================================#
# 2.3.1 Basic Commands 
#========================================================================#

# Create a vector: c()
# We can also save things using "=" rather than "<-"
x <- c(1, 6, 2)
x = c(1, 6, 2)
y = c(1, 4, 3)

# Check the length (number of elements) of the vector: length()
length(x)
length(y)

# Arithmetic operations
x + y
x - y
x * y
x / y

# Create a matrix : matrix()
# We can omit typing "data=", "nrow=", and "ncol="
x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x = matrix(c(1, 2, 3, 4), 2, 2)

# Populate the matrix in order of the rows : byrow=TRUE
# By default, R creates matrices by successively filling in columns
x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)

# Square root of each element : sqrt()
sqrt(x)

# Raise each element of "x" to the power "2"
x^2

# Create standard normal random variables with sample size "N" : rnorm(N)
# The mean and standard deviation can be altered using the "mean" and "sd" arguments
x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = 0.1)

# Compute the correlation between "x" and "y" : cor(x, y)
cor(x, y)

# Initialize the pseudo-random number generator : set.seed()
set.seed(3)
y = rnorm(50)

# Compute the mean, variance, and standard deviation : mean(), var(), sd()
# Standard deviation can be computed by "sqrt(var())"
mean(y)
var(y)
sd(y)
sqrt(var(y)) 

#========================================================================#
# 2.3.2 Graphics 
#========================================================================#

# Generate sample data
x = rnorm(100)
y = rnorm(100)

# Produce a scatter plot : plot(x, y)
# "xlab=", "ylab=" : label on the x-, y-axis
# "main=" : Title of the figure
plot(x, y)
plot(x, y, xlab = "x-axis", ylab = "y-axis", main = "Plot of x vs y")

# Create a sequence : seq()
x = seq(1,10)               
x = 1:10
x = seq(-pi, pi, length = 50)

# Produce a contour plot : contour()
y = x
f = outer(x, y, function(x, y) cos(y)/(1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa = (f - t(f))/2
contour(x, y, fa, nlevels = 15)

# Produce a heatmap : image()
image(x, y, fa)

# Produce a three-dimensional plot : persp()
# "theta" and "phi" control the angles at which the plot is viewed
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

#========================================================================#
# 2.3.3 Indexing Data 
#========================================================================#

# Suppose that our data is stored in the matrix "A"
A = matrix(1:16, 4, 4)

# The following command will select the element corresponding to the second row and the third column
A[2, 3]

# We can also select multiple rows and columns at a time
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
A[1, ]
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]

# Output the number of rows and columns of a given matrix : dim()
dim(A)

#========================================================================#
# 2.3.4 Loading Data 
#========================================================================#

# Import a data set from a text file : read.table()
Auto = read.table("Auto.txt", header = TRUE, na.strings = "?") 

# View the data in a spreadsheet like window : fix()
fix(Auto) 

# Import a data set from a csv file : read.csv()
Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
fix(Auto)
dim(Auto)
Auto[1:4, ]

# Remove the rows that contain missing observations : na.omit()
Auto = na.omit(Auto)
dim(Auto)

# Check the variable names : names()
names(Auto)

#========================================================================#
# 2.3.5 Additional Graphical and Numerical Summaries 
#========================================================================#

# Simply typing the variable names will produce an error message 
plot(cylinders, mpg) 

# To refer to a variable, we must type the data set and the variable name joined with a "$" symbol
plot(Auto$cylinders, Auto$mpg) 

# Alternatively, "attach()" function tells R to make the variables available by name
attach(Auto) 
plot(cylinders, mpg)

# Convert quantitative variables into qualitative variables : as.factor()
cylinders = as.factor(cylinders)

# "plot()" function produces boxplots if the variable plotted on the x-axis is categorical 
# As usual, a number of options are available
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = TRUE)
plot(cylinders, mpg, col = "red", varwidth = TRUE, horizontal = TRUE)
plot(cylinders, mpg, col = "red", varwidth = TRUE, xlab = "cylinders", ylab = "MPG")

# Plot a histogram : hist()
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)

# Create a scatter plot matrix, a scatter plot for every pair of variables : pairs()
pairs(Auto) 

# We can also produce scatter plots for just a subset of the variables
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto) 

# Produce a numerical summary of each variable in a particular data set : summary()
# For qualitative variables, R will list the number of observations that fall in each category
summary(Auto)

# We can also produce a summary of just a single variable
summary(mpg)
