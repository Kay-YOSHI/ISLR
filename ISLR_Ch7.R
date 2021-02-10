# An Introduction to Statistical Learning with Applications in R
# Ch.7: Moving Beyond Linearity

# Load "ISLR" library
library(ISLR)

# We use "Wage" data set in the "ISLR" library
attach(Wage)

#========================================================================#
# 7.8.1 Polynomial Regression and Step Functions
#========================================================================#

# Fit a degree-4 polynomial regression model 
#========================================================
fit = lm(wage ~ poly(age, 4), data = Wage)

# The following commands essentially produces the same result
fit2 = lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
fit3 = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
fit4 = lm(wage ~ cbind(age, age^2, age^3, age^4))

# Create a grid of values for "age"
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])

# Prediction and confidence interval
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit) # 95% confidence interval

# Plot the data and add the fit (Replication of left-side figure of FIGURE 7.1 in page 267)
library(ggplot2)
df1 = data.frame(age = age, wage = wage)
df2 = data.frame(age = age.grid, pred = preds$fit, bot = se.bands[, 1], up = se.bands[, 2])
ggplot(data = df1, aes(x = age, y = wage)) + 
  geom_point(color = "darkgrey") + 
  geom_line(data = df2, aes(x = age, y = pred), size = 1, color = "blue") + 
  geom_line(data = df2, aes(x = age, y = bot), size = 0.5, color = "blue", linetype = "dashed") + 
  geom_line(data = df2, aes(x = age, y = up), size = 0.5, color = "blue", linetype = "dashed") + 
  labs(title = "Degree-4 Polynomial", x = "Age", y = "Wage") + 
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(agelims[1], agelims[2])) + 
  scale_y_continuous(breaks = seq(50, 300, by = 50), limits = c(0, 320))

# How to decide on the degree of the polynomial to use
#========================================================
# One way is using hypothesis tests 

# We consider five different models
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)

# Hypothesis test using "anova()" function
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Fit a polynomial logistic regression model
#========================================================
fit.log = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

# Prediction and confidence interval
preds.log = predict(fit.log, newdata = list(age = age.grid), se = TRUE)
se.bands.log = cbind(preds.log$fit + 2 * preds.log$se.fit, preds.log$fit - 2 * preds.log$se.fit) # 95% confidence interval

# Transform predictions from logit to P(Y = 1 | X)
pfit = exp(preds.log$fit) / (1 + exp(preds.log$fit))
se.bands.log = exp(se.bands.log) / (1 + exp(se.bands.log))

# Plot the data and add the fit (Replication of right-side figure of FIGURE 7.1 in page 267)
# We can use "geom_rug()" function rather than "geom_point()" function to more accurately replicate the figure
wage2 = ifelse(wage > 250, 0.2, 0)
df3 = data.frame(age = age, wage = wage2)
df4 = data.frame(age = age.grid, pred = pfit, bot = se.bands.log[, 1], up = se.bands.log[, 2])
ggplot(data = df3, aes(x = age, y = wage)) + 
  geom_point(color = "darkgrey") + 
  geom_line(data = df4, aes(x = age, y = pred), size = 1, color = "blue") + 
  geom_line(data = df4, aes(x = age, y = bot), size = 0.5, color = "blue", linetype = "dashed") + 
  geom_line(data = df4, aes(x = age, y = up), size = 0.5, color = "blue", linetype = "dashed") + 
  labs(title = "Degree-4 Polynomial", x = "Age", y = "Wage") + 
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(agelims[1], agelims[2])) + 
  scale_y_continuous(breaks = seq(0.00, 0.20, by = 0.05), limits = c(0.00, 0.21))

# Fit a step function
#========================================================
fit.step = lm(wage ~ cut(age, 4), data = Wage)

# Prediction and confidence interval
preds.step = predict(fit.step, newdata = list(age = age.grid), se = TRUE)
se.bands.step = cbind(preds.step$fit + 2 * preds.step$se.fit, preds.step$fit - 2 * preds.step$se.fit) # 95% confidence interval

# [OPTIONAL] Plot the data and add the fit (Replication of left-side figure of FIGURE 7.2 in page 269)
df5 = data.frame(age = age.grid, pred = preds.step$fit, bot = se.bands.step[, 1], up = se.bands.step[, 2])
ggplot(data = df1, aes(x = age, y = wage)) + 
  geom_point(color = "darkgrey") + 
  geom_line(data = df5, aes(x = age, y = pred), size = 1, color = "blue") + 
  geom_line(data = df5, aes(x = age, y = bot), size = 0.5, color = "blue", linetype = "dashed") + 
  geom_line(data = df5, aes(x = age, y = up), size = 0.5, color = "blue", linetype = "dashed") + 
  labs(title = "Piecewise Constant", x = "Age", y = "Wage") + 
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(agelims[1], agelims[2])) + 
  scale_y_continuous(breaks = seq(50, 300, by = 50), limits = c(0, 320))

#========================================================================#
# 7.8.2 Splines
#========================================================================#

# Load "splines" library
library(splines)

# Fit a regression spline
#========================================================
fit.sp = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)

# Predictions with standard errors
pred.sp = predict(fit.sp, newdata = list(age = age.grid), se = TRUE)
se.bands.sp = cbind(pred.sp$fit + 2 * pred.sp$se.fit, pred.sp$fit - 2 * pred.sp$se.fit) # 95% confidence interval

# Fit a natural spline with four degrees of freedom
#========================================================
fit.ns = lm(wage ~ ns(age, df = 4), data = Wage)
pred.ns = predict(fit.ns, newdata = list(age = age.grid), se = TRUE)

# Plot the data and add the fit from both regression and natural spline
df6 = data.frame(age = age.grid, pred_reg = pred.sp$fit, bot_reg = se.bands.sp[, 1], up_reg = se.bands.sp[, 2], pred_nat = pred.ns$fit)
ggplot(data = df1, aes(x = age, y = wage)) + 
  geom_point(color = "darkgrey") +
  # Plot fit and confidence interval for regression spline 
  geom_line(data = df6, aes(x = age, y = pred_reg), size = 1, color = "blue") + 
  geom_line(data = df6, aes(x = age, y = bot_reg), size = 0.5, color = "blue", linetype = "dashed") + 
  geom_line(data = df6, aes(x = age, y = up_reg), size = 0.5, color = "blue", linetype = "dashed") + 
  # Plot fit for natural spline
  geom_line(data = df6, aes(x = age, y = pred_nat), size = 1, color = "red") + 
  labs(title = "Regression and Natural Spline", x = "Age", y = "Wage") + 
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(agelims[1], agelims[2])) + 
  scale_y_continuous(breaks = seq(50, 300, by = 50), limits = c(0, 320))

# Fit a smoothing spline
#========================================================
fit.ss = smooth.spline(age, wage, df = 16)
fit.ss2 = smooth.spline(age, wage, cv = TRUE)

# Predictions
pred.ss = predict(fit.ss, newdata = list(age = age.grid))
pred.ss2 = predict(fit.ss2, newdata = list(age = age.grid))

# Plot the data and add the fit (Replication of FIGURE 7.8 in page 280)
age.temp = c(pred.ss$x, pred.ss$x)
var = c(rep("16 DF", length(pred.ss$x)), rep("6.8 DF (LOOCV)", length(pred.ss$x)))
pred.temp = c(pred.ss$y, pred.ss2$y)
df7 = data.frame(age = age.temp, var = var, pred = pred.temp)
ggplot(data = df1, aes(x = age, y = wage)) + 
  geom_point(color = "darkgrey") +
  # Plot fit and confidence interval for regression spline 
  geom_line(data = df7, aes(x = age, y = pred, color = var), size = 1) + 
  labs(title = "Smoothing Spline", x = "Age", y = "Wage") + 
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(agelims[1], agelims[2])) + 
  scale_y_continuous(breaks = seq(50, 300, by = 50), limits = c(0, 320)) + 
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.position = c(0.88, 0.92), legend.title = element_blank())

# Perform local regression
#========================================================
fit.lr = loess(wage ~ age, span = 0.2, data = Wage)
fit.lr2 = loess(wage ~ age, span = 0.7, data = Wage)

# Predictions
pred.lr = predict(fit.lr, data.frame(age = age.grid))
pred.lr2 = predict(fit.lr2, data.frame(age = age.grid))

# Plot the data and add the fit (Replication of FIGURE 7.10 in page 283)
age.temp2 = rep(age.grid, 2)
var2 = c(rep("Span is 0.2 (16.4 DF)", length(age.grid)), rep("Span is 0.7 (5.3 DF)", length(age.grid)))
pred.temp2 = c(pred.lr, pred.lr2)
df8 = data.frame(age = age.temp2, var = var2, pred = pred.temp2)
ggplot(data = df1, aes(x = age, y = wage)) + 
  geom_point(color = "darkgrey") +
  # Plot fit and confidence interval for regression spline 
  geom_line(data = df8, aes(x = age, y = pred, color = var), size = 1) + 
  labs(title = "Local Linear Regression", x = "Age", y = "Wage") + 
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(agelims[1], agelims[2])) + 
  scale_y_continuous(breaks = seq(50, 300, by = 50), limits = c(0, 320)) + 
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.position = c(0.86, 0.93), legend.title = element_blank())

#========================================================================#
# 7.8.3 GAMS
#========================================================================#

# Load "gam" library
library(gam)

# Fit a GAM using natural splines
#=====================================================
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# Plot the results (Replication of FIGURE 7.11 in page 284)
par(mfrow = c(1, 3))
plot.Gam(gam1, se = TRUE, col = "red")

# Fit a GAM using smoothing splines
#=====================================================
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

# Plot the results (Replication of FIGURE 7.12 in page 285)
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

# Model selection in terms of "year"
# M1: a GAM that excludes "year"
# M2: A GAM that uses a linear function of "year"
# M3: a GAM that uses a spline function of "year"
gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# Summary of the gam fit
summary(gam.m3)

# Predictions on the training set
preds = predict(gam.m2, newdata = Wage)

# Fit a local regression GAM
#=====================================================
gam.lo = gam(wage ~ s(year, 4) + lo(age, span = 0.7) + education, data = Wage)
plot(gam.lo, se = TRUE, col = "green")

# Fit a local regression GAM including interactions
gam.lo.i = gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

# Plot the resulting two-dimensional surface
library(akima)
plot(gam.lo.i)

# Fit a logistic regression GAM 
#=====================================================
gam.lr = gam(I(wage > 250) ~ year + s(age, 5) + education, family = binomial, data = Wage)

# Plot the results (Replication of FIGURE 7.13 in page 287)
par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "green")

# Fit a logistic regression GAM after excluding "<HS" category from "education"
# Note that there are no high earners in the "<HS" category

gam.lr.s = gam(I(wage > 250) ~ year + s(age, 5) + education, family = binomial, 
	           data = Wage, subset = (education != "1. < HS Grad"))

# Plot the results (Replication of FIGURE 7.14 in page 288)
par(mfrow = c(1, 3))
plot(gam.lr.s, se = TRUE, col = "green")
