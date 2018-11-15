# Simple Linear Regression Model
# by Wyatt Clegg

# I'll use a built-in data set to give an example of simple linear regression.
# This data set contains the average heights and weights of women. 
data(women)

# EDA ---------------------------------------------------------------------
# We'll explore the data first to see what we have.
# Columns
colnames(women)
# Easy, only two variables to consider. Let's say we want to know how height increases
# relative to weight (This is a bit of a stupid example, but it illustrates the technique).
par(mfrow = c(1, 1))
plot(women$weight, women$height, xlab = "Weight", ylab = "Height", main = "Plot I", pch = 18)
# An extremely linear fit.


# Model Fitting -----------------------------------------------------------
mod <- lm(height ~ weight, data = women)
# This is the simplest model.
summary(mod)
# The adjusted R-squared is 0.99, meaning that we explain about 99% of the variation in height
# by our function on weight. This is extremely high, and I doubt another model will perform
# better. We need to check the assumptions, though.


# Check Assumptions -------------------------------------------------------
# Linear: As can be seen in Plot I, the data have a very linear relationship.
lines(women$weight, predict(mod), col = "tomato")
# Independent: We assume that the height/weight ratio of women en masse is independent of all other
#   women. There is a chance that women might try to conform to the height/weight ratio of their neighbors
#   but we assume the effect is negligable.

library(MASS)
checkAssumptions <- function(mod) {
  # Checks our assumptions on residuals.
  # Args:
  #   mod - an R lm object.
  # Returns:
  #   NA
  par(mfrow = c(2, 1))
  # Normal Residuals-
  hist(stdres(mod), col = "tomato", main = "Histogram of\n Residuals", 
       xlab = "Standardized Residuals", xlim = c(-3, 3), freq = FALSE)
  xx <- seq(-3, 3, length.out = 50)
  lines(xx, dnorm(xx), lwd = 2, lty = 2)
  # Equal Variance among Residuals
  plot(predict(mod), residuals(mod), pch = 18, xlab = "Predicted Values",
       ylab = "Residuals", main = "Fitted v. Residuals Plot")
  abline(h = 0, col = "tomato")
}
checkAssumptions(mod)
# OH NO! The residuals don't appear to be normal. What do we do?!!!

# Simple. We can transform the height variable to see what difference it makes.
mod2 <- lm(log(women$height) ~ women$weight)
summary(mod2)
# The adjusted R^2 went down, but let's check the residuals.
checkAssumptions(mod2)

# That didn't work. Why? Because as can be seen clearly in the fitted v. residuals plot,
# the discrepancy between our model and the actual data changes relative to weight. We need
# to account for nonlinearity! Let's use natural splines, cause they're dope.
library(splines)
mod3 <- lm(women$height ~ ns(women$weight, 2))
summary(mod3)
# The adjusted R-squared went up to 0.999. Let's see if the change is justified.
checkAssumptions(mod3)
# While still not looking great, this looks better. Let's see the fit in 
# real-time.
par(mfrow = c(2, 1))
plot(women$weight, women$height, pch = 18, main = "Plot I - Linear", xlab = "Weight",
     ylab = "Height", type = "n")
newx <- seq(min(women$weight), max(women$weight), length.out = 50)
preds1 <- predict(mod, interval = "confidence", newdata = data.frame(weight = newx))
# Add a confidence interval region
polygon(c(rev(newx), newx), c(rev(preds1[ ,3]), preds1[ ,2]), col = 'grey80', border = NA)
points(women$weight, women$height, pch = 18)
lines(women$weight, predict(mod), col = "tomato")
# Nonlinear plot
plot(women$weight, women$height, pch = 18, main = "Plot II - Nonlinear", xlab = "Weight",
     ylab = "Height", type = "n")
preds3 <- predict(mod3, interval = "confidence")
# Add a confidence interval region
polygon(c(rev(women$weight), women$weight), c(rev(preds3[ ,3]), preds3[ ,2]),
        col = 'grey80', border = NA)
points(women$weight, women$height, pch = 18)
lines(women$weight, predict(mod3), col = "tomato")

# So what do we sacrifice by adding nonlinear elements? Well, if we were attempting to 
# define some causal relationship, we couldn't use the values of the coefficients in the 
# regression equation. The coeffiecients have no definable value after you add nonlinear 
# elements. But, the nonlinear fit appears to be better. 
