# Multivariate Linear Regression Example
# by Wyatt Clegg
 

# Load Data, Define Functions, and Call Libraries -------------------------------------------------
data(mtcars)

plotMPG=function(varName, varLongName = '', ...) {
  varLongName<- ifelse(varLongName == '', varName, varLongName)
  plot(mtcars[, varName], mtcars[, 'mpg'], xlab = varLongName, pch = 18,
       ylab = "MPG",...)
}

plotDisp=function(varName, varLongName = '', add = FALSE, ...) {
  varLongName<- ifelse(varLongName == '', varName, varLongName)
  if(!add) {
    plot(mtcars[, varName], mtcars[, "disp"], xlab = varLongName, pch = 18,
         ylab = "Displacement",...)
  } else {
    points(mtcars[, varName], mtcars[, "disp"], pch = 18, ...)
    legend("topleft", c("MPG", "Displacement"), col = c("black", "tomato"),
           pch = 18)
  }
}
# EDA ---------------------------------------------------------------------

# Lets assume mpg and displacement as the response variables.
colnames(mtcars)
head(mtcars)
respVars <- c("mpg", "disp")
indVars <- colnames(mtcars)[-c(1, 3)]
indLongVars <- c("# of Cylinders", "Gross Horsepower", "Rear Axle Ratio",
                 "Weight (1000 lbs)", "Quarter Mile Time", "Engine Type",
                 "Transmission", "# of Forward Gears", "# of Carburetors")
par(mfrow = c(3, 2))
for(i in 1:length(indVars)) {
  plotMPG(indVars[i], indLongVars[i])
  plotDisp(indVars[i], indLongVars[i], col = "tomato", add = FALSE)
}
dev.off()

# There doesn't appear to be any nonlinear trends in any of these variables.


# Fit a Model -------------------------------------------------------------
model1 <- lm(cbind(mpg,disp) ~ ., data = mtcars)
summary(model1)

# Forward Variable Selection ----------------------------------------------
X <- matrix(1, ncol = 1, nrow = nrow(mtcars))
cNames <- c("Intercept")
for(b in indVars){
  X1 <- cbind(X, mtcars[, b])
  colnames(X1) <- c(cNames, b)
  modelA <- lm(cbind(mpg,disp) ~ X, data = mtcars)
  modelB <- lm(cbind(mpg,disp) ~ X1, data = mtcars)
  # Likelihood ratio test
  pValue <- anova(modelB, modelA)$"Pr(>F)"[2]
  if(pValue < 0.05) {
    X <- X1
    cNames <- colnames(X1)
  }
  cat(b, '\n')
}
cat(cNames)
modelFinal <- lm(cbind(mpg, disp) ~ X1, data = mtcars)
# Using the Likelihood Ratio Test, it appears that only the number of cylinders and
# weight are significant.


# Interpretation ----------------------------------------------------------

# Assumptions in Multivariate Linear Regression
#  1- Linear; we checked this
#  2- Independent Observations- cars are different
#  3- Normal Residuals- we'll check this
#  4- Equal Variance- we'll check this
#  5- Independence between error matrices- we'll examine this

resid <- residuals(modelFinal)
preds <- predict(modelFinal)
par(mfrow = c(2, 2))
for(j in 1:ncol(resid)) {
  hist(scale(resid[, j]), col = "tomato", freq = FALSE,
       main = paste0("Histogram of Residuals for ", respVars[j]),
       xlab = "Standardized Residuals")
  xx <- seq(-4, 4, length.out = 100)
  lines(xx, dnorm(xx), lwd = 2, lty = 2)
  plot(preds[, j], scale(resid[, j]), pch = 18, cex = 0.8,
       xlab = paste0("Fitted Values for ", respVars[j]),
       ylab = "Residuals")
  abline(h = 0, col = 'red')
}

# The residuals look normally distributed. There's some unequal variance for
# mpg on the ends of the spectrum, but nothing too crazy.

cor(X1[, -1]) # Cylinder and Weight seem highly related. Collinearity may be an issue.

# We'll perform a cross-validation study to verify how off this might make the model.


# CV Study ----------------------------------------------------------------
nreps <- 1000
for(iter in 1:nreps) {
  testInd <- sample(nrow(mtcars), 0.1*nrow(mtcars))
  test <- mtcars[testInd, ]
  train <- mtcars[-testInd, ]
  modelCV <- lm(cbind(mpg, disp) ~ cyl + wt, data = train)
  yhat <- predict(modelCV, newdata = test)
  y <- test[, c("mpg", "disp")]
  RPMSE <- sqrt(apply(((y - yhat)^2), 2, mean))
}

# We can do a confidence region for the first car, a Honda Civic
# center of ellipse
cent <- c(yhat[1,1],yhat[1,2])
level <- 0.95
# shape of ellipse
Z <- model.matrix(modelCV)
Y <- modelCV$model[[1]]
n <- nrow(Y)
m <- ncol(Y)
r <- ncol(Z) - 1
S <- crossprod(resid(modelCV))/(n-r-1)

# radius of circle generating the ellipse
z0 <- c(1, as.matrix(test[1, c("cyl", "wt")]))
rad <- sqrt((m*(n-r-1)/(n-r-m))*qf(level,m,n-r-m)*t(z0)%*%solve(t(Z)%*%Z) %*% z0)

# generate ellipse using ellipse function in car package
ell_points <- car::ellipse(center = c(cent), shape = S, 
                           radius = c(rad), draw = FALSE)

ggplot = TRUE
# ggplot2 plot
if(ggplot){
  require(ggplot2, quietly = TRUE)
  ell_points_df <- as.data.frame(ell_points)
  ggplot(ell_points_df, aes(x, y)) +
    geom_path() +
    geom_point(aes(x = mpg, y = disp), data = data.frame(test[1, ])) +
    labs(x = respVars[1], y = respVars[2], 
         title = '')
} else {
  # base R plot
  plot(ell_points, type = "l", xlab = respVars[1], ylab = respVars[2], main = '')
  points(x = cent[1], y = cent[2])
  points(x = test[1, 'mpg'], y = test[1, 'disp'], pch = 18,
         col = "red")
}

# Coverage is a bit of a harder concept to nail down in
# multivariate multiple regression.
