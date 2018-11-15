# Multiple Linear Regression performed on the Boston Data Set.
# By Wyatt Clegg


# Load Data and Libraries -------------------------------------------------


if(!(require(MASS) & require(ggplot2) & require(olsrr) & require(leaps))){
  install.packages(c("MASS", "ggplot2", "olsrr", "leaps"))
  library(MASS)
  library(ggplot2)
}

data(Boston)


# Function Definition -----------------------------------------------------

plotHist = function(graphType = "baseR", xlab = "Crime Rate", colFill = "tomato",
                    dataGraph = Boston, aesElement = Boston$crim, binTotal = 30){
  if(graphType == "baseR"){
    hist(aesElement, col = colFill, freq = FALSE, xlab = xlab, 
         main = "", breaks = binTotal)
  } 
  if(graphType == "qplot"){
    qplot(aesElement, geom = "density", fill = I(colFill),
          xlab = xlab)
  }
  if(graphType == "ggplot"){
    ggplot(data=dataGraph, aes(aesElement)) + geom_histogram(fill = colFill,
                                                             color = "black", 
                                                             aes(y= ..density..),
                                                             bins = binTotal) +
      labs(x = "Crime Rate", y = "Density")
  }
}

plotCorr = function(graphType, xlab, colFill, aesElement,...){
  if(graphType == "baseR"){
    plot(aesElement, Boston$medv, pch = 18, col = colFill, xlab = xlab,
         ylab = "Median Home Value")
  }
  if(graphType == "qplot"){
    qplot(aesElement, Boston$medv, xlab = xlab, ylab = "Median Home Value",
          colour = I(colFill))
  }
  if(graphType == "ggplot"){
    ggplot(data=Boston, aes(x=aesElement, y=Boston$medv)) + geom_point(size=1,
                                                                       color = colFill) +
      labs(x = xlab, y = "Median Home Value")
  }
}
# EDA ---------------------------------------------------------------------

colnames(Boston)
par(mfrow = c(3, 2))
quit <- 0
while(quit == 0){
  cat("What kind of graphs are wanted?\nPlease enter one of the following:")
  cat("\n\tbaseR, qplot, ggplot")
  graphType <- readline()
  if(graphType %in% c("baseR", "qplot", "ggplot")){
    quit <- 1
  } else {
    cat("\nInvalid Entry. Please enter a valid choice.\n\n")
  }
}


# Crime Rate
plotHist(graphType = graphType, xlab = "Crime Rate", colFill = "tomato",
         aesElement = Boston$crim)

# ZN
plotHist(graphType = graphType, xlab = "Residential Zoning", colFill = "orange",
         aesElement = Boston$zn)

# indus
plotHist(graphType = graphType, xlab = "Industrial Density", colFill = "yellow",
         aesElement = Boston$indus)

# chas
plotHist(graphType = graphType, xlab = "River Boundary", colFill = "dodgerblue",
         aesElement = Boston$chas, binTotal = 10)
# nox
plotHist(graphType = graphType, xlab = "Nitrous Oxide Concentration", colFill = "violet",
         aesElement = Boston$nox, binTotal = 20)
# age
plotHist(graphType = graphType, xlab = "Proportion Built Before 1940", colFill = "purple",
         aesElement = Boston$age, binTotal = 20)
# dis
plotHist(graphType = graphType, xlab = "Distance to Employment Center", colFill = "red",
         aesElement = Boston$dis, binTotal = 20)
# rad
plotHist(graphType = graphType, xlab = "Access to Radial Highways", colFill = "coral",
         aesElement = Boston$rad, binTotal = 20)
# tax
plotHist(graphType = graphType, xlab = "Property Tax per $10,000", colFill = "goldenrod",
         aesElement = Boston$tax, binTotal = 20)
# ptratio
plotHist(graphType = graphType, xlab = "Pupil-Teacher Ratio", colFill = "blue",
         aesElement = Boston$ptratio, binTotal = 20)
# black
plotHist(graphType = graphType, xlab = "Proportion of African-Americans", colFill = "darkorchid",
         aesElement = Boston$black, binTotal = 20)
# lstat
plotHist(graphType = graphType, xlab = "% Lower-Class", colFill = "darkslateblue",
         aesElement = Boston$lstat, binTotal = 20)
# medv
plotHist(graphType = graphType, xlab = "Median Value of Homes", colFill = "black",
         aesElement = Boston$medv, binTotal = 20)
# rm- This isn't labeled.
plotHist(graphType = graphType, xlab = "?", colFill = "white",
         aesElement = Boston$rm, binTotal = 20)
dev.off()

# Correlations ------------------------------------------------------------

par(mfrow = c(3, 2))
# Crime Rate
plotCorr(graphType = graphType, xlab = "Crime Rate", colFill = "tomato",
         aesElement = Boston$crim)

# ZN
plotCorr(graphType = graphType, xlab = "Residential Zoning", colFill = "orange",
         aesElement = Boston$zn)

# indus
plotCorr(graphType = graphType, xlab = "Industrial Density", colFill = "yellow",
         aesElement = Boston$indus, binTotal = 20)

# chas
plotCorr(graphType = graphType, xlab = "River Boundary", colFill = "dodgerblue",
         aesElement = Boston$chas, binTotal = 10)
# nox
plotCorr(graphType = graphType, xlab = "Nitrous Oxide Concentration", colFill = "violet",
         aesElement = Boston$nox, binTotal = 20)
# age
plotCorr(graphType = graphType, xlab = "Proportion Built Before 1940", colFill = "purple",
         aesElement = Boston$age, binTotal = 20)
# dis
plotCorr(graphType = graphType, xlab = "Distance to Employment Center", colFill = "red",
         aesElement = Boston$dis, binTotal = 20)
# rad
plotCorr(graphType = graphType, xlab = "Access to Radial Highways", colFill = "coral",
         aesElement = Boston$rad, binTotal = 20)
# tax
plotCorr(graphType = graphType, xlab = "Property Tax per $10,000", colFill = "goldenrod",
         aesElement = Boston$tax, binTotal = 20)
# ptratio
plotCorrr(graphType = graphType, xlab = "Pupil-Teacher Ratio", colFill = "blue",
         aesElement = Boston$ptratio, binTotal = 20)
# black
plotCorr(graphType = graphType, xlab = "Proportion of African-Americans", colFill = "darkorchid",
         aesElement = Boston$black, binTotal = 20)
# lstat
plotCorr(graphType = graphType, xlab = "Percent Lower-Class", colFill = "darkslateblue",
         aesElement = Boston$lstat, binTotal = 20)
# rm- This isn't labeled.
plotCorr(graphType = graphType, xlab = "?", colFill = "black",
         aesElement = Boston$rm, binTotal = 20)

par(mfrow = c(1, 1))
# The actual correlations are
corrStudy <- sapply(1:ncol(Boston), function(g) {
  cor(Boston$medv, Boston[,g])
})
colStuff <- c("tomato", "orange", "yellow", "dodgerblue", "violet", "purple",
              "red", "coral", "goldenrod", "blue", "darkorchid", "darkslateblue",
              "black", "white")
plot(abs(corrStudy), pch = 18, col = c("tomato", "orange", "yellow", "dodgerblue", "violet", "purple",
                                       "red", "coral", "goldenrod", "blue", "darkorchid", "darkslateblue",
                                       "black", "white"),
     ylab = "Absolute Correlation", xaxt = 'n', xlab = "")
meanCorr <- mean(abs(corrStudy))
abline(h = meanCorr, lty = 2)
axis(1, at = 1:ncol(Boston), labels = colnames(Boston))
corStuff <- as.data.frame(cbind(1:ncol(Boston), abs(corrStudy)))
ggplot(corStuff, aes(x=corStuff[, 1], y=corStuff[,2])) + geom_point(size=1,
                                                                   color = colStuff) +
  labs(x = "", y = "Absolute Correlation") + geom_hline(yintercept = meanCorr, linetype = "dashed",
                                                        "black", size = 1)

# Yay, we looked at everything... now let's do some actual analysis.


# Multiple Regression Variable Selection ----------------------------------
model <- lm(medv ~ ., data = Boston)
# These would work but they're too slow.
# k1 <- ols_step_all_possible(model)
# k2 <- ols_step_best_subset(model)
# k3 <- ols_step_forward_p(model)
# k4 <- ols_step_backward_p(model)
# k5 <- ols_step_both_p(model)
# 
# l3 <- ols_step_forward_aic(model)
# l4 <- ols_step_backward_aic(model)
# l5 <- ols_step_both_aic(model)

leaps <- regsubsets(medv ~., data = Boston, nbest = 3)
plot(leaps, scale = "adjr2")
plot(leaps, scale = "bic")
# It appears that zn, chas, nox, rm, dis, ptrratio, black, lstat are the best model.
modelFinal <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + black + lstat,
                 data = Boston)
summary(modelFinal)


# Check Assumptions -------------------------------------------------------


plot(predict(modelFinal), residuals(modelFinal), pch = 18,
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
# There appears to be some trend we're not accounting for. The line
# at the top right seems to be a problem. It's the best we can manage, though.


xx <- seq(-6, 6, length.out = 100)
hist(stdres(modelFinal), col = "tomato", freq = FALSE,
     main = 'Histogram of Residuals')
lines(xx, dnorm(xx), col = "black", lty = 1, lwd = 2)
# The residuals appear to be normal.



# Cross-Validation Study --------------------------------------------------
cvStudy <- sapply(1:1000, function(i) {
  testInd <- sample(1:nrow(Boston), 0.1 * nrow(Boston))
  test <- Boston[testInd, ]; train <- Boston[-testInd, ]
  cvMod <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + black + lstat,
              data = train)
  yhat <- predict(cvMod, newdata = test)
  predInt <- predict(cvMod, newdata=test, interval = "prediction")
  RPMSE <- sqrt(mean((yhat - test[, "medv"])^2))
  coverage <- mean(predInt[, 2] < test[, "medv"] & test[, "medv"] < predInt[, 3])
  c(coverage, RPMSE)
})

# Lots of Histograms to show the results
par(mfrow = c(2, 1))
hist(cvStudy[1, ], col = "red", xlab = "Coverage", freq = FALSE, main = "")
hist(cvStudy[2, ], col = "red", xlab = "RPMSE", freq = FALSE, main = "")

qplot(cvStudy[1, ], geom = "density", fill = I("dodgerblue"),
      xlab = "Coverage")
qplot(cvStudy[2, ], geom = "density", fill = I("blue"),
      xlab = "RPMSE")
cv2 <- as.data.frame(t(cvStudy))
colnames(cv2) <- c("coverage", "rpmse")
ggplot(data= cv2, aes(cv2$coverage)) + geom_histogram(fill = "yellow",
                                                         color = "black", 
                                                         aes(y= ..density..),
                                                         bins = 10) +
  labs(x = "Coverage", y = "Density")

ggplot(data= cv2, aes(cv2$rpmse)) + geom_histogram(fill = "green",
                                                      color = "black", 
                                                      aes(y= ..density..),
                                                      bins = 10) +
  labs(x = "RPMSE", y = "Density")
# The coverage is good, although rather high.
# The RPMSE is also high.