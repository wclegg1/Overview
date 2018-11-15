### 637 Final Project
library(lme4); library("spaMM"); library(xtable); library(e1071); library(splines); library(maps)
library(glmmTMB)
library(geosphere)

# Data Wrangling
#################-------------------------------------
# Set working directory to current directory
salem <- read.csv("Accused-Witches-Data-Set.csv", header = TRUE)
locations <- read.csv("locations.csv", header = TRUE)
salem$Month.of.Execution <- ifelse(is.na(salem$Month.of.Execution), 0,
                                   salem$Month.of.Execution)
salem$Executed <- ifelse(salem$Month.of.Execution > 0, 1, 0)
salem$Residence <- as.character(salem$Residence)
salem$Residence <- substr(as.character(salem$Residence), 2,
                          nchar(as.character(salem$Residence)) - 1)
salem$MA2 <- salem$Month.of.Accusation^2
salem <- salem[salem$Month.of.Accusation > 0, ]
locations$Township <- as.character(locations$Township)
distMatrix <- as.matrix(dist(locations[, 2:3], diag = TRUE, upper = TRUE))
locations[, 4] <- c(0, distMatrix[2:23, 1])
for(i in 1:nrow(locations)){
  locations[i, 4] <- distm(locations[i, 3:2], locations[1, 3:2], fun = distHaversine)
}
colnames(locations)[4] <- "Distance"
locations <- locations[order(locations$Distance), ]
toAdd <- data.matrix(t(sapply(salem$Residence, function(x){
  locations[which(locations$Township == as.character(x)), 2:4]/1000
})))
rownames(toAdd) <- NULL
newSalem <- salem
newSalem$Latitude <- unlist(toAdd[, 1])
newSalem$Longitude <- unlist(toAdd[, 2])
newSalem$Distance <- unlist(toAdd[, 3])
newSalem <- newSalem[newSalem$Month.of.Accusation < 10, ]
newSalem$Far <- ifelse(newSalem$Distance > 0.15, 1, 0)

# Table 1

uniqueRows <- unique(newSalem[, 2:3])
poisData <- as.data.frame(t(sapply(1:nrow(uniqueRows), function(x){
  choose1Ind <- which(newSalem[, 2] == uniqueRows[x, 1])
  innerInd <- which(newSalem[choose1Ind, 3] == uniqueRows[x, 2])
  tempData <- newSalem[choose1Ind[innerInd], ]
  dead <- sum(tempData$Executed)
  accused <- nrow(tempData)
  ret <- as.data.frame(c(tempData[1, c(2:3, 7:10)],dead, accused))
  colnames(ret)[7:8] <- c("Executed", "Accused")
  ret[, 1] <- as.character(ret[, 1])
  ret
})))
for(k in 1:ncol(poisData)){
  poisData[, k] <- unlist(poisData[, k])
}
poisData$Anyone <- ifelse(poisData$Executed > 0, 0, 1)
poisData$Far <- ifelse(poisData$Distance > 0.15, 1, 0)
poisData$prop <- poisData$Executed/poisData$Accused
poisData <- poisData[order(poisData$Month.of.Accusation), ]
poisData <- poisData[order(poisData$Residence), ]


#########------------------------------------------------------------------
# General Statistics - EDA
par(mfrow = c(1, 1))
#pdf("byTime.pdf", width = 8, height = 8)
  barplot(t(as.matrix(table(salem$Month.of.Accusation, salem$Executed))), 
          col = c("sienna1", "black"), xlab = "Month Accused", ylab = "Count")
  legend("topright", legend = c("Accused", "Executed"), col = c("sienna1", "black"),
         pch = 18, cex = 1.5)
#dev.off()
salem <- newSalem[order(newSalem$Distance), ]
#pdf("countByDist.pdf", width = 8, height = 8)
  barplot(t(as.matrix(table(salem$Residence, salem$Executed)[unique(salem$Residence),])), axisnames = FALSE,
          col = c("sienna1", "black"), 
          xlab = "Residence, ordered by Distance from Salem Town")
  legend("topright", legend = c("Accused", "Executed"), col = c("sienna1", "black"),
         pch = 18, cex = 1.5)
#dev.off()
#pdf("pointsByMonth.pdf", width = 8, height = 8)
  plot(newSalem$Month.of.Accusation, jitter(newSalem$Executed), pch = 18,
       xlab = "Month of Accusation", ylab = "Jittered Execution Status")
  lines(loess.smooth(newSalem$Month.of.Accusation, newSalem$Executed), col = "sienna1",
        lty = 2, lwd = 2)
#dev.off()
#pdf("countsAndover.pdf", width = 8, height = 8)
  barplot(t(as.matrix(table(salem[salem$Residence == "Andover", ]$Month.of.Accusation,
                            salem[salem$Residence == "Andover", ]$Executed))),
          col = c("sienna1", "black"))
#dev.off()
#pdf("outlierAndover.pdf", width = 8, height = 8)
  plot(jitter(poisData[poisData$Residence != "Andover", ]$Month.of.Accusation),
       jitter(poisData[poisData$Residence != "Andover", ]$Accused),
       pch = 18, xlab = "Month of Accusation", ylab = "Accused Witches", xlim = c(2, 9),
       ylim = c(0, 25), main = "")
  points(poisData[poisData$Residence == "Andover", ]$Month.of.Accusation,
         poisData[poisData$Residence == "Andover", ]$Accused,
       pch = 20, col = "red", cex = 2)
  lines(poisData[poisData$Residence == "Andover", ]$Month.of.Accusation,
        poisData[poisData$Residence == "Andover", ]$Accused,
         lty = 2, col = "red")
  legend("topleft", legend = c("Andover"), col = "red", pch = 20, cex = 2)
#dev.off()

newSalem <- newSalem[newSalem$Residence != "Andover", ]
poisData <- poisData[poisData$Residence != "Andover", ]

n <- nrow(newSalem)
n.region <- length(unique(newSalem$Residence))
n.kill <- length(which(newSalem$Month.of.Execution > 0))
n.monthsA <- max(unique(newSalem$Month.of.Accusation))
n.monthsE <- max(unique(newSalem$Month.of.Execution))

out <- t(matrix(c(n, n.region, n.kill, n.monthsA, n.monthsE, mean(salem$Month.of.Execution > 0))))
colnames(out) <- c("n", "# Regions", "# Executions", "# Accused Months",
                   "# Execution Months", "Proportion Executed/Accused")
rownames(out) <- c("")
xtable(out, digits = 0)

#pdf("countByDist2.pdf", width = 8, height = 8)
barplot(t(as.matrix(table(newSalem$Residence, newSalem$Executed)[unique(newSalem$Residence),])), axisnames = FALSE,
        col = c("sienna1", "black"), 
        xlab = "Residence, ordered by Distance from Salem Town")
legend("topright", legend = c("Accused", "Executed"), col = c("sienna1", "black"),
       pch = 18, cex = 1.5)
#dev.off()

tab1Res <- matrix(c(nrow(newSalem),
             length(unique(newSalem$Residence)),
             sum(newSalem$Executed),
             length(unique(newSalem$Month.of.Accusation)),
             sum(newSalem$Executed)/nrow(newSalem),
             mean(poisData$Accused)), nrow = 1,
             ncol = 6)
colnames(tab1Res) <- c("n",
                    "Total # Townships",
                    "Total # Executions",
                    "# Accusation Months",
                    "Total Proportion of Accused Witches Executed",
                    "Average Accusations per Township per Month"
)
xtable(matrix(tab1Res, nrow = 1, ncol = 6))

# Random Effect: Location Model
#################################-----------------------------------------
mod1 <- glmer(Executed ~ -1 + Month.of.Accusation + (1|Residence), data = newSalem,
              family = binomial)
# Alternative model without Location
# mod1 <- glmer(Executed ~ -1 + Month.of.Accusation + (1|Month.of.Accusation), data = newSalem,
#               family = binomial)


# Time Series Logistic Regression Model
#######################################--------------------------------------
W <- sapply(newSalem$Residence, function(x){
  N <- length(newSalem$Residence)
  closeInd <- which(newSalem$Residence == x)
  out <- rep(0, N)
  out[closeInd] <- 1
  out
})
colnames(W) <- NULL
diag(W) <- 0

# # Alternate W
 altW <- cbind(diag(8), 0, 0) + cbind(0, 0, diag(8)) 
 altW <- altW[, -c(1, 10)]

# Adding in Far causes the model to diverge
newSalem$MA2 <- newSalem$Month.of.Accusation^2
newSalem$MA3 <- newSalem$Month.of.Accusation^3
newSalem$MA4 <- newSalem$Month.of.Accusation^4
newSalem$MA5 <- newSalem$Month.of.Accusation^5
newSalem$MA6 <- newSalem$Month.of.Accusation^6
newSalem$MA7 <- newSalem$Month.of.Accusation^7

timeFit <- glm(Executed ~ Month.of.Accusation , 
                     data = newSalem, family = binomial(link = "logit"))
nonlineFit2 <- glm(Executed ~ Month.of.Accusation + MA6  , 
                   data = newSalem, family = binomial(link = "logit"))
# Likelihood Ratio Test
anova(timeFit1, nonlineFit2)
1 - pchisq(0.05911, 1 )
timeFit2 <- glm(Executed ~  -1 + Month.of.Accusation , 
               data = newSalem, family = binomial(link = "probit"))

timeFit3 <- glm(Executed ~  -1 + Month.of.Accusation , 
               data = newSalem, family = binomial(link = "cloglog"))

timeFit4 <- glm((1-Executed) ~  -1 + Month.of.Accusation , 
               data = newSalem, family = binomial(link = "cloglog"))
AIC(timeFit)
AIC(timeFit2)
AIC(timeFit3)
AIC(timeFit4)
newData <- as.data.frame(2:10)
colnames(newData) <- "Month.of.Accusation"
newData$MA2 <- newData$Month.of.Accusation^2
newData$MA3 <- newData$Month.of.Accusation^3
newData$MA4 <- newData$Month.of.Accusation^4 

#pdf("onePred.pdf", width = 6, height = 4)
  par(mar=c(5.1, 6.1, 1.1, 1.1))
  plot(predict(timeFit1, newdata = newData, type = "response"),
       type = "l", xlab = "Accusation Month",
       ylab = "Probability of Execution\n Among Accused Witches",
       lty = 2, col = "tomato", lwd = 2)
  points(jitter(poisData$Month.of.Accusation), jitter(poisData$prop),
         pch = 18)
  legend("topright", "Proportion Executed\n per Township", pch = 18)
#dev.off()
xtable(summary(timeFit1)$coefficients)
coefLog <- summary(timeFit1)$coefficients
coefPrint <- cbind(coefLog[, 1],
                   coefLog[, 1] - 1.96*coefLog[, 2],
                   coefLog[, 1] + 1.96*coefLog[, 2])
colnames(coefPrint) <- c("Estimate", "LB", "UB")
xtable(coefPrint)
timeBoring <- glm(Executed ~ -1 + Month.of.Accusation, 
                  data = newSalem, family = binomial)

fixedLRT(Executed ~ -1 + Month.of.Accusation + adjacency(1|Month.of.Accusation),
         Executed ~ -1 + Month.of.Accusation + Far + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newSalem, family = binomial(link = "log"), method = "ML") 
 
fixedLRT(Executed ~ -1 + Month.of.Accusation + adjacency(1|Month.of.Accusation),
         Executed ~ -1 + Month.of.Accusation + Distance + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newSalem, family = binomial(link = "log"), method = "ML")
summary(timeFit)
plot(predict(timeFit), residuals(timeFit))

# Time Poisson Model
#############################
# Remove outliers
newPois <- poisData[order(poisData$Distance), ]
newPois$D2 <- newPois$Distance^2
newPois$D3 <- newPois$Distance^3
newPois$D4 <- newPois$Distance^4
poisFit <- corrHLfit(Accused ~ Distance +D2 + D3 + adjacency(1|Month.of.Accusation),
                     adjMatrix = altW,
                     data = newPois, family = poisson(link = "log"))
# Test the relative fits of the model
fixedLRT(Accused ~ Distance  + adjacency(1|Month.of.Accusation),
  Accused ~ Distance + D2 + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newPois, family = poisson(link = "log"), method = "ML")

fixedLRT(Accused ~ Distance  + adjacency(1|Month.of.Accusation),
         Accused ~ Distance + D2 + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newPois, family = poisson(link = "log"), method = "ML")

fixedLRT(Accused ~ Distance + D2 + adjacency(1|Month.of.Accusation),
         Accused ~ Distance + D2 + D3  + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newPois, family = poisson(link = "log"), method = "ML")

fixedLRT(Accused ~ Distance + D2 +D3 + adjacency(1|Month.of.Accusation),
         Accused ~ Distance + D2 + D3 +D4 + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newPois, family = poisson(link = "log"), method = "ML")
# Get Confidence Intervals on the coefficients
coefPois <- summary(poisFit)$beta_table
coefPois <- cbind(coefPois[, 1],
                  coefPois[, 1] - qt(0.975, 40)*coefPois[, 2],
                  coefPois[, 1] + qt(0.975, 40)*coefPois[, 2])
colnames(coefPois) <- c("Estimate", "95% LB", "95% UB")
-2.364 + c(-1,1)*qt(0.975, 40)*0.8005

xtable(coefPois, digits = 3)
predict(poisFit)
mod1 <- glmer(Executed ~ Month.of.Accusation + (1|Month.of.Accusation), data = newSalem,
               family = poisson)
1 - pchisq(-2*logLik(poisFit), 40)
1 - pchisq(-2*logLik(poisFit) + 2*logLik(mod1), 40 - 3)

poisBoring <- glm(Accused ~ -1 + Month.of.Accusation + Distance, data = newPois, family = poisson)
fixedLRT(Accused ~ -1 + Month.of.Accusation + adjacency(1|Month.of.Accusation),
         Accused ~ -1 + Month.of.Accusation + Far + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newPois, family = poisson(link = "log"), method = "ML")

fixedLRT(Accused ~  Distance + adjacency(1|Month.of.Accusation),
         Accused ~  Month.of.Accusation + Distance + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = newPois, family = poisson(link = "log"), method = "ML")


poisFit2 <- corrHLfit(Accused ~ -1 + Month.of.Accusation + Far + 
                       Matern(1|Latitude + Longitude),
                     adjMatrix = altW,
                     data = newPois, family = poisson(link = "log") )
summary(poisFit2)

# Compare with outliers included
poisFit3 <- corrHLfit(Accused ~ -1 + Month.of.Accusation + Distance + adjacency(1|Month.of.Accusation),
                     adjMatrix = altW,
                     data = poisData, family = poisson(link = "log") )
summary(poisFit3)


poisFit4 <- corrHLfit(Accused ~ -1 + Month.of.Accusation + Far + 
                        Matern(1|Latitude + Longitude),
                      adjMatrix = altW,
                      data = poisData, family = poisson(link = "log") )

fixedLRT(Accused ~ -1 + Month.of.Accusation + adjacency(1|Month.of.Accusation),
         Accused ~ -1 + Month.of.Accusation + Far + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = poisData, family = poisson(link = "log"), method = "ML")

fixedLRT(Accused ~ -1 + Month.of.Accusation + adjacency(1|Month.of.Accusation),
         Accused ~ -1 + Month.of.Accusation + Distance + adjacency(1|Month.of.Accusation),
         adjMatrix = altW,
         data = poisData, family = poisson(link = "log"), method = "ML")

summary(poisFit4)
fixedLRT(poisFit)
AIC(poisFit)
AIC(poisFit2)
AIC(poisFit3)
AIC(poisFit4)

