pilot <- read.csv("Pilot_tests.csv", header = TRUE)
library(ggplot2)
library(xtable)

average <- mean(pilot$shrinkage)
sdPilot <- sd(pilot$shrinkage)
ciBounds <- average + 1.96*c(-1, 1)*sdPilot
setwd("~/Documents/School/BYU 2017-2018/Winter 2018/531/Project")
pdf("linePilot.pdf", height = 4, width = 4)
(ggplot(data = pilot, aes(x=X, y = shrinkage, group=1)) + 
    geom_line(aes(color = "Observed Data")) + xlab("Test #") + ylab("Shinkage") + 
    geom_hline(aes(yintercept = average,color="Average"),
               linetype = "dashed")+
    geom_hline(aes(yintercept = ciBounds[1], color="CI Bound"),
               linetype="dashed", size = 0.2)+
    geom_hline(aes(yintercept = ciBounds[2], color="CI Bound"),
               linetype="dashed", size = 0.2)+
    scale_colour_manual("", 
                        breaks = c("Observed Data", "Average", "CI Bound"),
                        values = c("Observed Data"="black", "Average"="red", 
                                   "CI Bound"="cyan"))
    )
dev.off()
sumPilot <- t(summary(pilot$shrinkage))
xtable(sumPilot, digits = 4)
#ggplot(data = pilot, aes(x=shrinkage, group=1)) + geom_histogram(binwidth = 1)
pdf("residPilot.pdf", width = 4, height = 4)
boxplot(pilot$shrinkage, col = "red", freq = FALSE, breaks = 16,
     main = "", ylab = "Shrinkage")
dev.off()
