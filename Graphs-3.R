### 3. Produce simple regression models and graphs from the stats created in Stats-2
###     Specifically, produce plots of simple regressions of Tech share vs. Population shares
###     for Blacks, Whites, and Asians

### Load stats df and objects
load(file="stats.RData")
library(R6)
load(file="R6Objects.RData")
library(ggplot2)

### A. Calculate simple regressions of racial share of total population vs racial share of tech

blackParityModel <- lm(blackPerTech ~ blackPerTotPop, dfBlackStats)
whiteParityModel <- lm(whitePerTech ~ whitePerTotPop, dfWhiteStats)
asianParityModel <- lm(asianPerTech ~ asianPerTotPop, dfAsianStats)

### B. Create table of betas
matRatioBetas = matrix(data=NA, nrow=3, ncol=1) 
rownames(matRatioBetas) <- c("black", "white", "asian")
colnames(matRatioBetas) <- "betas"

matRatioBetas[1,] <- round(blackParityModel$coef[2], digits=2)
matRatioBetas[2,] <- round(whiteParityModel$coef[2], digits=2)
matRatioBetas[3,] <- round(asianParityModel$coef[2], digits=2)
matRatioBetas

### C. Create scatter plots of tech shares vs. population shares plus regression lines w ggplot
plot(dfBlackStats$blackPerTotPop, dfBlackStats$blackPerTech)
abline(blackParityModel)

plot(dfWhiteStats$whitePerTotPop, dfWhiteStats$whitePerTech)
abline(whiteParityModel)

plot(dfAsianStats$asianPerTotPop, dfAsianStats$asianPerTech)
abline(asianParityModel)

### C. Graphs via ggplot
#       black data
blackPlot <- ggplot(dfBlackStats, aes(blackPerTotPop, blackPerTech)) 
blackPlot <- blackPlot+ geom_point(color="black", shape=20)
blackPlot <- blackPlot + stat_smooth(geom="smooth", method="lm", color="black", se=FALSE)
blackPlot

#       white data
whitePlot <- ggplot(dfWhiteStats, aes(whitePerTotPop, whitePerTech)) 
whitePlot <- whitePlot+ geom_point(color="black", shape=20)
whitePlot <- whitePlot + stat_smooth(geom="smooth", method="lm", color="blue", se=FALSE)
whitePlot

#       asian data
asianPlot <- ggplot(dfAsianStats, aes(asianPerTotPop, asianPerTech)) 
asianPlot <- asianPlot+ geom_point(color="black", shape=20)
asianPlot <- asianPlot + stat_smooth(geom="smooth", method="lm", color="red", se=FALSE)
asianPlot

#       three regression lines
bPlot <- ggplot(dfBlackStats, aes(blackPerTotPop, blackPerTech)) 
bPlot <- bPlot + stat_smooth(geom="smooth", method="lm", color="black", se=FALSE)
wPlot <- ggplot(dfWhiteStats, aes(whitePerTotPop, whitePerTech)) 
wPlot <- wPlot + stat_smooth(geom="smooth", method="lm", color="blue", se=FALSE)
aPlot <- ggplot(dfAsianStats, aes(asianPerTotPop, asianPerTech)) 
aPlot <- aPlot + stat_smooth(geom="smooth", method="lm", color="red", se=FALSE)
threePlot <- bPlot + wPlot + aPlot
threePlot




