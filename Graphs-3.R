### 3. Produce simple regression models and graphs from the stats created in Stats-2
###     Specifically, produce plots of simple regressions of Tech share vs. Population shares
###     for Blacks, Whites, and Asians

### Load stats df and objects
load(file="stats.RData")
library(R6)
load(file="R6Objects.RData")

### A. Calculate simple regressions of racial share of total population vs racial share of tech
blackParityModel <- lm(blackPerTech ~ blackPerTotPop, dfBlackStats)
whiteParityModel <- lm(whitePerTech ~ whitePerTotPop, dfWhiteStats)
asianParityModel <- lm(asianPerTech ~ asianPerTotPop, dfAsianStats)

### B. Create table of betas
matRatioBetas = matrix(data=NA, nrow=3, ncol=1) 
rownames(matRatioBetas) <- c("black", "white", "asian")
colnames(matRatioBetas) <- "betas"

matRatioBetas[1,] <- round(blackParityModel$coefficients[2], digits=2)
matRatioBetas[2,] <- round(whiteParityModel$coefficients[2], digits=2)
matRatioBetas[3,] <- round(asianParityModel$coefficients[2], digits=2)
matRatioBetas

### C. Create scatter plots of tech shares vs. population shares plus regression lines w ggplot
plot(dfBlackStats$blackPerTotPop, dfBlackStats$blackPerTech)
abline(blackParityModel)

plot(dfWhiteStats$whitePerTotPop, dfWhiteStats$whitePerTech)
abline(whiteParityModel)

plot(dfAsianStats$asianPerTotPop, dfAsianStats$asianPerTech)
abline(asianParityModel)




