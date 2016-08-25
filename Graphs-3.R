### 3. Produce the graphs from the stats created in Stats-2
###     Specifically, produce plots of simple regressions of Tech share vs. Population shares
###     for Blacks, Whites, and Asians

### Load stats df and objects
load(file="stats.RData")
library(R6)
load(file="R6Objects.RData")

### A. Calculate simple regressions of racial percent of total population vs racial percent of tech
blackParityModel <- lm(blackPerTech ~ blackPerTotPop, dfBlackStats)
blackParityModel
whiteParityModel <- lm(whitePerTech ~ whitePerTotPop, dfWhiteStats)
whiteParityModel
asianParityModel <- lm(asianPerTech ~ asianPerTotPop, dfAsianStats)
asianParityModel

### B. Create table of parity ratios and betas
matRatiosBetas = matrix(data=NA, nrow=3, ncol=7) 
blackVec <- c(CA$blackRatio, DC$blackRatio, GA$blackRatio, NY$blackRatio, NC$blackRatio, WA$blackRatio)
blackVec <- sort(blackVec)
blackVec <- c(blackVec, blackParityModel$coefficients[2])

whiteVec <- c(CA$blackRatio, DC$whiteRatio, GA$whiteRatio, NY$whiteRatio, NC$whiteRatio, WA$whiteRatio)
whiteVec <- sort(whiteVec)
whiteVec <- c(whiteVec, whiteParityModel$coefficients[2])

asianVec <- c(CA$asianRatio, DC$asianRatio, GA$asianRatio, NY$asianRatio, NC$asianRatio, WA$asianRatio)
asianVec <- sort(asianVec)
asianVec <- c(asianVec, asianParityModel$coefficients[2])

rownames(matRatiosBetas) <- c("black", "white", "asian")
matRatiosBetas[1,] <- blackVec
matRatiosBetas[2,] <- whiteVec
matRatiosBetas[3,] <- asianVec

matRatiosBetas





matRatiosBetas



### C. Create scatter plots of tech shares vs. population shares plus regression lines w ggplot
plot(dfBlackStats$blackPerTotPop, dfBlackStats$blackPerTech)
abline(blackParityModel)

plot(dfWhiteStats$whitePerTotPop, dfWhiteStats$whitePerTech)
abline(whiteParityModel)

plot(dfAsianStats$asianPerTotPop, dfAsianStats$asianPerTech)
abline(asianParityModel)




