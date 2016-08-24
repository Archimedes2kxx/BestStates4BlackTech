### 3. Produce the graphs from the stats created in Stats-2
###     Specifically, produce plots of simple regressions of Tech share vs. Population shares
###     for Blacks, Whites, and Asians

### Load stats df and objects
load(file="stats.RData")
library(R6)
load(file="R6Objects.RData")

### Calculate simple regressions of racial percent of total population vs racial percent of tech
blackParityModel <- lm(blackPerTech ~ blackPerTotPop, dfBlackStats)
blackParityModel
whiteParityModel <- lm(whitePerTech ~ whitePerTotPop, dfWhiteStats)
whiteParityModel
asianParityModel <- lm(asianPerTech ~ asianPerTotPop, dfAsianStats)
asianParityModel

### Crude scatterplots
plot(dfBlackStats$blackPerTotPop, dfBlackStats$blackPerTech)
abline(blackParityModel)

plot(dfWhiteStats$whitePerTotPop, dfWhiteStats$whitePerTech)
abline(whiteParityModel)

plot(dfAsianStats$asianPerTotPop, dfAsianStats$asianPerTech)
abline(asianParityModel)




