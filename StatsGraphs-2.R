setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

### LOGIC -- Define six state objects: CA, DC, GA, NC, NY, and WA
### Each state object has the following properties: totalPop, techPop, 
###     blackPercentOfTotal, blackTechPop, blackPercentOfTech, ratioBlack, 
###     whitePercentOfTotal, whiteTechPop, whitePercentOfTech, ratioWhite, 
###     asianPercentOfTotal, asianTechPop, asianPercentOfTech, ratioAsian. 
### The values of these properties are assigned by the initialization function
### The initialization function receives dfCensus2 and the following paramaters for each state as inputs
###     totalPop, blackPercentOfTotal, whitePercentOfTotal, and asianPercentOfTotal
### The initialization function calculates all of the other properters from these inputs
### All of the other properties are statistics of the sample and are used to estimate the corresponding pop params

### Blacks in states
getPercentages = function(perBlackPop, dfCensus, stateLabel, totalPop) {
    df = subset(dfCensus, dfCensus$state == stateLabel)
    options("scipen"=10) ## suppress scientific format
    raceVec = df$race
    totalVec = table(raceVec)
    grandTotal = sum(totalVec)
    perBlack = totalVec[" Black"]/grandTotal
    ratio = perBlack/perBlackPop
    blackPop = round(perBlackPop * totalPop, digits = 0)
    outVec = c(perBlack, perBlackPop, ratio, totalPop, blackPop)
    names(outVec) = c("BlackTechsPer", "BlackPopPer", "RatioToPop", "TotalPop", "BlackPop")
    return(outVec)
    ### return all values that will appear in the same row for each state in the report
}

perBlackPop = .483
totalPop = 672228
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/11
dcLabel = " District of Columbia/DC"
dcBlackPop = getPercentages(perBlackPop, census2, dcLabel, totalPop)

perBlackPop = .065 ###percent of CA pop
totalPop = 39144818
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/06
caLabel = " California/CA"
caBlackPop = getPercentages(perBlackPop, census2, caLabel, totalPop)

perBlackPop = .176 ###percent of NY population
totalPop = 19795791
### CENSUS link = http://www.census.gov/quickfacts/table/BZA110214/36
nyLabel = " New York/NY"
nyBlackPop = getPercentages(perBlackPop, census2, nyLabel, totalPop)

perBlackPop = .221 ###percent of NC population
totalPop = 10042802
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/37
ncLabel = " North Carolina/NC"
ncBlackPop = getPercentages(perBlackPop, census2, ncLabel, totalPop)

perBlackPop = .317 ###percent of GA population
totalPop = 10214860
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/13
gaLabel = " Georgia/GA"
gaBlackPop = getPercentages(perBlackPop, census2, gaLabel, totalPop)

### Washington State 
perBlackPop = .041
totalPop = 7170351
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/53
waLabel = " Washington/WA"
waBlackPop = getPercentages(perBlackPop, census2, waLabel, totalPop)

### Assemble stats into a matrix, then a data frame
matStats = matrix(data=NA, nrow=6, ncol=5) 
matStats[1,] = caBlackPop
matStats[2,] = dcBlackPop
matStats[3,] = gaBlackPop
matStats[4,] = nyBlackPop
matStats[5,] = ncBlackPop
matStats[6,] = waBlackPop

rownames(matStats) = c("California", "District of Columbia", 
                       "Georgia", "New York", "North Carolina", "Washington")
colnames(matStats) = c("BlackTechsPer", "BlackPopPer", "RatioToPop", "TotalPop", "BlackPop")

dfStats = as.data.frame(matStats)
dfStats$TotalPop = as.integer((dfStats$TotalPop))
dfStats$BlackPop = as.integer(dfStats$BlackPop)
dfStats

### Reorder the df in descending order of percentage of Blacks in each state's tech sector
dfStats = dfStats[order(-dfStats$BlackTechsPer),]
### file = "dfStats.csv"
### write.csv(dfStats, file)

