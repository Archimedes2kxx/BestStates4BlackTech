setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

### LOGIC -- Create six state objects: CA, DC, GA, NC, NY, and WA using W. Chang's R6 class
### Real paramaters of states are provided  to constructor function
### Data frame of sample observations also provided to constructor ("initialize") function
### Other attributes are calculated by the constructor based on parameters 
###     and sample data in the census2 data frame
### Calculated stats become estimates the corresponding population parameters 

###install.packages("R6")
library(R6)

### Define State class
State <- R6Class("State",
     public=list(
         ### population parameters
         name = NULL, totPop = NULL, stateLabel = NULL,
         blackPerTotPop = NULL, whitePerTotPop = NULL, asianPerTotPop = NULL,
         
         ### statisics calculated from sample
         totTechSamplePop = NULL, blackTechSamplePop = NULL, 
         whiteTechSamplePop = NULL, asianTechSamplePop = NULL,
         blackPerTech = NULL, whitePerTech = NULL, asianPerTech = NULL,
         ratioBlack = NULL, ratioWhite = NULL, ratioAsian = NULL, 
         df = NULL, ### read in the census data frame, in case I change its name
         
         initialize = function(name = NA, totPop = NA, stateLabel = NA,
                    blackPerTotPop = NA, whitePerTotPop = NA, asianPerTotPop = NA, df = NA) {
             options("scipen"=10) ## suppress scientific format
             self$name <- name
             self$stateLabel <- stateLabel
             self$totPop <- totPop
             self$blackPerTotPop <- blackPerTotPop
             self$whitePerTotPop <- whitePerTotPop
             self$asianPerTotPop <- asianPerTotPop
             self$df <- df
             
             private$getSampleStats()
         }
    ), 
    ### Calculate sample stats via a private method
    private = list(
         getSampleStats = function() {
             
         }
     )
)

### Create the six states
### Source = CENSUS link = http://www.census.gov/quickfacts/table/PST045215/06 
caName = "California"; caLabel = " California/CA"
caTotPop = 672228; blackPerTotPop = .065; whitePerTotPop = .38; asianPerTotPop = .15
CA <- State$new(caName, caTotPop, caLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

CA$name
CA$totPop
CA$stateLabel
CA$blackPerTotPop
CA$whitePerTotPop
CA$asianPerTotPop
str(CA$df)











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

caName = "California"; caLabel = " California/CA"
totalPop = 672228; blackPerTotPop = .483; whitePerTotPop = NULL; asianPerTotPop = NULL

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

