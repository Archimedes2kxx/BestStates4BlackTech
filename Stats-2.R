setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

### LOGIC -- Create six state objects: CA, DC, GA, NC, NY, and WA using W. Chang's R6 class
### Real paramaters of states are provided to constructor ("initialize") function
### Data frame of sample observations also provided to constructor 
### Statistics of the sample are calculated by the constructor based on parameters 
###     and sample data in the census2 data frame
### Calculated stats become estimates for the corresponding population parameters 
 
###install.packages("R6")
library(R6) 

### A. Read American Fact finder data for pop of states in 2014
file = "Pop-All-States-PuertoRico-DC-American-Fact-Finder.csv"
statesPop <- read.csv(file, header=TRUE, stringsAsFactors = FALSE)
str(statesPop)

### Delete extra census id columns
statesPop$GEO.id <- NULL
statesPop$GEO.id2 <- NULL

colnames(statesPop) <- c("state", "statePop")
### Delete extra census row
statesPop <- statesPop[-1,]
statesPop

#################################
##### B. Calculate the stats for the states
load("census2.RData")
numSampleObservations = dim(census2)[1]

### Define State as R6 class object
State <- R6Class("State",
     public=list(
         ### population parameters
         name = NULL, totPop = NULL, stateLabel = NULL,
         blackPerTotPop = NULL, whitePerTotPop = NULL, asianPerTotPop = NULL,
         
         ### statisics calculated from sample
         blackTotPop = NULL, whiteTotPop = NULL, asianTotPop = NULL,
         totTechSamplePop = NULL, blackTechSamplePop = NULL, 
         whiteTechSamplePop = NULL, asianTechSamplePop = NULL,
         blackPerTech = NULL, whitePerTech = NULL, asianPerTech = NULL,
         blackRatio = NULL, whiteRatio = NULL, asianRatio = NULL, 
         df = NULL, ### read in the census data frame
         
         ### Read paramaters of the state and read census data frame 
         initialize = function(name = NA, totPop = NA, stateLabel = NA,
                    blackPerTotPop = NA, whitePerTotPop = NA, asianPerTotPop = NA, dfCensus = NA) {
             options("scipen"=10) ## suppress scientific format
             self$name <- name
             self$stateLabel <- stateLabel
             self$totPop <- totPop
             self$blackPerTotPop <- blackPerTotPop
             self$whitePerTotPop <- whitePerTotPop
             self$asianPerTotPop <- asianPerTotPop
             self$df <- dfCensus

             private$setSampleStats(self$df)
         }
    ), 
    
    ### Calculate sample stats via a private method
    private = list(
         setSampleStats = function(df_p) {
             df_p = subset(df_p, df_p$state == self$stateLabel)
             raceVec = df_p$race
             totalVec = table(raceVec)
             self$totTechSamplePop = sum(totalVec)
             
             self$blackTechSamplePop = totalVec[" Black"]
             self$whiteTechSamplePop = totalVec[" White"]
             self$asianTechSamplePop = totalVec[" Asian"]
             
             self$blackPerTech = round(self$blackTechSamplePop/ self$totTechSamplePop, digits = 3)
             self$whitePerTech = round(self$whiteTechSamplePop/ self$totTechSamplePop, digits = 3)
             self$asianPerTech = round(self$asianTechSamplePop/ self$totTechSamplePop, digits = 3)
             
             self$blackRatio = round(self$blackPerTech / self$blackPerTotPop, digits = 2)
             self$whiteRatio = round(self$whitePerTech / self$whitePerTotPop, digits = 2)
             self$asianRatio = round(self$asianPerTech / self$asianPerTotPop, digits = 2)
             
             self$blackTotPop = round(self$totPop * self$blackPerTotPop, digits = 0)
             self$whiteTotPop = round(self$totPop * self$whitePerTotPop, digits = 0)
             self$asianTotPop = round(self$totPop * self$asianPerTotPop, digits = 0)
         }
     )
)

#####################################
### Create the states ###############

###. 

# CALIFORNIA ...
### Source = CENSUS link = http://www.census.gov/quickfacts/table/PST045215/06 
caName = "California"; caLabel = " California/CA"
caTotPop = 39144818; blackPerTotPop = .065; whitePerTotPop = .38; asianPerTotPop = .147
CA <- State$new(caName, caTotPop, caLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

# District of Columbia ...
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/11
dcName = "District of Columbia"; dcLabel = " District of Columbia/DC"
dcTotPop = 672228; blackPerTotPop = .483; whitePerTotPop = .361; asianPerTotPop = .042
DC <- State$new(dcName, dcTotPop, dcLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

# Georgia ...
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/13
gaName = "Georgia"; gaLabel = " Georgia/GA"
gaTotPop = 10214860; blackPerTotPop = .317; whitePerTotPop = .539; asianPerTotPop = .04
GA <- State$new(gaName, gaTotPop, gaLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

# North Carolilna ...
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/37
ncName = "North Carolina"; ncLabel = " North Carolina/NC"
ncTotPop = 10042802; blackPerTotPop = .221; whitePerTotPop = .638; asianPerTotPop = .022
NC <- State$new(ncName, ncTotPop, ncLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

# New York ...
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/37
nyName = "New York"; nyLabel = " New York/NY"
nyTotPop = 19795791; blackPerTotPop = .176; whitePerTotPop = .560; asianPerTotPop = .088
NY <- State$new(nyName, nyTotPop, nyLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

# Washington ...
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/53 
waName = "Washington"; waLabel = " Washington/WA"
waTotPop = 7170351; blackPerTotPop = .041; whitePerTotPop = .698; asianPerTotPop = .084
WA <- State$new(waName, waTotPop, waLabel, blackPerTotPop, whitePerTotPop, asianPerTotPop, census2)

save(CA, DC, GA, NY, NC, WA, file="R6Objects.RData")

#############################
#############################
### B. Assemble stats into matrices, then data frames
### B1. Black data

matBlackStats = matrix(data=NA, nrow=6, ncol=5) 
##### ##### revise to create vector for each state by a function, then loop through all states
matBlackStats[1,] = c(CA$blackPerTech, CA$blackPerTotPop, CA$blackRatio, CA$totPop, CA$blackTotPop)
matBlackStats[2,] = c(DC$blackPerTech, DC$blackPerTotPop, DC$blackRatio, DC$totPop, DC$blackTotPop)
matBlackStats[3,] = c(GA$blackPerTech, GA$blackPerTotPop, GA$blackRatio, GA$totPop, GA$blackTotPop)
matBlackStats[4,] = c(NY$blackPerTech, NY$blackPerTotPop, NY$blackRatio, NY$totPop, NY$blackTotPop)
matBlackStats[5,] = c(NC$blackPerTech, NC$blackPerTotPop, NC$blackRatio, NC$totPop, NC$blackTotPop)
matBlackStats[6,] = c(WA$blackPerTech, WA$blackPerTotPop, WA$blackRatio, WA$totPop, WA$blackTotPop)

rownames(matBlackStats) = c("California", "District of Columbia", 
                       "Georgia", "New York", "North Carolina", "Washington")
colnames(matBlackStats) = c("blackPerTech", "blackPerTotPop", "ratios", "totPop", "blackTotPop")
dfBlackStats = as.data.frame(matBlackStats)

### Reorder the df in descending order of percentage of Blacks in each state's total population 
dfBlackStats = dfBlackStats[order(-dfBlackStats$blackPerTotPop),]
dfBlackStats


### B2. White data
matWhiteStats = matrix(data=NA, nrow=6, ncol=5) 
matWhiteStats[1,] = c(CA$whitePerTech, CA$whitePerTotPop, CA$whiteRatio, CA$totPop, CA$whiteTotPop)
matWhiteStats[2,] = c(DC$whitePerTech, DC$whitePerTotPop, DC$whiteRatio, DC$totPop, DC$whiteTotPop)
matWhiteStats[3,] = c(GA$whitePerTech, GA$whitePerTotPop, GA$whiteRatio, GA$totPop, GA$whiteTotPop)
matWhiteStats[4,] = c(NY$whitePerTech, NY$whitePerTotPop, NY$whiteRatio, NY$totPop, NY$whiteTotPop)
matWhiteStats[5,] = c(NC$whitePerTech, NC$whitePerTotPop, NC$whiteRatio, NC$totPop, NC$whiteTotPop)
matWhiteStats[6,] = c(WA$whitePerTech, WA$whitePerTotPop, WA$whiteRatio, WA$totPop, WA$whiteTotPop)

rownames(matWhiteStats) = c("California", "District of Columbia", 
                            "Georgia", "New York", "North Carolina", "Washington")
colnames(matWhiteStats) = c("whitePerTech", "whitePerTotPop", "ratios", "totPop", "whiteTotPop")
dfWhiteStats = as.data.frame(matWhiteStats)

### Reorder the df in descending order of percentage of Whites in each state's total population 
dfWhiteStats = dfWhiteStats[order(-dfWhiteStats$whitePerTotPop),]
dfWhiteStats

### B3. Asian data
matAsianStats = matrix(data=NA, nrow=6, ncol=5) 
matAsianStats[1,] = c(CA$asianPerTech, CA$asianPerTotPop, CA$asianRatio, CA$totPop, CA$asianTotPop)
matAsianStats[2,] = c(DC$asianPerTech, DC$asianPerTotPop, DC$asianRatio, DC$totPop, DC$asianTotPop)
matAsianStats[3,] = c(GA$asianPerTech, GA$asianPerTotPop, GA$asianRatio, GA$totPop, GA$asianTotPop)
matAsianStats[4,] = c(NY$asianPerTech, NY$asianPerTotPop, NY$asianRatio, NY$totPop, NY$asianTotPop)
matAsianStats[5,] = c(NC$asianPerTech, NC$asianPerTotPop, NC$asianRatio, NC$totPop, NC$asianTotPop)
matAsianStats[6,] = c(WA$asianPerTech, WA$asianPerTotPop, WA$asianRatio, WA$totPop, WA$asianTotPop)

rownames(matAsianStats) = c("California", "District of Columbia", 
                            "Georgia", "New York", "North Carolina", "Washington")
colnames(matAsianStats) = c("asianPerTech", "asianPerTotPop", "ratios", "totPop", "asianTotPop")
dfAsianStats = as.data.frame(matAsianStats)

### Reorder the df in descending order of percentage of Asians in each state's total population 
dfAsianStats = dfAsianStats[order(-dfAsianStats$asianPerTotPop),]
dfAsianStats

### C. Save stats
save(dfBlackStats, dfWhiteStats, dfAsianStats, file="stats.RData")
