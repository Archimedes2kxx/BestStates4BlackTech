### A. Read PUMA sample data from files downloaded from U.S. Census DataWeb site PUMA 2014
###    and calculate each racial group's share of the sample for each state

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
library(dplyr)
library(tidyr)

### 1. Read codebooks
###  Raw code files were prepared by copying the appropriate 
###    part of the full codebook into separate .txt files for each variable
###  All info is in one column ... Code in first column, some spaces, then text for label
### Ignore temporary "code" "labels" headers in first row of each file

### Note: "Hispanic" was manually added to last row of races in Race-Short-Names-Code.txt file
###         ... with code = 99

### Codes and labels are separated by variable number of blanks for state and race
###     read.csv(blah, blah, sep=" ", blah, blah)

file = "Race-Short-Names-Code.txt"
raceRawCodes = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE, colClasses = "character")

file = "State-Code.txt"
stateRawCodes = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE, colClasses = "character")

makeCodeBook = function(rawCodes){
    ### Split the single raw code column into two cols: code and  label
    R = nrow(rawCodes)
    mat = matrix(data=NA, nrow=R-1, ncol=2) 
    ### Initialize empty matrix, omitting temporary "header" in first row
    ###     That's why R-1, instead of R rows 
    
    for (i in 2:R) {
        ### find 2 blank spaces, substitute "~" not used in labels
        line = sub("  ", "~", rawCodes[i,]) 
        parts = strsplit(line, "~", fixed=TRUE)
        mat[i-1,] = c(unlist(parts))
    }
    colnames(mat) = c("codes", "labels")
    df = as.data.frame(mat, stringsAsFactors=FALSE)  
    return(df)
}

raceCodeBook = makeCodeBook(raceRawCodes)
stateCodeBook = makeCodeBook(stateRawCodes)

### Drop the state initials, i.e., everything from "/" to end of line
stateCodeBook$labels <-gsub("/.*","",stateCodeBook$labels)

### 2. Read original PUMA sample data ... all variables read as character 
file = "Race-Hisp-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2014-Data.csv"
dfCensus1 = read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE, colClasses = "character")
str(dfCensus1) ### 39692 for population weights, all states, all races, all Hispanic subgroups

save(dfCensus1, file="dfCensus1")
dfCensus2 = dfCensus1

### Delete tech variable ... SOCP
dfCensus2$SOCP <- NULL

### Use comfortable variable names
colnames(dfCensus2) = c("personalWeight", "race", "state", "hisp")

### Add new category to race = "hisp"
### ... ACS coded HISP = "1" for "not Hispanic" so change race values to 99 ("hispanic") when hisp != 1
rows <- dfCensus2$hisp != "1"
dfCensus2$race[rows] <- "99"
str(dfCensus2)

### Convert personal weights to integers
dfCensus2$personalWeight <- as.integer(dfCensus2$personalWeight)

### 3. Convert category data from character to factors, e.g., black, California, etc
### xCodes are comprehensive dictionaries that contains all codes, not just the ones for this report
### Loop through values in each char variable, selecting matching label in xCodes
###     Returns factor version with labels on values
createFactorsWithLabels = function(xVar, xCodes){
    xValues = sort(unique(xVar))
    nValues = length(xValues)
    
    ### Read all codes as characters, not intergers or 
    codeVec = as.character(xCodes[,1])
    xLabels = vector(mode="character") ### empty char vector
    i = 1
    while(i <= nValues){
        ### Find first occurence of value in codeVec
        k = match(xValues[i], codeVec, nomatch = -1) 
        if (k!= -1) {
            xLabels = c(xLabels, xCodes[k,2])
        } else {
            print(paste("Label not found for i and value = ", i, " ", xValues[i]))
            xLabels = c(xLabels, paste("Label not found for i = ", i, " ", xValues[i]))
        }
        i = i + 1
    }
    facVar = factor(xVar, levels=xValues, labels=xLabels)
    return(facVar)
}    

dfCensus2$state = sprintf("%03s", dfCensus2$state) ### states are 3-digit codes, some with leading zeros
dfCensus2$state = createFactorsWithLabels(dfCensus2$state, stateCodeBook)
dfCensus2$race = createFactorsWithLabels(dfCensus2$race, raceCodeBook)

str(dfCensus2) ### 39692 obs. of  4 variables:
head(dfCensus2)
save(dfCensus2, file="dfCensus2.RData")
dfCensus3 <- dfCensus2

### 4. Calculate each racial group's personal points and total points in each state 
censusGroups <- group_by(dfCensus3, state, race) # ... Thank you, Hadley!!! ... :-)
dfPtsPerRace <- summarise(censusGroups, ptsPerRace = sum(personalWeight)) # ... Thank you, Hadley!!! ... :-)
head(dfPtsPerRace, 20)  

censusStates <- group_by(dfCensus3, state)
dfPtsPerState <- summarise(censusStates, ptsPerState = sum(personalWeight)) # ... Thank you, Hadley!!! ... :-)
head(dfPtsPerState, 20)

dfRacePointsPerState <- spread(dfPtsPerRace, key=race, value=ptsPerRace) # ... Thank you, Hadley!!! ... :-)
head(dfRacePointsPerState)

columnNames <- c("state", "white", "black", "amInAlNat", "alNat", "otherNat", "asian", "pacific", "other", "mixed" , "hisp")
colnames(dfRacePointsPerState) <- columnNames
str(dfRacePointsPerState)
head(dfRacePointsPerState)

dfCensus3 <- dfRacePointsPerState
with(dfRacePointsPerState, other <- amInAlNat + alNat + otherNat + pacific + other + mixed)
head(dfRacePointsPerState)

dfRacePointsPerState$amInAlNat <- NULL
dfRacePointsPerState$alNat <- NULL
dfRacePointsPerState$otherNat <- NULL
dfRacePointsPerState$pacific <- NULL
dfRacePointsPerState$other <- NULL
dfRacePointsPerState$mixed <- NULL

### 5. Racial shares
vecPtsPerState <- c(unlist((dfPtsPerState[,2])))
dfRaceShares <- dfRacePointsPerState[,2:5] / dfPtsPerState[,2]


str(dfCensus3) ### 39692 obs. of  4 variables:
head(dfCensus3)
save(dfCensus3, file="dfCensus3.RData")
