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

save(dfCensus1, file="dfCensus1.RData")
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

### 4. Calculate each racial group's share of the sample for each state 
dfCensus3 <- dfCensus2
censusGroups <- group_by(dfCensus3, state, race) # ... Thank you, Hadley!!! ... :-)
dfPtsPerRace <- summarise(censusGroups, ptsPerRace = sum(personalWeight)) # ... Thank you, Hadley!!! ... :-)
head(dfPtsPerRace, 20)

censusStates <- group_by(dfCensus3, state)
dfPtsPerState <- summarise(censusStates, ptsPerState = sum(personalWeight)) # ... Thank you, Hadley!!! ... :-)
head(dfPtsPerState, 20)

dfRaceEmploymentPerState <- spread(dfPtsPerRace, key=race, value=ptsPerRace) # ... Thank you, Hadley!!! ... :-)
head(dfRaceEmploymentPerState)
dfRaceEmploymentPerState[is.na(dfRaceEmploymentPerState)] <- 0 ### Replace NAs with zeros

columnNames <- c("state", "white", "black", "amInAlNat", "alNat", "otherNat", "asian", "pacific", "other", "mixed" , "hisp")
colnames(dfRaceEmploymentPerState) <- columnNames
head(dfRaceEmploymentPerState)

dfRaceEmploymentPerState$OTHERS <- dfRaceEmploymentPerState$amInAlNat + dfRaceEmploymentPerState$alNat + dfRaceEmploymentPerState$otherNat + dfRaceEmploymentPerState$pacific + dfRaceEmploymentPerState$other + dfRaceEmploymentPerState$mixed
head(dfRaceEmploymentPerState$OTHERS)

### Delete the component columns of other
dfRaceEmploymentPerState$amInAlNat <- NULL
dfRaceEmploymentPerState$alNat <- NULL
dfRaceEmploymentPerState$otherNat <- NULL
dfRaceEmploymentPerState$pacific <- NULL
dfRaceEmploymentPerState$other <- NULL
dfRaceEmploymentPerState$mixed <- NULL
head(dfRaceEmploymentPerState)
str(dfRaceEmploymentPerState)

vecPtsPerState <- c(unlist((dfPtsPerState[,2])))
dfRaceShares <- round(dfRaceEmploymentPerState[,2:6] / vecPtsPerState, digits = 3)
dfRaceSharesPerState <- dfRaceEmploymentPerState ### Dummy copy just to get the right dimensions & labels
dfRaceSharesPerState[, 2:6] <- dfRaceShares
colnames(dfRaceSharesPerState) <- c("state", "perWhite", "perBlack", "perAsian", "perHisp", "perOTHERS")
head(dfRaceSharesPerState)

### 5. Combine the two data frames
dfEmploymentAndShares <- cbind(dfRaceEmploymentPerState, dfRaceSharesPerState[, 2:6])
head(dfEmploymentAndShares) 

### 6. Add a totals row 
(allRacesInTech <- colSums(dfEmploymentAndShares[,2:6]))
(allTech <- sum(allRacesInTech)) ### 4125164
(raceSharesInTech <- round((allRacesInTech/allTech), digits=3))

dfTotalsRow <- dfEmploymentAndShares[1,] ### dummy to get columns and types
dfTotalsRow$state <- "TOTALS"
dfTotalsRow[2:6] <- allRacesInTech
dfTotalsRow[7:11] <- raceSharesInTech
dfEmploymentAndShares <- rbind(dfEmploymentAndShares, dfTotalsRow)
    
### 5. Save employment and shares data fames
head(dfEmploymentAndShares)
tail(dfEmploymentAndShares)
save(dfEmploymentAndShares, file="dfEmploymentAndShares.RData")

### Note the URL for Census data about racial shares of total U.S. pop
### https://www.census.gov/quickfacts/table/PST045215/00


