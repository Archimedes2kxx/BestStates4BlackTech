### A. Read PUMS 2014 sample data from files downloaded from U.S. Census DataWeb site using DataFerret

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
library(dplyr)
library(tidyr)

### 1. Read original PUMS sample data and save
file = "Race-Sex-Hisp-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2014-Data.csv"
dfCensus1 = read.csv(file, header=TRUE, stringsAsFactors = FALSE)
save(dfCensus1, file="dfCensus1.rda")

dfCensus2 = dfCensus1
colnames(dfCensus2) = c("personalWeight", "race", "state", "hisp", "sex", "occupation") 

### 2. Add new category to race = "hisp"
### ... ACS coded HISP = "1" for "not Hispanic" so change race values to 99 ("hispanic") when hisp != 1
rows <- dfCensus2$hisp != "1"
dfCensus2$race[rows] <- 99
str(dfCensus2) ### 39692 observations ... for pop weights, state, race, sex, Hispanic subgroups
head(dfCensus2)

### 3.. Read manually edited codebooks 
###     ... commas added between codes and labels ... commas deleted within labels
file = "Race-Codes-Short-Names.txt"
raceCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")

file = "State-Codes.txt"
stateCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
stateCodes$State <-gsub("/.*","",stateCodes$State) ### Drop state initials, e.g., "New York/NY

file="Sex-Codes.txt"
sexCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")

file="Occupation-Codes.txt"
occupationCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
occupationCodes

### 4. Convert coded categorical variables to factors ... sex, race, state, occupation
dfCensus2$race <- as.factor(dfCensus2$race)
levels(dfCensus2$race) <- trimws(raceCodes[,2])

dfCensus2$sex <- as.factor(dfCensus2$sex)
levels(dfCensus2$sex) <- trimws(sexCodes[,2])

dfCensus2$state <- as.factor(dfCensus2$state)
levels(dfCensus2$state) <- trimws(stateCodes[,2])

dfCensus2$occupation <- as.factor(dfCensus2$occupation)
levels(dfCensus2$occupation) <- trimws(occupationCodes[,2])
 
str(dfCensus2)
save(dfCensus2, file="dfCensus2.rda")

### Stats-2 will calculate the first group of tables from dfCensus2 for the overall U.S. 
### ... Total U.S. techs, male/female breakdown, total for each type of tech, total techs in each state

### Each row in the ACS Public Use Microdat Sample (PUMS) table that I downloaded contains the responses of one real tech employee, i.e., his/her sex, race/ethnicity, occupation, and state of residence. Each observation also conatins a number called "personal weight" which is the Census Bureau's estimate of how many people in the real population are like that one person in the sample. So to obtain an estimate of the total number of techs in the real population represented by the techs in the sample, one merely adds the personal weights of the techs in the sample. This applies to subgroups, e.g, male vs. female, residents of specific states, etc. 

### Following this logic, to obtain the percentage share of a subgroup's employment, one merely divides the estimate employment in the subgroup by the estimate employment in the entire group, i.e., divide the sum of personal weights in a subgroup by the sum of personal weights in the larger group. 

### Note: Here's the URL to the Census Bureau's description of this process 
###       https://usa.ipums.org/usa/voliii/ACSsamp.shtml
### The description appears in the paragraph with the heading: Production of Estimates"

### 5. Calculate each racial group's employment for each state ... Thank you, Hadley ... :-)
dfCensus3 <- dfCensus2 
censusGroups <- group_by(dfCensus3, state, race) 
dfPtsPerRace <- summarise(censusGroups, ptsPerRace = sum(personalWeight))
censusStates <- group_by(dfCensus3, state)
dfPtsPerState <- summarise(censusStates, ptsPerState = sum(personalWeight))
dfRaceEmploymentPerState <- spread(dfPtsPerRace, key=race, value=ptsPerRace) 
dfRaceEmploymentPerState[is.na(dfRaceEmploymentPerState)] <- 0 ### Replace NAs with zeros

### 6. Combine all groups other than black, white, asian, and hispanic into OTHERS
columnNames <- c("state", "white", "black", "amInAlNat", "alNat", "otherNat", "asian", "pacific", "other", "mixed" , "hispanic")
colnames(dfRaceEmploymentPerState) <- columnNames
dfRaceEmploymentPerState$OTHERS <- dfRaceEmploymentPerState$amInAlNat + dfRaceEmploymentPerState$alNat + dfRaceEmploymentPerState$otherNat + dfRaceEmploymentPerState$pacific + dfRaceEmploymentPerState$other + dfRaceEmploymentPerState$mixed

### Delete component columns of OTHERS
dfRaceEmploymentPerState$amInAlNat <- NULL
dfRaceEmploymentPerState$alNat <- NULL
dfRaceEmploymentPerState$otherNat <- NULL
dfRaceEmploymentPerState$pacific <- NULL
dfRaceEmploymentPerState$other <- NULL
dfRaceEmploymentPerState$mixed <- NULL

### 7. Add "totals" column after "state" ... 
dfRaceEmploymentPerState$totals <- rowSums(dfRaceEmploymentPerState[,2:6])
dfRaceEmploymentPerState <- dfRaceEmploymentPerState[,c(1,7, 2:6)] ### move totals into second column

### 8. Calculate each racial groups share of total tech employment in each state
vecTotalEmploymentPerState <- c(unlist(dfRaceEmploymentPerState[,2]))
dfRaceShares <- round(100 * dfRaceEmploymentPerState[,3:7] / vecTotalEmploymentPerState, digits = 0)
dfRaceSharesPerState <- dfRaceEmploymentPerState[,c(1, 3:7)] ### Dummy copy just to get the right dimensions & labels

dfRaceSharesPerState[, 2:6] <- dfRaceShares
colnames(dfRaceSharesPerState) <- c("state", "per_white", "per_black", "per_asian", "per_hispanic", "per_OTHERS")

### 9. Merge the two data frames
dfEmploymentAndShares <- merge(dfRaceEmploymentPerState, dfRaceSharesPerState)

### 10. Add a totals row 
(allRacesInTech <- colSums(dfEmploymentAndShares[,3:7]))
(allTech <- sum(allRacesInTech)) ### 4125164
(raceSharesInTech <- round(100 * allRacesInTech/allTech, digits=0))

dfTotalsRow <- dfEmploymentAndShares[1,] ### dummy to get columns and types
dfTotalsRow$state <- "ALL STATES"
dfTotalsRow[1,3:7] <- allRacesInTech
dfTotalsRow[1,8:12] <- raceSharesInTech
dfTotalsRow[1,2] <- allTech
dfEmploymentAndShares <- rbind(dfTotalsRow, dfEmploymentAndShares)
dfEmploymentAndShares

### 11. Save combined employment and shares data frame
head(dfEmploymentAndShares)
save(dfEmploymentAndShares, file="dfEmploymentAndShares.rda")
