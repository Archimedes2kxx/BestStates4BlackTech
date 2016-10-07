### A. Read PUMS 2014 sample data from files downloaded from U.S. Census DataWeb site using DataFerret

library(dplyr)
library(tidyr)

### 1. Read downloaded PUMS sample data and save
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
###  ... commas added between codes and labels ... commas deleted within labels ... and 99 Hispanic added manually
file = "Codes-Race.txt"
raceCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
raceCodes

file = "Codes-State.txt"
stateCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
stateCodes$State <-gsub("/.*","",stateCodes$State) ### Drop state initials, e.g., "New York/NY
stateCodes

file="Codes-Sex.txt"
sexCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
sexCodes

file="Codes-Occupation.txt"
occupationCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
occupationCodes

### 4. Convert coded categorical variables to factors ... sex, race, state, occupation ... drop padding/blanks before/after each category
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

### Following this logic, to obtain the percentage share of a subgroup's Residence, one merely divides the estimated Residence in the subgroup by the estimated Residence in the entire group, i.e., divide the sum of personal weights in a subgroup by the sum of personal weights in the larger group. 

### Note: Here's the URL to the Census Bureau's description of this process 
###       https://usa.ipums.org/usa/voliii/ACSsamp.shtml
### The description appears in the paragraph with the heading: Production of Estimates"

### 5. Calculate racial group's residence per each state ... Thank you, Hadley ... :-)
dfCensus3 <- dfCensus2 
census3StateRace <- group_by(dfCensus3, state, race) 
head(census3StateRace)
dfPtsPwtStateRace <- summarise(census3StateRace, ptsPwtStateRace = sum(personalWeight))
head(dfPtsPwtStateRace, 10)
dfRaceResidencePerState <- spread(dfPtsPwtStateRace, key=race, value=ptsPwtStateRace)
head(dfRaceResidencePerState)
dfRaceResidencePerState[is.na(dfRaceResidencePerState)] <- 0 ### Replace NAs with zeros
head(dfRaceResidencePerState)

### 5F Calculate the residence each sex per each state ... Thank you, Hadley ... :-)
census3StateSex <- group_by(dfCensus3, state, sex) 
head(census3StateSex)
dfPtsPwtStateSex <- summarise(census3StateSex, ptsPwtStateSex = sum(personalWeight))
head(dfPtsPwtStateSex, 10)
dfSexResidencePerState <- spread(dfPtsPwtStateSex, key=sex, value=ptsPwtStateSex)
head(dfSexResidencePerState)
dfSexResidencePerState[is.na(dfSexResidencePerState)] <- 0 ### Replace NAs with zeros
head(dfSexResidencePerState)
dfFemale <- subset(dfSexResidencePerState, select=c(state, Female))
colnames(dfFemale) =  c("state", "female")
head(dfFemale)

### 5AF. Asian females
census3StateRaceSex <- group_by(dfCensus3, state, sex, race) 
head(census3StateRaceSex)
dfPtsPwtStateRaceSex <- summarise(census3StateRaceSex, ptsPwtStateRaceSex = sum(personalWeight))
head(dfPtsPwtStateRaceSex, 10)
dfRaceSexResidencePerState <- spread(dfPtsPwtStateRaceSex, key=race, value=ptsPwtStateRaceSex)
head(dfRaceSexResidencePerState)
dfRaceSexResidencePerState[is.na(dfRaceSexResidencePerState)] <- 0 ### Replace NAs with zeros
head(dfRaceSexResidencePerState)
dfFemAsian <- subset(dfRaceSexResidencePerState, sex=="Female", select=c(state,Asian))
colnames(dfFemAsian) =  c("state", "femAsian")
head(dfFemAsian)

#################
### Combine the two female dfs, create additional column
###   femNonAsian <- female - femAsian
### Add these three varis to back of regular
### Then  percentages ... per_female, per_femAsian, per_femNonAsian
dfFemale <- merge(dfFemale, dfFemAsian)
head(dfFemale)
dfFemale$femNonAsian <- dfFemale$female - dfFemale$femAsian
head(dfFemale)

################

### 6. Combine all groups other than black, white, asian, and hispanic into OTHERS
columnNames <- c("state", "white", "black", "amInAlNat", "alNat", "otherNat", "asian", "pacific", "other", "mixed" , "hispanic")
colnames(dfRaceResidencePerState) <- columnNames
dfRaceResidencePerState$OTHERS <- dfRaceResidencePerState$amInAlNat + dfRaceResidencePerState$alNat + dfRaceResidencePerState$otherNat + dfRaceResidencePerState$pacific + dfRaceResidencePerState$other + dfRaceResidencePerState$mixed

### Delete component columns of OTHERS
dfRaceResidencePerState$amInAlNat <- NULL
dfRaceResidencePerState$alNat <- NULL
dfRaceResidencePerState$otherNat <- NULL
dfRaceResidencePerState$pacific <- NULL
dfRaceResidencePerState$other <- NULL
dfRaceResidencePerState$mixed <- NULL
head(dfRaceResidencePerState)

### 7. Add "totals" column after "state" ... 
dfRaceResidencePerState$totals <- rowSums(dfRaceResidencePerState[,2:6])
dfRaceResidencePerState <- dfRaceResidencePerState[,c(1,7, 2:6)] ### move totals into second column
head(dfRaceResidencePerState)

#####################################
##### column merge dfFemales at this point to end 
##dfRaceResidencePerState2 <- dfRaceResidencePerState
dfRaceResidencePerState <- merge(dfRaceResidencePerState, dfFemale)
head(dfRaceResidencePerState)


### 8. Calculate each racial group's share of total tech Residence in each state
vecTotalRaceResidencePerState <- c(unlist(dfRaceResidencePerState[,2]))
head(vecTotalRaceResidencePerState)
dfRaceResidenceShares <- round(100 * dfRaceResidencePerState[,3:10] / vecTotalRaceResidencePerState, digits = 1) ### Need percents, so multiply by 100
head(dfRaceResidenceShares)

dfRaceResidenceSharesPerState <- dfRaceResidencePerState[,c(1, 3:10)] ### Dummy copy to get columns and types 
dfRaceResidenceSharesPerState[, 2:9] <- dfRaceResidenceShares
colnames(dfRaceResidenceSharesPerState) <- c("state", "per_white", "per_black", "per_asian", "per_hispanic", "per_OTHERS", "per_female", "per_femAsian", "per_femNonAsian")

### 9. Merge the two data frames on state, only common variable
dfRaceResidenceAndShares <- merge(dfRaceResidencePerState, dfRaceResidenceSharesPerState)
head(dfRaceResidenceAndShares)


### 10. Add a totals row 
(allRacesInTech <- colSums(dfRaceResidenceAndShares[,3:10]))
(allTech <- sum(allRacesInTech[1:5])) ### 4125164 ... don't include females for total
(raceSharesInTech <- round(100 * allRacesInTech/allTech, digits=1))

dfTotalsRow <- dfRaceResidenceAndShares[1,] ### dummy coy to get columns and types
dfTotalsRow$state <- "ALL STATES"
dfTotalsRow[1,3:10] <- allRacesInTech
dfTotalsRow[1,11:18] <- raceSharesInTech
dfTotalsRow[1,2] <- allTech
dfRaceResidenceAndShares <- rbind(dfTotalsRow, dfRaceResidenceAndShares)
head(dfRaceResidenceAndShares)

### Change to original name of this data frame; it's still used in later modules
dfEmploymentAndShares <- dfRaceResidenceAndShares
save(dfEmploymentAndShares, file="dfEmploymentAndShares.rda")
