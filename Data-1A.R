### A. Read PUMS 2015 sample data from files downloaded from U.S. Census DataWeb site using DataFerret

library(dplyr)
library(tidyr)

### 1. Read downloaded PUMS sample data and save
file = "Race-Sex-Hisp-Citizen-WAOB-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2015-DataFerret.csv"
dfCensus1 = read.csv(file, header=TRUE, stringsAsFactors = FALSE)
save(dfCensus1, file="dfCensus1.rda")

dfCensus2 = dfCensus1
str(dfCensus2)
colnames(dfCensus2) = c("personalWeight", "Hisp", "Birth" , "CIT", "Sex", "State", "Race","Occupation") 
str(dfCensus2) ### 45081 obs. of  8 variables

#################################
### 1A. Evil Twin Data from 2010
file2010 = "Race-Sex-Hisp-Citizen-WAOB-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2010-DataFerret.csv"
dfCensus2.2010 = read.csv(file2010, header=TRUE, stringsAsFactors = FALSE)
str(dfCensus2.2010) ### Always check the order of the variables for new data.frames
colnames(dfCensus2.2010) = c("personalWeight", "Hisp", "State" , "Race", "Birth", "CIT", "Sex", "Occupation") 
str(dfCensus2.2010) ### 42843 obs. of  8 variables

########################################

### 2. Add new category to race = "hisp"
### ... ACS coded HISP = "1" for "not Hispanic" so change race values to 99 ("hispanic") when hisp != 1
rows <- dfCensus2$Hisp != "1"
dfCensus2$Race[rows] <- 99
str(dfCensus2) ### 45081 observations ... for pop weights, state, race, sex, Hispanic subgroups
head(dfCensus2)

### Evil twin ... 2010
rows <- dfCensus2.2010$Hisp != "1"
dfCensus2.2010$Race[rows] <- 99
str(dfCensus2.2010) ### 37135  observations ... for pop weights, state, race, sex, Hispanic subgroups
head(dfCensus2.2010)

##################################
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

file="Codes-Area.txt"
areaCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
areaCodes

file="Codes-Citizen.txt"
citizenCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
citizenCodes

### 4. Convert coded categorical variables to factors ... sex, race, state, occupation ... drop padding/blanks before/after each category 
dfCensus2$Race <- as.factor(dfCensus2$Race)
levels(dfCensus2$Race) <- trimws(raceCodes[,2])

dfCensus2$Sex <- as.factor(dfCensus2$Sex)
levels(dfCensus2$Sex) <- trimws(sexCodes[,2])

dfCensus2$State <- as.factor(dfCensus2$State)
levels(dfCensus2$State) <- trimws(stateCodes[,2])
### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping

dfCensus2$Occupation <- as.factor(dfCensus2$Occupation)
levels(dfCensus2$Occupation) <- trimws(occupationCodes[,2])

dfCensus2$Birth <- as.factor(dfCensus2$Birth)
levels(dfCensus2$Birth) <- trimws(areaCodes[,2])

dfCensus2$CIT <- as.factor(dfCensus2$CIT)
levels(dfCensus2$CIT) <- trimws(citizenCodes[,2])

str(dfCensus2) ### 45081 obs. of  9 variables:
head(dfCensus2, 10)
dfCensus2$Citizen <- TRUE
dfCensus2$Citizen[dfCensus2$CIT=="No"] <- FALSE
save(dfCensus2, file="dfCensus2.rda")

dfCensus3 <- subset(dfCensus2, Citizen==TRUE) 
str(dfCensus3) ### 40278 obs. of  9 variables
save(dfCensus3, file="dfCensus3.rda")

######################################
### 4. Evil twin ... 2010 ... categories to factors
dfCensus2.2010$Race <- as.factor(dfCensus2.2010$Race)
levels(dfCensus2.2010$Race) <- trimws(raceCodes[,2])

dfCensus2.2010$Sex <- as.factor(dfCensus2.2010$Sex)
levels(dfCensus2.2010$Sex) <- trimws(sexCodes[,2])

dfCensus2.2010$State <- as.factor(dfCensus2.2010$State)
levels(dfCensus2.2010$State) <- trimws(stateCodes[,2])
### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping

dfCensus2.2010$Occupation <- as.factor(dfCensus2.2010$Occupation)
levels(dfCensus2.2010$Occupation) <- trimws(occupationCodes[,2])

dfCensus2.2010$Birth <- as.factor(dfCensus2.2010$Birth)
levels(dfCensus2.2010$Birth) <- trimws(areaCodes[,2])

dfCensus2.2010$CIT <- as.factor(dfCensus2.2010$CIT)
levels(dfCensus2.2010$CIT) <- trimws(citizenCodes[,2])

str(dfCensus2.2010) ### 37135 obs. of  8 variables:
head(dfCensus2.2010, 10)
dfCensus2.2010$Citizen <- TRUE
dfCensus2.2010$Citizen[dfCensus2.2010$CIT=="No"] <- FALSE
save(dfCensus2.2010, file="dfCensus2.2010.rda")

dfCensus3.2010 <- subset(dfCensus2.2010, Citizen==TRUE) 
str(dfCensus3.2010) ### 33448 obs. of  9 variables
save(dfCensus3.2010, file="dfCensus3.2010.rda")
################ End of Evil Twin 2010
##########################################

### Stats-2 will calculate the first group of tables from dfCensus2 for the overall U.S. 
### ... Total U.S. techs, male/female breakdown, total for each type of tech, total techs in each state

### Each row in the ACS Public Use Microdat Sample (PUMS) table that I downloaded contains the responses of one real tech employee, i.e., his/her sex, race/ethnicity, occupation, and state. Each observation also conatins a number called "personal weight" which is the Census Bureau's estimate of how many people in the real population are like that one person in the sample. So to obtain an estimate of the total number of techs in the real population represented by the techs in the sample, one merely adds the personal weights of the techs in the sample. This applies to subgroups, e.g, male vs. female, residents of specific states, etc. 

### Following this logic, to obtain the percentage share of a subgroup's Count, one merely divides the estimated Count in the subgroup by the estimated Count in the entire group, i.e., divide the sum of personal weights in a subgroup by the sum of personal weights in the larger group. 

### Note: Here's the URL to the Census Bureau's description of this process 
###       https://usa.ipums.org/usa/voliii/ACSsamp.shtml
### The description appears in the paragraph with the heading: Production of Estimates"

######################
### 5. Calculate racial group's Count per each state ... Thank you, Hadley ... :-)
###### Only analyze U.S. citizens #############
#######################


census3StateRace <- group_by(dfCensus3, State, Race) 
head(census3StateRace)
dfPtsPwtStateRace <- summarise(census3StateRace, ptsPwtStateRace = sum(personalWeight))
head(dfPtsPwtStateRace, 10)
dfRaceCountPerState <- spread(dfPtsPwtStateRace, key=Race, value=ptsPwtStateRace)
head(dfRaceCountPerState)
dfRaceCountPerState[is.na(dfRaceCountPerState)] <- 0 ### Replace NAs with zeros
head(dfRaceCountPerState)

##############
### 6. Combine all groups other than black, white, asian, and hispanic into OTHERS
columnNames <- c("State", "White", "Black", "amInAlNat", "alNat", "otherNat", "Asian", "pacific", "other", "mixed" , "Hispanic")
colnames(dfRaceCountPerState) <- columnNames
dfRaceCountPerState$OTHERS <- dfRaceCountPerState$amInAlNat + dfRaceCountPerState$alNat + dfRaceCountPerState$otherNat + dfRaceCountPerState$pacific + dfRaceCountPerState$other + dfRaceCountPerState$mixed

### Delete components of OTHERS
dfRaceCountPerState <- subset(dfRaceCountPerState, select=-c(amInAlNat, alNat,otherNat, pacific, other, mixed))

#################
### 7. Add "totals" column after "state" ... 
dfRaceCountPerState$Totals <- rowSums(dfRaceCountPerState[,2:6])
dfRaceCountPerState <- dfRaceCountPerState[,c(1,7, 2:6)] ### move totals into second column
head(dfRaceCountPerState)

#####################################
### 8 Calculate the Count each sex per each state ... Thank you, Hadley ... :-)
census3StateSex <- group_by(dfCensus3, State, Sex) 
head(census3StateSex)
dfPtsPwtStateSex <- summarise(census3StateSex, ptsPwtStateSex = sum(personalWeight))
head(dfPtsPwtStateSex, 10)
dfSexCountPerState <- spread(dfPtsPwtStateSex, key=Sex, value=ptsPwtStateSex)
head(dfSexCountPerState)
dfSexCountPerState[is.na(dfSexCountPerState)] <- 0 ### Replace NAs with zeros
head(dfSexCountPerState)
dfFemale <- subset(dfSexCountPerState, select=c(State, Female))
colnames(dfFemale) =  c("State", "Female")
head(dfFemale)

##################
### 9. Asian females
census3StateRaceSex <- group_by(dfCensus3, State, Sex, Race) 
head(census3StateRaceSex, 20)
dfSumPwtStateRaceSex <- summarise(census3StateRaceSex, SumPwtStateRaceSex = sum(personalWeight))
head(dfSumPwtStateRaceSex, 20)
dfRaceSexPerState <- spread(dfSumPwtStateRaceSex, key=Race, value=SumPwtStateRaceSex)
head(dfRaceSexPerState)
dfRaceSexPerState[is.na(dfRaceSexPerState)] <- 0 ### Replace NAs with zeros
head(dfRaceSexPerState)
dfFemAsian <- subset(dfRaceSexPerState, Sex=="Female", select=c(State,Asian))
colnames(dfFemAsian) =  c("State", "FemAsian")
head(dfFemAsian)

#################
### 10. Combine the two female dfs, create FemNonAsian
dfFemale <- merge(dfFemale, dfFemAsian)
head(dfFemale)
dfFemale$FemNonAsian <- dfFemale$Female - dfFemale$FemAsian
head(dfFemale)

##### column merge dfFemales at this point to end 
##dfRaceSexCountPerState2 <- dfRaceSexCountPerState
dfRaceSexCountPerState <- merge(dfRaceCountPerState, dfFemale)
head(dfRaceSexCountPerState)

### 11. Calculate each racial group's share of total tech Count in each state
vecTotalRaceSexCountPerState <- c(unlist(dfRaceSexCountPerState[,2]))
head(vecTotalRaceSexCountPerState)
dfRaceSexCountShares <- round(100 * dfRaceSexCountPerState[,3:10] / vecTotalRaceSexCountPerState, digits = 1) ### Need percents, so multiply by 100
head(dfRaceSexCountShares)

dfRaceSexCountSharesPerState <- dfRaceSexCountPerState[,c(1, 3:10)] ### Dummy copy to get columns and types 
dfRaceSexCountSharesPerState[, 2:9] <- dfRaceSexCountShares
colnames(dfRaceSexCountSharesPerState) <- c("State", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS", "perFemale", "perFemAsian", "perFemNonAsian")

### 12. Merge the two data frames on state, only common variable
dfRaceSexCountAndShares <- merge(dfRaceSexCountPerState, dfRaceSexCountSharesPerState)
head(dfRaceSexCountAndShares)

### 13. Add a totals row 
(allRacesInTech <- colSums(dfRaceSexCountAndShares[,3:10]))
(allTech <- sum(allRacesInTech[1:5])) ### 4125164 ... don't include females for total
(raceSharesInTech <- round(100 * allRacesInTech/allTech, digits=1))

dfTotalsRow <- dfRaceSexCountAndShares[1,] ### dummy coy to get columns and types
dfTotalsRow$State <- "ALL STATES"
dfTotalsRow[1,3:10] <- allRacesInTech
dfTotalsRow[1,11:18] <- raceSharesInTech
dfTotalsRow[1,2] <- allTech
dfRaceSexCountAndShares <- rbind(dfTotalsRow, dfRaceSexCountAndShares)
head(dfRaceSexCountAndShares)

###########################
#####################
### 14. Tabulate the non citizens ... focus on Asians
#######################
dfCensus4 <- subset(dfCensus2, select=c("personalWeight","State", "Birth"), Citizen==FALSE) 
str(dfCensus4) ### 4803 obs. of  3 variables:
save(dfCensus4, file="dfCensus4.rda")
head(dfCensus4)
table(dfCensus4$Birth) ### raw ... unweighted by personal weights

### Identify totals, Asian, NotAsia
census4StateBirth <- group_by(dfCensus4, State, Birth) 
dfSumPwtStateBirth <- summarise(census4StateBirth, ptsPwtStateBirth = sum(personalWeight))
head(dfSumPwtStateBirth) 
dfBirthPerState <- spread(dfSumPwtStateBirth, key=Birth, value=ptsPwtStateBirth, fill=0, drop=FALSE)
head(dfBirthPerState)
str(dfBirthPerState)

### Combine all Non-Asian into Others dfBirthPerState$LatinAmerica + 
dfBirthPerState$FrnOthers <- dfBirthPerState$"Latin America" + dfBirthPerState$Europe + dfBirthPerState$Africa + dfBirthPerState$"North America" + dfBirthPerState$Oceania 

dfForeignTechStates <- subset(dfBirthPerState, select=-c(USA, `US Islands`, `Latin America`, Europe, Africa, `North America`, Oceania)) ### Delete the components of FrnOthers
head(dfForeignTechStates)

dfForeignTechStates[is.na(dfForeignTechStates)] <- 0 ### Not necessary after drop=FALSE??
dfForeignTechStates$Foreign <- dfForeignTechStates$Asia + dfForeignTechStates$FrnOthers
dfForeignTechStates <- dfForeignTechStates[,c(1, 4, 2, 3)] ### Put Foreign in 1st col
colnames(dfForeignTechStates) <- c("State", "Foreign", "Asia", "NotAsia")
head(dfForeignTechStates)

### Add totals row at top
(allForeign <- colSums(dfForeignTechStates[,2:4]))
dfForeignTop <- dfForeignTechStates[1,] ### dummy copy first row to set the types
dfForeignTop$State <- "ALL STATES"
dfForeignTop[1,2:4] <- allForeign
dfForeignTechStates <- rbind(dfForeignTop, dfForeignTechStates)
  
dim(dfForeignTechStates)
dfForeignTechStates <- as.data.frame(dfForeignTechStates)
rownames(dfForeignTechStates) <- dfForeignTechStates[,1]

dfForeignTechStates$perAsia <- round(100 * dfForeignTechStates$Asia/dfForeignTechStates$Foreign, digits=2)
dfForeignTechStates$perNotAsia <- round((100 - dfForeignTechStates$perAsia), digits=2)
head(dfForeignTechStates)

##############
save(dfRaceSexCountAndShares, dfForeignTechStates, file="dfRaceSexCountAndShares.rda")


