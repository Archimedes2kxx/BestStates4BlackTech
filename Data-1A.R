### A. Read PUMS 2015 sample data from files downloaded from U.S. Census DataWeb site using DataFerret
### ==> data about techs in each of the 50 states plus DC

### These scripts are ugly, especially the "Evil Twin" duplicated code segments ... but they get the job done
### Goal: Convert more chunks into readable functions, then move functions into the functions-0.R file
### ===> Reduce the size of the ugly remaining scripgs; hopefully make them more readable

library(dplyr)
library(tidyr)
load(file="functions-0.rda")

### 1. Read downloaded PUMS sample data and save
file = "Race-Sex-Hisp-Citizen-WAOB-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2015-DataFerret.csv"
dfCensus1 = read.csv(file, header=TRUE, stringsAsFactors = FALSE)
save(dfCensus1, file="dfCensus1.rda")

dfCensus2 = dfCensus1
str(dfCensus2)
colnames(dfCensus2) = c("personalWeight", "Hisp", "Birth" , "Citizen", "Sex", "State", "Race","Occupation") 
str(dfCensus2) ### 45081 obs. of  8 variables

### 1A. Evil Twin Data from 2010
file2010 = "Race-Sex-Hisp-Citizen-WAOB-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2010-DataFerret.csv"
dfCensus2.2010 = read.csv(file2010, header=TRUE, stringsAsFactors = FALSE)
str(dfCensus2.2010) ### Always check the order of the variables for new data.frames
colnames(dfCensus2.2010) = c("personalWeight", "Hisp", "State" , "Race", "Birth", "Citizen", "Sex", "Occupation") 
str(dfCensus2.2010) ### 42843 obs. of  8 variables

########################################
### 2. Add new category to race = "hisp"
### ... ACS coded HISP = "1" for "not Hispanic" so change race values to 99 ("hispanic") when hisp != 1
rows <- dfCensus2$Hisp != "1"
dfCensus2$Race[rows] <- 99
str(dfCensus2) ### 45081 observations ... for pop weights, state, race, sex, Hispanic subgroups
head(dfCensus2)

### 2A. Evil twin ... 2010
rows <- dfCensus2.2010$Hisp != "1"
dfCensus2.2010$Race[rows] <- 99
str(dfCensus2.2010) ### 37135  observations ... for pop weights, state, race, sex, Hispanic subgroups
head(dfCensus2.2010)

##################################
### 3.. Read manually edited codebooks 
###  ... commas added between codes and labels ... commas deleted within labels ... and 99 Hispanic added manually
listCodes <- readCodeBooks()

### 4. Convert coded categorical variables to factors ... sex, race, state, occupation ... drop padding/blanks before/after each category 
dfCensus2$Race <- as.factor(dfCensus2$Race)
levels(dfCensus2$Race) <- trimws(listCodes[["Race"]][,2])

dfCensus2$Sex <- as.factor(dfCensus2$Sex)
levels(dfCensus2$Sex) <- trimws(listCodes[["Sex"]][,2])

dfCensus2$State <- as.factor(dfCensus2$State)
levels(dfCensus2$State) <- trimws(listCodes[["State"]][,2])
### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping

dfCensus2$Occupation <- as.factor(dfCensus2$Occupation)
levels(dfCensus2$Occupation) <- trimws(listCodes[["Occupation"]][,2])

dfCensus2$Birth <- as.factor(dfCensus2$Birth)
levels(dfCensus2$Birth) <- trimws(listCodes[["Birth"]][,2])

dfCensus2$Citizen <- as.factor(dfCensus2$Citizen)
levels(dfCensus2$Citizen) <- trimws(listCodes[["Citizen"]][,2])

str(dfCensus2) ### 45081 obs. of  9 variables:
head(dfCensus2, 10)

### This samba stems from my lack facility with factors
table(dfCensus2$Citizen) ### 4803 non-citizens
dfCensus2$Citizen2 <- TRUE 
dfCensus2$Citizen2[dfCensus2$Citizen == "No"] <- FALSE
table(dfCensus2$Citizen2)
dfCensus2$Citizen <- dfCensus2$Citizen2
dfCensus2$Citizen2 <- NULL
str(dfCensus2)

save(dfCensus2, file="dfCensus2.rda")

dfCensus3 <- subset(dfCensus2, Citizen==TRUE) 
str(dfCensus3) ### 40278 obs. of  9 variables
save(dfCensus3, file="dfCensus3.rda")

######################################
### 4. Evil twin ... 2010 ... categories to factors
dfCensus2.2010$Race <- as.factor(dfCensus2.2010$Race)
levels(dfCensus2.2010$Race) <- trimws(listCodes[["Race"]][,2])

dfCensus2.2010$Sex <- as.factor(dfCensus2.2010$Sex)
levels(dfCensus2.2010$Sex) <- trimws(listCodes[["Sex"]][,2])

dfCensus2.2010$State <- as.factor(dfCensus2.2010$State)
levels(dfCensus2.2010$State) <- trimws(listCodes[["State"]][,2])
### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping

dfCensus2.2010$Occupation <- as.factor(dfCensus2.2010$Occupation)
levels(dfCensus2.2010$Occupation) <- trimws(listCodes[["Occupation"]][,2])

dfCensus2.2010$Birth <- as.factor(dfCensus2.2010$Birth)
levels(dfCensus2.2010$Birth) <- trimws(listCodes[["Birth"]][,2])

dfCensus2.2010$Citizen <- as.factor(dfCensus2.2010$Citizen)
levels(dfCensus2.2010$Citizen) <- trimws(listCodes[["Citizen"]][,2])

str(dfCensus2.2010) ### 37135 obs. of  8 variables:
head(dfCensus2.2010, 10)

### Same awkward evil samba stems from my lack facility with factors
table(dfCensus2.2010$Citizen) 
dfCensus2.2010$Citizen2 <- TRUE 
dfCensus2.2010$Citizen2[dfCensus2.2010$Citizen == "No"] <- FALSE
table(dfCensus2.2010$Citizen2)
dfCensus2.2010$Citizen <- dfCensus2.2010$Citizen2
dfCensus2.2010$Citizen2 <- NULL
str(dfCensus2.2010)

save(dfCensus2.2010, file="dfCensus2.2010.rda")

dfCensus3.2010 <- subset(dfCensus2.2010, Citizen==TRUE) 
str(dfCensus3.2010) ### 33448 obs. of  8 variables
save(dfCensus3.2010, file="dfCensus3.2010.rda")
################ End of Evil Twin 2010
##########################################

### 5. Calculate racial group's Count per each state
###    Only analyze U.S. citizens #############

census3StateRace <- group_by(dfCensus3, State, Race) 
head(census3StateRace)
dfPtsPwtStateRace <- summarise(census3StateRace, ptsPwtStateRace = sum(personalWeight))
dfRaceCountPerState <- spread(dfPtsPwtStateRace, key=Race, value=ptsPwtStateRace)
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
dfRaceCountPerState <- addTotCol(dfRaceCountPerState, 2:6, "Totals")
head(dfRaceCountPerState)

#####################################
### 8 Calculate the Count each sex per each state ... Thank you, Hadley ... :-)
census3StateSex <- group_by(dfCensus3, State, Sex) 
dfPtsPwtStateSex <- summarise(census3StateSex, ptsPwtStateSex = sum(personalWeight))
dfSexCountPerState <- spread(dfPtsPwtStateSex, key=Sex, value=ptsPwtStateSex)
dfSexCountPerState[is.na(dfSexCountPerState)] <- 0 ### Replace NAs with zeros
dfFemale <- subset(dfSexCountPerState, select=c(State, Female))
colnames(dfFemale) =  c("State", "Female")
head(dfFemale)

##################
### 9. Asian females
census3StateRaceSex <- group_by(dfCensus3, State, Sex, Race) 
dfSumPwtStateRaceSex <- summarise(census3StateRaceSex, SumPwtStateRaceSex = sum(personalWeight))
dfRaceSexPerState <- spread(dfSumPwtStateRaceSex, key=Race, value=SumPwtStateRaceSex)
dfRaceSexPerState[is.na(dfRaceSexPerState)] <- 0 ### Replace NAs with zeros
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

### 11/12. Calculate each racial group's share of total tech Count in each state
dfRaceSexCountAndShares <- addPerCols(dfRaceSexCountPerState, 2, 3:10)
head(dfRaceSexCountAndShares)

### 13. Add a totals row ... MAKE THIS A FUNCTION ... ALSO USED BY DATA-1B
dfTest <- dfRaceSexCountAndShares

### <DELETE THIS CODE>
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
tail(dfRaceSexCountAndShares)
### </DELETE THIS CODE>

dfTest  <- addTotalsRow(dfTest, 2, 3:10, 11:18, "ALL STATES")
head(dfTest)
tail(dfTest)

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
dfTest <- dfForeignTechStates

### <DELETE THIS CODE>
(allForeign <- colSums(dfForeignTechStates[,2:4]))
dfForeignTop <- dfForeignTechStates[1,] ### dummy copy first row to set the types
dfForeignTop$State <- "ALL STATES"
dfForeignTop[1,2:4] <- allForeign
dfForeignTechStates <- rbind(dfForeignTop, dfForeignTechStates)
  
dim(dfForeignTechStates)
dfForeignTechStates <- as.data.frame(dfForeignTechStates)
rownames(dfForeignTechStates) <- dfForeignTechStates[,1]

dfForeignTechStates$perAsia <- round((100 * dfForeignTechStates$Asia/dfForeignTechStates$Foreign), digits=1)
dfForeignTechStates$perNotAsia <- round((100 - dfForeignTechStates$perAsia), digits=1)
### </DELETE THIS CODE>

dfTest1 <- addPerCols(dfTest, 2, 3:4)
dfTest2  <- addTotalsRow(dfTest1, 2, 3:4, 5:6, "ALL STATES")
head(dfTest2)
tail(dfTest2)

head(dfForeignTechStates)
tail(dfForeignTechStates)

##############################
save(dfRaceSexCountAndShares, dfForeignTechStates, file="dfRaceSexCountAndShares.rda")

