### A. Read PUMS 2015 sample data from files downloaded from U.S. Census DataWeb site using DataFerret to get population estimates of citizens in U.S. workforce by states

### These scripts are ugly ... but they get the job done
### Goal: Convert more chunks into readable functions, then move functions into the functions-0.R file
### ===> Reduce the size of the ugly remaining scripgs; hopefully make them more readable

library(dplyr)
library(tidyr)
load(file = "functions-0.rda")

### 1. Read downloaded PUMS sample data and save ... NO occupation, NO area of birth
file = "Race-Sex-Hisp-Citizen-AllStates-PersonalWeight-PUMS-2015-DataFerret.csv"
dfCensusPop1 = read.csv(file, header=TRUE, stringsAsFactors = FALSE)
head(dfCensusPop1)
save(dfCensusPop1, file="dfCensusPop1.rda")
str(dfCensusPop1) ### 1411289 obs. of  6 variables:

sum(dfCensusPop1$PWGTP) ### 2015 total U.S. workforce = 148,297,138 includes non-citizens

str(dfCensusPop1) ### 1411289 obs. of  6 variables:
colnames(dfCensusPop1) = c("personalWeight", "Race", "Sex",   "Citizen", "State", "Hisp") 
str(dfCensusPop1) ### 1411289 obs. of  6 variables:
table(dfCensusPop1$Citizen) ### 95867  non-citizens on 11/6/16

### 2. Add new category to race = "hisp"
### ... ACS coded HISP = "1" for "not Hispanic" so change race values to 99 ("hispanic") when hisp != 1
rows <- dfCensusPop1$Hisp != "1"
dfCensusPop1$Race[rows] <- 99
str(dfCensusPop1) ### 1315422 obs. of  5 variables:
head(dfCensusPop1)

### 3. load codes from file
listCodes <- readCodeBooks()

### 4. Convert coded categorical variables to factors ... sex, race, state ... drop padding/blanks before/after each category 
dfCensusPop1$Race <- as.factor(dfCensusPop1$Race)
(levels(dfCensusPop1$Race) <- trimws(listCodes[["Race"]][,2]))

dfCensusPop1$Sex <- as.factor(dfCensusPop1$Sex)
(levels(dfCensusPop1$Sex) <- trimws(listCodes[["Sex"]][,2]))

dfCensusPop1$State <- as.factor(dfCensusPop1$State)
levels(dfCensusPop1$State) <- trimws(listCodes[["State"]][,2])
### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping
levels(dfCensusPop1$State)

dfCensusPop1$Citizen <- as.factor(dfCensusPop1$Citizen)
levels(dfCensusPop1$Citizen) <- trimws(listCodes[["Citizen"]][,2])
levels(dfCensusPop1$Citizen)
str(dfCensusPop1) 

### This samba stems from my lack facility with factors
table(dfCensusPop1$Citizen) ### 4803 non-citizens
dfCensusPop1$Citizen2 <- TRUE 
dfCensusPop1$Citizen2[dfCensusPop1$Citizen == "No"] <- FALSE
table(dfCensusPop1$Citizen2) ### false, true = 95867 1315422 
dfCensusPop1$Citizen <- dfCensusPop1$Citizen2
dfCensusPop1$Citizen2 <- NULL
str(dfCensusPop1)

save(dfCensusPop1, file="dfCensusPop1.rda")

###sum(is.na(dfCensusPop1$Citizen))

dfCensusPop2 <- subset(dfCensusPop1, Citizen==TRUE) 
str(dfCensusPop2) ### 1315422 obs. of  5 variables:
dfCensusPop2 <- subset(dfCensusPop2, select=c(personalWeight, State, Race, Sex))
str(dfCensusPop2)
head(dfCensusPop2)
sum(dfCensusPop2$personalWeight) ### 135475088 U.S. citizens in workforce

######################
### 5. Calculate racial group's Count per each state ... Thank you, Hadley ... :-)
censusPop2StateRace <- group_by(dfCensusPop2, Race, State) 
dfPtsPwtStateRace <- summarise(censusPop2StateRace, ptsPwtStateRace = sum(personalWeight))
dfRaceCountPerState <- spread(dfPtsPwtStateRace, key=Race, value=ptsPwtStateRace, drop=FALSE, fill=0)
head(dfRaceCountPerState)
### dfRaceCountPerState[is.na(dfRaceCountPerState)] <- 0 ### Replace NAs with zeros

##############
### 6. Combine all groups other than black, white, asian, and hispanic into OTHERS
columnNames <- c("State", "White", "Black", "amInAlNat", "alNat", "otherNat", "Asian", "pacific", "other", "mixed" , "Hispanic")
colnames(dfRaceCountPerState) <- columnNames
dfRaceCountPerState$OTHERS <- dfRaceCountPerState$amInAlNat + dfRaceCountPerState$alNat + dfRaceCountPerState$otherNat + dfRaceCountPerState$pacific + dfRaceCountPerState$other + dfRaceCountPerState$mixed

### Delete components of OTHERS
dfRaceCountPerState <- subset(dfRaceCountPerState, select=-c(amInAlNat, alNat,otherNat, pacific, other, mixed))
head(dfRaceCountPerState)

#################
### 7. Add "totals" column after "state" ... 
dfRaceCountPerState <- addTotCol(dfRaceCountPerState, 2:6, "TotPop")
head(dfRaceCountPerState)

#####################################
### 8 Calculate the Count each sex per each state ... Thank you, Hadley ... :-)
censusPop2StateSex <- group_by(dfCensusPop2, State, Sex) 
head(censusPop2StateSex)
dfPtsPwtStateSex <- summarise(censusPop2StateSex, ptsPwtStateSex = sum(personalWeight))
dfSexCountPerState <- spread(dfPtsPwtStateSex, key=Sex, value=ptsPwtStateSex)
dfSexCountPerState[is.na(dfSexCountPerState)] <- 0 ### Replace NAs with zeros
dfFemale <- subset(dfSexCountPerState, select=c(State, Female))
colnames(dfFemale) =  c("State", "Female")
head(dfFemale)

##################
### 9. Asian females
censusPop2StateRaceSex <- group_by(dfCensusPop2, State, Sex, Race) 
dfSumPwtStateRaceSex <- summarise(censusPop2StateRaceSex, SumPwtStateRaceSex = sum(personalWeight))
dfRaceSexPerState <- spread(dfSumPwtStateRaceSex, key=Race, value=SumPwtStateRaceSex)
dfRaceSexPerState[is.na(dfRaceSexPerState)] <- 0 ### Replace NAs with zeros
dfFemAsian <- subset(dfRaceSexPerState, Sex=="Female", select=c(State,Asian))
colnames(dfFemAsian) =  c("State", "FemAsian")
head(dfFemAsian)

#################
### 10. Combine the two female dfs, create FemNonAsian
dfFemale <- merge(dfFemale, dfFemAsian)
dfFemale$FemNonAsian <- dfFemale$Female - dfFemale$FemAsian
head(dfFemale)

##### column merge dfFemales at this point to end 
##dfRaceSexCountPerState2 <- dfRaceSexCountPerState
dfRaceSexCountPerState <- merge(dfRaceCountPerState, dfFemale)
head(dfRaceSexCountPerState)

### 11/12. Calculate each racial group's share of total tech Count in each state
dfRaceSexCountAndShares <- addPerCols(dfRaceSexCountPerState, 2, 3:10)

### 13. Add a totals row ... MAKE THIS A FUNCTION ... ALSO USED BY DATA-1A
(allRacesInPop <- colSums(dfRaceSexCountAndShares[,3:10]))
(allPop <- sum(allRacesInPop[1:5])) ### 148297138 ... don't include females for total
(raceSharesInPop <- round(100 * allRacesInPop/allPop, digits=1))

dfTest <- dfRaceSexCountAndShares

dfTotalsRow <- dfRaceSexCountAndShares[1,] ### dummy coy to get columns and types
dfTotalsRow$State <- "ALL STATES"
dfTotalsRow[1,3:10] <- allRacesInPop
dfTotalsRow[1,11:18] <- raceSharesInPop
dfTotalsRow[1,2] <- allPop
dfRaceSexCountAndShares <- rbind(dfTotalsRow, dfRaceSexCountAndShares)

dfStatesPop3 <- dfRaceSexCountAndShares ### rename
rownames(dfStatesPop3) <- dfStatesPop3$State
head(dfStatesPop3)
tail(dfStatesPop3)
save(dfStatesPop3, file = "dfStatesPop3.rda")

dfTest  <- addTotalsRow(dfTest, 2, 3:10, 11:18, "ALL STATES")
head(dfTest)
tail(dfTest)
    