### Data-1 scripts create the data frames used by the Stats-2A, 2B, and 2C files to make tables

### Read PUMS 2015 sample data from files downloaded from U.S. Census DataWeb site using DataFerret
### ==> data about techs in each of the 50 states plus DC and data about total workforce population

######################################
######################################

### Download a special PUMS files for foreign techs in 2010 and 2015 ... no citizens
### Hopefully it wil have enough observations to enable reliable totals for states, race, etc

######################################
######################################

####################
    load(file="functions-0.rda")
    library(dplyr)
    library(tidyr)
 
#################################   
### 1. Read downloaded PUMS sample data from 2015 for techs ... citizens and foreigners
    file = "Race-Sex-Hisp-Citizen-WAOB-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2015-DataFerret.csv"
    dfCensus1Year2015 = read.csv(file, header=TRUE, stringsAsFactors = FALSE)
    ###save(dfCensus1Year2015, file="dfCensus1Year2015.rda")
    dfCensus2 = dfCensus1Year2015
    
    str(dfCensus2) ### Verify order of variables from PUMS
    colnames(dfCensus2) = c("personalWeight", "Hisp", "Birth" , "Citizen", "Sex", "State", "Race", "Occupation") 
    str(dfCensus2) ### 45081 obs. of  8 variables
    save(dfCensus2, file="dfCensus2.rda")
    
### Data for Tables 4 about techs
    dfRaceSexCountAndShares <-createPopRaceAndShares(dfCensus2, bCitizen=TRUE)
    dfForeignRaceSexCountAndShares <-createPopRaceAndShares(dfCensus2, bCitizen=FALSE)
    
    save(dfRaceSexCountAndShares, dfForeignRaceSexCountAndShares, file="dfRaceSexCountAndShares.rda")
    
###########################    
### Profiles data for Tables 3
    
    dfProfileCitizens_RawRaceState <-createProfiles_OccRaceState(dfCensus2) 
    dfProfileCitizens_RawState <- createProfiles_OccState(dfCensus2)
    dfProfileCitizens_RawRace <- createProfiles_OccRace(dfCensus2)
    dfProfileCitizens <- createProfiles_Occ(dfCensus2)
    
    dfProfileForeigners_RawRaceState <- createProfiles_OccRaceState(dfCensus2, bCitizen=FALSE) 
    dfProfileForeigners_RawState <- createProfiles_OccState(dfCensus2, bCitizen=FALSE)
    dfProfileForeigners_RawRace <- createProfiles_OccRace(dfCensus2, bCitizen=FALSE)
    dfProfileForeigners <- createProfiles_Occ(dfCensus2, bCitizen=FALSE)

#################################   
    save(dfProfileCitizens, dfProfileCitizens_RawRace, dfProfileCitizens_RawState, dfProfileCitizens_RawRaceState, dfProfileForeigners, dfProfileForeigners_RawState, dfProfileForeigners_RawRace, dfProfileForeigners_RawRaceState, file="dfProfiles.rda")

    
#################################
#################################
### 1B. Data from 2010 ... Mostly duplicate of previous code ... If something goes wrong 
###     with the data, I want to see exactly where it happened ... So don't wrap this in a function
    
    file2010 = "Race-Sex-Hisp-Citizen-WAOB-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2010-DataFerret.csv"
    dfCensus1Year2010 = read.csv(file2010, header=TRUE, stringsAsFactors = FALSE)
    dfCensus2.2010 = dfCensus1Year2010
    
    str(dfCensus2.2010) ### Verify order of variables from PUMS ########## DIFFERS FROM 2015
    
    colnames(dfCensus2.2010) = c("personalWeight", "Hisp", "State" , "Race", "Birth", "Citizen", "Sex", "Occupation") 
    str(dfCensus2.2010) ### 45081 obs. of  8 variables
    save(dfCensus2.2010, file="dfCensus2.2010.rda")
    
    ### Data for Tables 4 for 2010 ... not used so far
    dfRaceSexCountAndShares.2010 <-createPopRaceAndShares(dfCensus2.2010, bCitizen=TRUE)
    dfForeignRaceSexCountAndShares.2010 <-createPopRaceAndShares(dfCensus2.2010, bCitizen=FALSE)
    
    save(dfRaceSexCountAndShares.2010, dfForeignRaceSexCountAndShares.2010, file="dfRaceSexCountAndShares.2010.rda")
    
###########################    
### Profiles 2010 data for Tables 3
    
    dfProfileCitizens_RawRaceState.2010 <-createProfiles_OccRaceState(dfCensus2.2010) 
    dfProfileCitizens_RawState.2010 <- createProfiles_OccState(dfCensus2.2010)
    dfProfileCitizens_RawRace.2010 <- createProfiles_OccRace(dfCensus2.2010)
    dfProfileCitizens.2010 <- createProfiles_Occ(dfCensus2.2010)
    
    dfProfileForeigners_RawRaceState.2010 <- createProfiles_OccRaceState(dfCensus2.2010, bCitizen=FALSE) 
    dfProfileForeigners_RawState.2010 <- createProfiles_OccState(dfCensus2.2010, bCitizen=FALSE)
    dfProfileForeigners_RawRace.2010 <- createProfiles_OccRace(dfCensus2.2010, bCitizen=FALSE)
    dfProfileForeigners.2010 <- createProfiles_Occ(dfCensus2.2010, bCitizen=FALSE)

    ########################  
    save(dfProfileCitizens.2010, dfProfileCitizens_RawRace.2010, dfProfileCitizens_RawState.2010, dfProfileCitizens_RawRaceState.2010, dfProfileForeigners.2010, dfProfileForeigners_RawState.2010, dfProfileForeigners_RawRace.2010, dfProfileForeigners_RawRaceState.2010, file="dfProfiles.2010.rda")
    
##########################
#########################a
###  Data for Total workforce population 2015 for Tables 4
###  Read downloaded PUMS sample data and save ... NO occupation, NO area of birth
    
    file = "Race-Sex-Hisp-Citizen-AllStates-PersonalWeight-PUMS-2015-DataFerret.csv"
    dfCensusPop1 = read.csv(file, header=TRUE, stringsAsFactors = FALSE)
    head(dfCensusPop1)
    save(dfCensusPop1, file="dfCensusPop1.rda")
    str(dfCensusPop1) ### 1411289 obs. of  6 variables:
    
    sum(dfCensusPop1$PWGTP) ### 2015 total U.S. workforce = 148,297,138 includes non-citizens
    
    colnames(dfCensusPop1) = c("personalWeight", "Race", "Sex",   "Citizen", "State", "Hisp") 
    ### table(dfCensusPop1$Citizen) ### 95867  non-citizens on 11/6/16
    
    dfStatesPop3 <-createPopRaceAndShares(dfCensusPop1, bCitizen=TRUE)
    head(dfStatesPop3)
    
    dfStatesPop3Foreign <- createPopRaceAndShares(dfCensusPop1, bCitizen=FALSE)
    head(dfStatesPop3Foreign)
 
######################   
    save(dfStatesPop3, dfStatesPop3Foreign, file = "dfStatesPop3.rda")
    