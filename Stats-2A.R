### Stat-2A Demographic tables and profiles ... Tables 1, 2, and 3

################################################
################################################ 
### Use map_dat() in ggplot2 as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################
    
    load(file="functions-0.rda")
    load(file="dfRaceSexCountAndShares.rda")
    load(file="dfRaceSexCountAndShares.2010.rda")
    load("dfProfiles.rda")
    load("dfProfiles.2010.rda")
    load(file="dfStatesPop3.rda")
    
    ### install.packages("gridExtra")
    library(tidyr)
    library(maps)
    library(mapproj) ### needed by ggplot2, but not installed automatically
    library(ggplot2)
    library(maps)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(gtable)

######################
### Table 1A. Total U.S. workforce, citizens, foreign workers in 2010
    totCitizens <- dfStatesPop3["ALL STATES","Totals"]
    totForeign <- dfStatesPop3Foreign["ALL STATES","Totals"]
    rowDf <- data.frame(Citizens=totCitizens, Foreign=totForeign)
    
    dfTable1A<- makeNumPerTable(rowDf)
    print(dfTable1A, quote=FALSE)

######################    
### Table 1B. White, Black, Asian, Hispanic American Components of U.S. Population in 2014
    rowDf <- dfStatesPop3[1,2:7]
    colnames(rowDf) <- c("Total", "White", "Black", "Asian", "Hispanic", "OTHERS")
    Total <- rowDf[1,1]
    
    dfTable1B<- makeNumPerTable(rowDf, Total)
    print(dfTable1B, quote=FALSE)

######################      
### Table 1C U.S. Tech Sector
    totCitTech <- as.numeric(dfRaceSexCountAndShares["ALL STATES", "Totals"])
    totForTech <- as.numeric(dfForeignRaceSexCountAndShares["ALL STATES", "Totals"])
    rowDf <- data.frame(Citizens=totCitTech, Foreign=totForTech)
    
    dfTable1C<- makeNumPerTable(rowDf)
    print(dfTable1C, quote=FALSE)    

######################      
### Table 1D. White, Black, Asian, Hispanic Components of U.S. Tech Sector in 2014
    rowDf <- dfRaceSexCountAndShares[1,2:7]
    colnames(rowDf) <- c("Total", "White", "Black", "Asian", "Hispanic", "OTHERS")
    Total <- rowDf[1,1]
    
    dfTable1D<- makeNumPerTable(rowDf, Total)
    print(dfTable1D, quote=FALSE)    

######################      
### Table 1E. Foreign professionals in U.S. Tech
    totForeignTech <-dfForeignRaceSexCountAndShares["ALL STATES", "Totals"]
    totAsianTech <- dfForeignRaceSexCountAndShares["ALL STATES", "Asian"]
    totNonAsianTech <- totForeignTech - totAsianTech
    
    rowDf <- data.frame(Total=totForeignTech, Asian=totAsianTech, `NonAsian`=totNonAsianTech)
    dfTable1E<- makeNumPerTable(rowDf, Total)
    print(dfTable1E, quote=FALSE) 
    
##########################
##########################
### Table 2A. Female Components of U.S. U.S. Population  in 2014
    totPop <- dfStatesPop3[1, "Totals"]
    totFemale <- dfStatesPop3[1, "Female"]
    totMale <- totPop - totFemale
    
    rowDf <- data.frame(Total=totPop, Male=totMale, Female=totFemale)
    dfTable2Aleft <- makeNumPerTable(rowDf, Total)
    ### print(dfTable2Aleft, quote=FALSE)
    
    totFemAsian <-dfStatesPop3[1, "FemAsian"]
    totFemNonAsian <- dfStatesPop3[1, "FemNonAsian"]
    rowDf <- data.frame(Total=totPop, FemAsian=totFemAsian, FemNonAsian=totFemNonAsian)
    dfTable2Aright <- makeNumPerTable(rowDf, Total)
    dfTable2Aright <- subset(dfTable2Aright, select=-Total)
   ### print(dfTable2Aright, quote=FALSE)
    
    dfTable2A <- cbind(dfTable2Aleft, dfTable2Aright)
    print(dfTable2A, quote=FALSE)

######################    
### Table 2B. Male/Female Components of of U.S. Tech Sector in 2014
    totPop <- dfRaceSexCountAndShares[1, "Totals"]
    totFemale <- dfRaceSexCountAndShares[1, "Female"]
    totMale <- totPop - totFemale
    
    rowDf <- data.frame(Total=totPop, Male=totMale, Female=totFemale)
    dfTable2Bleft <- makeNumPerTable(rowDf, Total)
    ### print(dfTable2Bleft, quote=FALSE)
    
    totFemAsian <-dfRaceSexCountAndShares[1, "FemAsian"]
    totFemNonAsian <- dfRaceSexCountAndShares[1, "FemNonAsian"]
    rowDf <- data.frame(Total=totPop, FemAsian=totFemAsian, FemNonAsian=totFemNonAsian)
    dfTable2Bright <- makeNumPerTable(rowDf, Total)
    dfTable2Bright <- subset(dfTable2Bright, select=-Total)
    ### print(dfTable2Bright, quote=FALSE)
    
    dfTable2B <- cbind(dfTable2Bleft, dfTable2Bright)
    print(dfTable2B, quote=FALSE)

###########################
############################
### Initialize raw profile data frames
    dfCit1 <- dfProfileCitizens.2010
    dfCit2 <- dfProfileCitizens
    
    dfCitR1 <- dfProfileCitizens_RawRace.2010
    dfCitR2 <- dfProfileCitizens_RawRace

    dfCitRS1 <- dfProfileCitizens_RawRaceState.2010
    dfCitRS2 <- dfProfileCitizens_RawRaceState
    
    dfFor1 <- dfProfileForeigners.2010
    dfFor2 <- dfProfileForeigners  
    
    dfForS1 <- dfProfileForeigners_RawState.2010
    dfForS2 <- dfProfileForeigners_RawState 

####################################
####################################        
### Table 3Z -- Profile of all U.S. Techs 2015
   Profiles <- createListProfiles(dfCit1, dfCit2) 
   (dfTable3Z <- Profiles[[1]])
   (dfTable3ZZ <- Profiles[[2]])
    
####################################
####################################
### Table 3A -- Profile of U.S. White Techs 2015

    Profiles <- createListProfiles(dfCitR1, dfCitR2, group="White") 
    (dfTable3A <- Profiles[[1]])
    (dfTable3AA <- Profiles[[2]])

####################################
####################################
### Table 3B -- Profile of U.S. Black Techs 2015
    
    Profiles <- createListProfiles(dfCitR1, dfCitR2, group="Black") 
    (dfTable3B <- Profiles[[1]])
    (dfTable3BB <- Profiles[[2]])
    
####################################
####################################
### Table 3C -- Profile of U.S. Hispanic Techs 2015

    Profiles <- createListProfiles(dfCitR1, dfCitR2, group="Hispanic") 
    (dfTable3C <- Profiles[[1]])
    (dfTable3CC <- Profiles[[2]])

####################################
####################################
### Table 3D -- Profile of U.S. Asian Techs 2015
    
    Profiles <- createListProfiles(dfCitR1, dfCitR2, group="Asian") 
    (dfTable3D <- Profiles[[1]])
    (dfTable3DD <- Profiles[[2]])

####################################
####################################
    
### Table 3E -- Profile of Foreign Techs 2015
    Profiles <- createListProfiles(dfFor1, dfFor2) 
    (dfTable3E <- Profiles[[1]])
    (dfTable3EE <- Profiles[[2]])

###############################################
###############################################    
### Table 3F -- Profile of Foreign Techs in California in 2015
    
    Profiles <- createListProfiles(dfForS1, dfForS2, state="California") 
    (dfTable3F <- Profiles[[1]])
    (dfTable3FF <- Profiles[[2]])    
    
###############################################
############################################### 
### APPENDIX 
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="California", group="White") 
    (dfTable3CAwhite <- Profiles[[1]])
    (dfTable3CACAwhite <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="California", group="Black") 
    (dfTable3CAblack <- Profiles[[1]])
    (dfTable3CACAblack <- Profiles[[2]])  

    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="California", group="Hispanic") 
    (dfTable3CAhispanic <- Profiles[[1]])
    (dfTable3CAhispanic <- Profiles[[2]])      
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="California", group="Asian") 
    (dfTable3CAasian <- Profiles[[1]])
    (dfTable3CAasian <- Profiles[[2]])    
    
    (dfTable3CAfor <- dfTable3F)
    (dfTable3CACAfor <- dfTable3FF) 
    
####################################
####################################
### Table 3FL -- Profile of Foreign Techs in Florida in 2015
print("Florida ... Florida Florida")

Profiles <- createListProfiles(dfFor1, dfFor2, state="Florida") 
(dfTable3FLfor <- Profiles[[1]])
(dfTable3FLFLfor <- Profiles[[2]])    

####################################
####################################
### Table 3NY -- Profile of Foreign Techs in New York in 2015

Profiles <- createListProfiles(dfFor1, dfFor2, state="New York") 
(dfTable3NYfor <- Profiles[[1]])
(dfTable3NYNYfor <- Profiles[[2]]) 

####################################
####################################
### Table 3TX -- Profile of Foreign Techs in Texas in 2015

Profiles <- createListProfiles(dfFor1, dfFor2, state="Texas") 
(dfTable3TXfor <- Profiles[[1]])
(dfTable3TXTXfor <- Profiles[[2]]) 

########################################
########################################
### Table 3VA -- Profile of Foreign Techs in Virginia in 2015

Profiles <- createListProfiles(dfFor1, dfFor2, state="Virginia") 
(dfTable3VAfor <- Profiles[[1]])
(dfTable3VAVAfor <- Profiles[[2]]) 

########################################
########################################
### Table 3IL -- Profile of Foreign Techs in Illinois in 2015

Profiles <- createListProfiles(dfFor1, dfFor2, state="Illinois") 
(dfTable3ILfor <- Profiles[[1]])
(dfTable3ILILfor <- Profiles[[2]]) 

########################################
########################################
### Table 3IL -- Black American Techs in California  in 2015
Profiles <- createListProfiles(dfRS1, dfRS2, state="California", group="Black") 
(dfTable3ILfor <- Profiles[[1]])
(dfTable3ILILfor <- Profiles[[2]]) 

########################################
########################################
save(dfTable1A, dfTable1B, dfTable1C, dfTable1D, dfTable1E, dfTable2A, dfTable2B, dfTable3Z, dfTable3ZZ, dfTable3A, dfTable3AA, dfTable3B, dfTable3BB, dfTable3C, dfTable3CC, dfTable3D, dfTable3DD, dfTable3E, dfTable3EE, dfTable3F, dfTable3FF, file="dfTab1A1B2A2B3ABCDEF.rda")

save(dfTable3CAfor, dfTable3CACAfor, dfTable3TXfor, dfTable3TXTXfor, dfTable3NYfor, dfTable3NYNYfor, dfTable3FLfor, dfTable3FLFLfor, dfTable3VAfor, dfTable3VAVAfor, dfTable3ILfor, dfTable3ILILfor, file="APPENDIX.rda")

