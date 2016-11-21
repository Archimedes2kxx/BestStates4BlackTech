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
    df1 <- dfProfileCitizens.2010
    df2 <- dfProfileCitizens
    
### Table 3Z -- Profile of all U.S. Techs 2015
   Profiles <- createListProfiles(df1, df2) 
   (dfTable3Z <- Profiles[[1]])
   (dfTable3ZZ <- Profiles[[2]])
    
####################################
####################################
### Table 3A -- Profile of U.S. White Techs 2015
    dfProfileA <- createProfile(dfProfileCitizens, group="White")
    dfProfileA.2010 <- createProfile(dfProfileCitizens.2010, group="White")
    
    dfTable3Ax <- subset(dfProfileA, select=-c(Male, perMale))
    colnames(dfTable3Ax) <- c("Occupation", "perTS","Tech15", "Fem", "per15")
    dfTable3Ax
    
    ### Table 3AA -- Compare U.S. White Techs 2010 to all 2015
    dfTable3AAx <- createCompareProfile(dfProfileA.2010, dfProfileA)
    colnames(dfTable3AAx) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3AAx
    
    Profiles <- createListProfiles(df1, df2, group="White") 
    (dfTable3A <- Profiles[[1]])
    (dfTable3AA <- Profiles[[2]])

####################################
####################################
### Table 3B -- Profile of U.S. Black Techs 2015
    dfProfileB <- createProfile(dfProfileCitizens, group="Black")
    dfProfileB.2010 <- createProfile(dfProfileCitizens.2010, group="Black")
    
    dfTable3Bx <- subset(dfProfileB, select=-c(Male, perMale))
    colnames(dfTable3Bx) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    dfTable3Bx
    
    ### Table 3BB -- Compare U.S. Black Techs 2010 to all 2015
    dfTable3BBx <- createCompareProfile(dfProfileB.2010, dfProfileB)
    colnames(dfTable3BBx) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3BBx
    
    Profiles <- createListProfiles(df1, df2, group="Black") 
    (dfTable3B <- Profiles[[1]])
    (dfTable3BB <- Profiles[[2]])
    
####################################
####################################
### Table 3C -- Profile of U.S. Hispanic Techs 2015
    dfProfileC <- createProfile(dfProfileCitizens, group="Hispanic")
    dfProfileC.2010 <- createProfile(dfProfileCitizens.2010, group="Hispanic")
    
    dfTable3Cx <- subset(dfProfileC, select=-c(Male, perMale))
    colnames(dfTable3Cx) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    dfTable3Cx
    
    ### Table 3CC -- Compare U.S. Hispanic Techs 2010 to all 2015
    dfTable3CCx <- createCompareProfile(dfProfileC.2010, dfProfileC)
    colnames(dfTable3CCx) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3CCx
    
    Profiles <- createListProfiles(df1, df2, group="Hispanic") 
    (dfTable3C <- Profiles[[1]])
    (dfTable3CC <- Profiles[[2]])

####################################
####################################
### Table 3D -- Profile of U.S. Asian Techs 2015
    dfProfileD <- createProfile(dfProfileCitizens, group="Asian")
    dfProfileD.2010 <- createProfile(dfProfileCitizens.2010, group="Asian")
    
    dfTable3Dx <- subset(dfProfileD, select=-c(Male, perMale))
    colnames(dfTable3Dx) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    dfTable3Dx
    
    ### Table 3DD -- Compare U.S. Hispanics Techs 2010 to all 2015
    dfTable3DDx <- createCompareProfile(dfProfileD.2010, dfProfileD)
    colnames(dfTable3DDx) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3DDx
    
    Profiles <- createListProfiles(df1, df2, group="Asian") 
    (dfTable3D <- Profiles[[1]])
    (dfTable3DD <- Profiles[[2]])

####################################
####################################
    dfFor1 <- dfProfileForeignersState.2010
    dfFor2 <- dfProfileForeignersState    
    
### Table 3E -- Profile of Foreign Techs 2015
    Profiles <- createListProfiles(dfFor1, dfFor2) 
    (dfTable3E <- Profiles[[1]])
    (dfTable3EE <- Profiles[[2]])

###############################################
###############################################    
### Table 3F -- Profile of Foreign Techs in California in 2015
print("California ... California California")
    dfProfileF <- createProfile(dfProfileForeignersState, group=NULL, state="California")
    dfProfileF.2010 <- createProfile(dfProfileForeignersState.2010, group=NULL, state="California")
    ### dfProfileF.2010
    
    dfTable3Fx <- subset(dfProfileF, select=-c(Male, perMale))
    colnames(dfTable3Fx) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    dfTable3Fx
    
    ### Table 3FF -- Compare Foreign Techs 2010 to all 2015
    dfTable3FFx <- createCompareProfile(dfProfileF.2010, dfProfileG)
    colnames(dfTable3FFx) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3FFx

    Profiles <- createListProfiles(dfFor1, dfFor2, state="California") 
    (dfTable3F <- Profiles[[1]])
    (dfTable3FF <- Profiles[[2]])    
    
    (dfTable3CAfor <- dfTable3F)
    (dfTable3CACAfor <- dfTable3FF)
print("END OF California ... California California")

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
save(dfTable1, dfTable1A, dfTable1B, dfTable1C, dfTable1D, dfTable1E, dfTable2A, dfTable2B, dfTable3Z, dfTable3ZZ, dfProfileZ.2010, dfTable3A, dfTable3AA, dfTable3B, dfTable3BB, dfTable3C, dfTable3CC, dfTable3D, dfTable3DD, dfTable3E, dfTable3EE, dfTable3F, dfTable3FF, file="dfTab1A1B2A2B3ABCDEF.rda")

save(dfTable3CAfor, dfTable3CACAfor, dfTable3TXfor, dfTable3TXTXfor, dfTable3NYfor, dfTable3NYNYfor, dfTable3FLfor, dfTable3FLFLfor, dfTable3VAfor, dfTable3VAVAfor, dfTable3ILfor, dfTable3ILILfor, file="APPENDIX.rda")

