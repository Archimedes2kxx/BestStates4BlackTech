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
### Table 3Z -- Profile of all U.S. Techs 2015
    dfProfileZ <- createProfile(dfProfileCitizens, group=NULL, state=NULL)
    dfProfileZ.2010 <- createProfile(dfProfileCitizens.2010, group=NULL, state=NULL)
    
    dfTable3Z <- subset(dfProfileZ, select=-c(Male, perMale))
    colnames(dfTable3Z) <- c("Occupation", "perTS", "Tech15", "Fem", "perF15")
    head(dfTable3Z)
    
    ### Table 3ZZ -- Compare all U.S. Techs 2010 to all 2015
    dfTable3ZZ <- createCompareProfile(dfProfileZ.2010, dfProfileZ)
    colnames(dfTable3ZZ) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3ZZ
    
 ### Better approach, wrap this stuff in  functions  
    df1 <- dfProfileCitizens.2010
    df2 <- dfProfileCitizens
    
    zProfiles <- createListProfiles(df1, df2) 
    (dfTable3Zz <- zProfiles[[1]])
    (dfTable3ZZz <- zProfiles[[2]])
    
    
    
    
####################################
####################################
### Table 3A -- Profile of U.S. White Techs 2015
    dfProfileA <- createProfile(dfProfileCitizens, group="White")
    dfProfileA.2010 <- createProfile(dfProfileCitizens.2010, group="White")
    
    dfTable3A <- subset(dfProfileA, select=-c(Male, perMale))
    colnames(dfTable3A) <- c("Occupation", "perTS","Tech15", "Fem", "per15")
    ### head(dfTable3A)
    
    ### Table 3AA -- Compare U.S. White Techs 2010 to all 2015
    dfTable3AA <- createCompareProfile(dfProfileA.2010, dfProfileA)
    colnames(dfTable3AA) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3AA
    
    
    aProfiles <- createListProfiles(df1, df2, group="White") 
    (dfTable3Aa <- aProfiles[[1]])
    (dfTable3AAa <- aProfiles[[2]])

####################################
####################################
### Table 3B -- Profile of U.S. Black Techs 2015
    dfProfileB <- createProfile(dfProfileCitizens, group="Black")
    dfProfileB.2010 <- createProfile(dfProfileCitizens.2010, group="Black")
    
    dfTable3B <- subset(dfProfileB, select=-c(Male, perMale))
    colnames(dfTable3B) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    head(dfTable3B)
    
    ### Table 3BB -- Compare U.S. Black Techs 2010 to all 2015
    dfTable3BB <- createCompareProfile(dfProfileB.2010, dfProfileB)
    colnames(dfTable3BB) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    head(dfTable3BB)

####################################
####################################
### Table 3C -- Profile of U.S. Hispanic Techs 2015
    dfProfileC <- createProfile(dfProfileCitizens, group="Hispanic")
    dfProfileC.2010 <- createProfile(dfProfileCitizens.2010, group="Hispanic")
    
    dfTable3C <- subset(dfProfileC, select=-c(Male, perMale))
    colnames(dfTable3C) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    head(dfTable3C)
    
    ### Table 3CC -- Compare U.S. Hispanic Techs 2010 to all 2015
    dfTable3CC <- createCompareProfile(dfProfileC.2010, dfProfileC)
    colnames(dfTable3CC) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    head(dfTable3CC)

####################################
####################################
### Table 3D -- Profile of U.S. Asian Techs 2015
    dfProfileD <- createProfile(dfProfileCitizens, group="Asian")
    dfProfileD.2010 <- createProfile(dfProfileCitizens.2010, group="Asian")
    
    dfTable3D <- subset(dfProfileD, select=-c(Male, perMale))
    colnames(dfTable3D) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    head(dfTable3D)
    
    ### Table 3DD -- Compare U.S. Hispanics Techs 2010 to all 2015
    dfTable3DD <- createCompareProfile(dfProfileD.2010, dfProfileD)
    colnames(dfTable3DD) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    head(dfTable3DD)

####################################
####################################
### Table 3E -- Profile of Foreign Techs 2015
    dfProfileE <- createProfile(dfProfileForeigners, group="")
    dfProfileE.2010 <- createProfile(dfProfileForeigners.2010, group="")
    
    dfTable3E <- subset(dfProfileE, select=-c(Male, perMale))
    colnames(dfTable3E) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    head(dfTable3E)
    
    ### Table 3EE -- Compare Foreign Techs 2010 to all 2015
    dfTable3EE <- createCompareProfile(dfProfileE.2010, dfProfileE)
    colnames(dfTable3EE) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    head(dfTable3EE)

####################################
####################################
### Table 3FL -- Profile of Foreign Techs in Florida in 2015
    print("Florida ... Florida Florida")
    dfProfileFL <- createProfile(dfProfileForeignersState, group="", state="Florida")
    dfProfileFL.2010 <- createProfile(dfProfileForeignersState.2010, group="", state="Florida")
    
    dfTable3FL <- subset(dfProfileFL, select=-c(Male, perMale))
    colnames(dfTable3FL) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    dfTable3FL
    
    ### Table 3FLFL -- Compare Foreign Techs 2010 to all 2015
    dfTable3FLFL <- createCompareProfile(dfProfileFL.2010, dfProfileFL)
    colnames(dfTable3FLFL) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3FLFL
    
print("END OF Florida ... Florida Florida")

###############################################
###############################################    
### Table 3F -- Profile of Foreign Techs in California in 2015
print("California ... California California")
    dfProfileF <- createProfile(dfProfileForeignersState, group="", state="California")
    dfProfileF.2010 <- createProfile(dfProfileForeignersState.2010, group="", state="California")
    ### dfProfileF.2010
    
    dfTable3F <- subset(dfProfileF, select=-c(Male, perMale))
    colnames(dfTable3F) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    dfTable3F
    
    ### Table 3FF -- Compare Foreign Techs 2010 to all 2015
    dfTable3FF <- createCompareProfile(dfProfileF.2010, dfProfileG)
    colnames(dfTable3FF) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    dfTable3FF

print("END OF California ... California California")



########################################
########################################
save(dfTable1, dfTable1A, dfTable1B, dfTable1C, dfTable1D, dfTable1E, dfTable2A, dfTable2B, dfTable3Z, dfTable3ZZ, dfProfileZ.2010, dfTable3A, dfTable3AA, dfTable3B, dfTable3BB, dfTable3C, dfTable3CC, dfTable3D, dfTable3DD, dfTable3E, dfTable3EE, dfTable3F, dfTable3FF,  dfTable3FL, dfTable3FLFL, file="dfTab1A1B2A2B3ABCDEF.rda")
