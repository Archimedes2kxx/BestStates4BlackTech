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
    
    ### head(dfStatesPop3Foreign)
    totCitizens <- dfStatesPop3["ALL STATES","Totals"]
    totForeign <- dfStatesPop3Foreign["ALL STATES","Totals"]
    totWorkforce <- totCitizens + totForeign
    perCit <- round(100*totCitizens/totWorkforce, digits=1)
    perFor <- round(100*totForeign/totWorkforce, digits=1)
    perTot <- 100.0

    df1 <- data.frame(totWorkforce, totCitizens, totForeign)
    df1 <- prettyNum(df1, big.mark = ",") 
    df2 <- as.character(data.frame(perTot, perCit, perFor))
    dfTable1A <- rbind(df1, df2)
    
    colnames(dfTable1A) <- c("Total", "Citizens", "Foreign")
    rownames(dfTable1A) <- c("Num", "Per")
    print(dfTable1A, quote=FALSE)
   
    
### Table 1B. White, Black, Asian, Hispanic American Components of U.S. Population in 2014
    dfTable1Bpop <- dfStatesPop3[1,2:7]
    colNames <- c("ALL", "White", "Black", "Asian", "Hispanic", "OTHERS")
    colnames(dfTable1Bpop) <- colNames
    dfTable1Bpop <- prettyNum(dfTable1Bpop, big.mark = ",") 
    ### dfTable1Bpop
    
    dfTable1Bper <- dfStatesPop3[1,c(2,11:15)]
    dfTable1Bper[1,1] <- 100 ### 100 percent for ALL
    colNames_per <- c("ALL", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS")
    colnames(dfTable1Bper) <- colNames_per
    dfTable1Bper <- as.character(round(dfTable1Bper, digits=1))
    
    dfTable1B <- data.frame(rbind(dfTable1Bpop, dfTable1Bper))
    rownames(dfTable1B) <- c("Num", "Per")
    print(dfTable1B, quote=FALSE)

### Table 1C U.S. Tech Sector
    (totCitTech <- dfRaceSexCountAndShares["ALL STATES", "Totals"])
    (totForTech <- dfForeignRaceSexCountAndShares["ALL STATES", "Totals"])
    totTech <- totCitTech + totForTech
    perCitTech <- round(100 * totCitTech/totTech, digits=1)
    perForTech <- round(100 * totForTech/totTech, digits=1)
    perTotTech <- 100.0
    
    df1 <- data.frame(totTech, totCitTech, totForTech)
    df1 <- prettyNum(df1, big.mark = ",") 
    df2 <- as.character(data.frame(perTotTech, perCitTech, perForTech))
    dfTable1C <- rbind(df1, df2)
    
    colnames(dfTable1C) <- c("Total", "Citizens", "Foreign")
    rownames(dfTable1C) <- c("Num", "Per")
    print(dfTable1C, quote=FALSE)
    
### Table 1D. White, Black, Asian, Hispanic Components of U.S. Tech Sector in 2014
    dfTable1Dtech <- dfRaceSexCountAndShares[1,2:7]
    colnames(dfTable1Dtech) <- colNames
    dfTable1Dtech <- prettyNum(dfTable1Dtech, big.mark = ",") 
    ### print(dfTable1Dtech, quote=FALSE)

### Percentages of total, white, black, asian, hispanic, and OTHERS
    dfTable1Dper <- dfRaceSexCountAndShares[1,c(2,11:15)]
    dfTable1Dper[1,1] <- 100 ### 100 percent for ALL
    colNames_per <- c("ALL", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS")
    colnames(dfTable1Dper) <- colNames_per
    dfTable1Dper <- as.character(round(dfTable1Dper, digits=1))
    #### dfTable1Dper
    
    dfTable1D <- data.frame(rbind(dfTable1Dtech,dfTable1Dper))
    rownames(dfTable1D) <- c("Num", "Per")
    print(dfTable1D, quote=FALSE)

### Table 1E. Foreign professionals in U.S. Tech
    totForeignTech <-dfForeignRaceSexCountAndShares["ALL STATES", "Totals"]
    totAsianTech <- dfForeignRaceSexCountAndShares["ALL STATES", "Asian"]
    totNonAsianTech <- totForeignTech - totAsianTech
    
    perAsianTech <- round(100 * totAsianTech/totForeignTech, digits=1)
    perNonAsianTech <- round(100 * totNonAsianTech/totForeignTech, digits=1)
    perTotForeignTech <- 100.0
    
    df1 <- data.frame(totForeignTech, totAsianTech, totNonAsianTech)
    df1 <- prettyNum(df1, big.mark = ",") 
    df2 <- as.character(data.frame(perTotForeignTech, perAsianTech, perNonAsianTech))
    dfTable1E <- rbind(df1, df2)
    
    colnames(dfTable1E) <- c("Foreign", "Asian", "Non-Asian")
    rownames(dfTable1E) <- c("Num", "Per")
    print(dfTable1E, quote=FALSE)   
   
### Table 2A. Female Components of U.S. U.S. Population  in 2014
    (dfTable2Apop <- dfStatesPop3[1,c(2,8:10)])
    dfTable2Apop$Male <- dfTable2Apop$Totals - dfTable2Apop$Female
    dfTable2Apop <- dfTable2Apop[, c(1, 5, 2:4)]
    
    colNames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
    colnames(dfTable2Apop) <- colNames
    dfTable2Apop <- prettyNum(dfTable2Apop, big.mark = ",")
    ### dfTable2Apop
    
    dfTable2Aper <- dfStatesPop3[1,c(2,16:18)]
    dfTable2Aper[1,1] <- 100.0 ### 100 percent for ALL
    dfTable2Aper$perMale <- round(100 - dfTable2Aper$perFemale, digits=1)
    dfTable2Aper <- dfTable2Aper[, c(1,5, 2:4)]
    
    colnames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
    colnames(dfTable2Aper) <- colNames
    dfTable2A <- data.frame(rbind(dfTable2Apop, dfTable2Aper))
    rownames(dfTable2A) <- c("Num", "Per")
    dfTable2A
  
### Table 2B. Male/Female Components of of U.S. Tech Sector in 2014
    dfTable2Bpop <- dfRaceSexCountAndShares[1,c(2,8:10)]
    dfTable2Bpop$Male <- dfTable2Bpop$Totals - dfTable2Bpop$Female
    dfTable2Bpop <- dfTable2Bpop[,c(1, 5, 2:4)] ### place males col 2
    
    colnames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
    colnames(dfTable2Bpop) <- colnames
    dfTable2Bpop <- prettyNum(dfTable2Bpop, big.mark = ",") 

### Percentages of Male/Female 
    dfTable2Bper <- dfRaceSexCountAndShares[1,c(2,16:18)]
    dfTable2Bper$perMale <- 100 - as.numeric(dfTable2Bper$perFemale)
    dfTable2Bper <- dfTable2Bper[, c(1,5,2:4)] ### males in col 2
    dfTable2Bper[1,1] <- 100 ### 100 percent for ALL
    
    colNames_per <- colnames
    colnames(dfTable2Bper) <- colNames_per
    
    dfTable2B <- data.frame(rbind(dfTable2Bpop, dfTable2Bper))
    rownames(dfTable2B) <- c("Num", "Per")
    dfTable2B

###########################
############################
### Table 3Z -- Profile of all U.S. Techs 2015
    dfProfileZ <- createProfile(dfProfileCitizens, group="", state="")
    dfProfileZ.2010 <- createProfile(dfProfileCitizens.2010, group="", state="")
    
    dfTable3Z <- subset(dfProfileZ, select=-c(Male, perMale))
    colnames(dfTable3Z) <- c("Occupation", "perTS", "Tech15", "Fem", "per15")
    head(dfTable3Z)
    
    ### Table 3ZZ -- Compare all U.S. Techs 2010 to all 2015
    dfTable3ZZ <- createCompareProfile(dfProfileZ.2010, dfProfileZ)
    colnames(dfTable3ZZ) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "PerF10")
    dfTable3ZZ

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
save(dfTable1, dfTable1A, dfTable1B, dfTable1C, dfTable1D, dfTable1E, dfTable2A, dfTable2B, dfTable3Z, dfTable3ZZ, dfTable3A, dfTable3AA, dfTable3B, dfTable3BB, dfTable3C, dfTable3CC, dfTable3D, dfTable3DD, dfTable3E, dfTable3EE, dfTable3F, dfTable3FF,  dfTable3FL, dfTable3FLFL, file="dfTab1A1B2A2B3ABCDEF.rda")
