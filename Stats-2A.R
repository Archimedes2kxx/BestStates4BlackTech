### Stat-2A Demographic tables and profiles ... Tables 1, 2, and 3

### Add charts to be included in the Overview of the report

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
    
    library(tidyr)
    library(ggplot2)
    library(scales)
    library(dplyr)

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

#####################################      
### Table 1C U.S. Tech Sector
    totCitTech <- as.numeric(dfRaceSexCountAndShares["ALL STATES", "Totals"])
    totForTech <- as.numeric(dfForeignRaceSexCountAndShares["ALL STATES", "Totals"])
    rowDf <- data.frame(Citizens=totCitTech, Foreign=totForTech)
    
    dfTable1C<- makeNumPerTable(rowDf)
    print(dfTable1C, quote=FALSE)  
    
### Bar Chart 1 showing Total, Citizens, and Foreign Techs
    chartTitle1 <- "Chart 1. Techs in U.S. Workforce in 2015"
    ggChart1 <- makeNumPerChart(dfTable1C[,-1], chartTitle1, font=3)
    ggChart1
    ggsave("ggChart1.png", width=4, height=2)

######################      
### Table 1D. White, Black, Asian, Hispanic Components of U.S. Tech in 2015
    rowDf <- dfRaceSexCountAndShares[1,2:7]
    colnames(rowDf) <- c("Total", "White", "Black", "Asian", "Hispanic", "OTHERS")
    Total <- rowDf[1,1]
    
    dfTable1D<- makeNumPerTable(rowDf, Total)
    print(dfTable1D, quote=FALSE)    

### Chart 2 showing groups in tech
    chartTitle2 <- "Chart 2. American Groups in Tech"
    ggChart2 <- makeNumPerChart(dfTable1D[,-1], chartTitle2, font=2)
    ggChart2
    ggsave("ggChart2.png", width=4, height=2)   
    
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
    
### Chart 3 showing male/female compnents
    chartTitle3 <- "Chart 3. American Male/Female Tech"
    ggChart3 <- makeNumPerChart(dfTable2B[,-1], chartTitle3, font=2)
    ggChart3
    ggsave("ggChart3.png", width=4, height=2)      
    
###########################
############################
### Initialize raw profile data frames
    dfCit1 <- dfProfileCitizens.2010
    dfCit2 <- dfProfileCitizens
    
    dfCitS1 <- dfProfileCitizens_RawState.2010
    dfCitS2 <- dfProfileCitizens_RawState
    
    dfCitR1 <- dfProfileCitizens_RawRace.2010
    dfCitR2 <- dfProfileCitizens_RawRace

    dfCitRS1 <- dfProfileCitizens_RawRaceState.2010
    dfCitRS2 <- dfProfileCitizens_RawRaceState
    
    dfFor1 <- dfProfileForeigners.2010
    dfFor2 <- dfProfileForeigners  
    
    dfForS1 <- dfProfileForeigners_RawState.2010
    dfForS2 <- dfProfileForeigners_RawState 
    
    dfForR1 <- dfProfileForeigners_RawRace.2010
    dfForR2 <- dfProfileForeigners_RawRace 
    
    dfForRS1 <- dfProfileForeigners_RawRaceState.2010
    dfForRS2 <- dfProfileForeigners_RawRaceState 
    ### colSums(dfForRS2[,4:5]) ### male, female = 455883 113831 which is correct
    ### colSums(dfForRS1[,4:5]) ### male, female = 336760  92537 which is correct

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
### Table 3C -- Profile of U.S. Asian Techs 2015
    
    Profiles <- createListProfiles(dfCitR1, dfCitR2, group="Asian") 
    (dfTable3C <- Profiles[[1]])
    (dfTable3CC <- Profiles[[2]])

####################################
####################################    
    ### Table 3D -- Profile of U.S. Hispanic Techs 2015
    
    Profiles <- createListProfiles(dfCitR1, dfCitR2, group="Hispanic") 
    (dfTable3D <- Profiles[[1]])
    (dfTable3DD <- Profiles[[2]])    
    
####################################
####################################
    
### Table 3E -- Profile of Foreign Techs 2015
    Profiles <- createListProfiles(dfFor1, dfFor2) 
    (dfTable3E <- Profiles[[1]])
    (dfTable3EE <- Profiles[[2]]) 
    
    
    ### Chart 4. Growth in Tech, 2010 to 2015
    listDfs <- list(White=dfTable3AA, Black=dfTable3BB, Hispanic=dfTable3CC, Asian=dfTable3DD, Foreign=dfTable3EE)
    ggDfChart4 <- createXYZdf("Tech", "10", "15", listdDfs)
    ###ggDfChart4
    chartTitle4 <- "Chart 4. Growth in Tech, 2010 to 2015"
    ggChart4 <- makeGroupedBarChart(ggDfChart4, chartTitle4)
    ggChart4
    ggsave("ggChart4.png", width=4, height=2)    
    
###############################################
###############################################    
### Table 3F -- Profile of Foreign from Asia in 2015
    
    Profiles <- createListProfiles(dfForR1, dfForR2, group="Asian") 
    (dfTable3F <- Profiles[[1]]) 
    (dfTable3FF <- Profiles[[2]]) 
    
###############################################
############################################### 
### APPENDIX 
    
### Table3CA -- Profile of Techs in California in 2015
    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="California") 
    (dfTable3CA <- Profiles[[1]])
    (dfTable3CACA <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="California", group="Black") 
    (dfTable3CAblack <- Profiles[[1]])
    (dfTable3CACAblack <- Profiles[[2]])  

    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="California", group="Hispanic") 
    (dfTable3CAhispanic <- Profiles[[1]])
    (dfTable3CACAhispanic <- Profiles[[2]])   
    
### Texas 2015 = 2190 foreign techs???  ... too small
### Tennessee 2015 = 52885 foreign techs??? ... too large
### New Jersey 2015 = 950 foreign techs ??? ... too small   
### North Dakota 2015 = 11788  foreign techs ???  ... too large 
### Washington 2015 = 569  foreign techs ??? ... too small
### New Hampshire 2015 = 31859  foreign techs ??? ... too large
### North Carolina 2015 = 77  foreign techs ??? ... too small
### Vermont 2015 = 16526  foreign techs ??? ... too large
### Utah  282 ... too small
### Nebraska 2279 is wrong ... from Nebraska to Wyoming (last) is wrong
### Missouri 6655 is OK ... from (Alabama first) to Missouri is right
    
   
### Table 3forCA -- Profile of Foreign in California  in 2015
    Profiles <- createListProfiles(dfForS1, dfForS2, state="California") 
    (dfTable3forCA <- Profiles[[1]]) 
    (dfTable3forCACA <- Profiles[[2]]) 
    
### Table 3.forAsian -- Profile of Foreign Asian in California  in 2015
    Profiles <- createListProfiles(dfForRS1, dfForRS2, state="California", group="Asian") 
    (dfTable3forAsianCA <- Profiles[[1]]) 
    (dfTable3forAsianCACA <- Profiles[[2]]) 
    
####################################
####################################
### Table 3TX -- Profile of Techs in Texas in 2015

    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="Texas") 
    (dfTable3TX <- Profiles[[1]])
    (dfTable3TXTX <- Profiles[[2]])    
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Texas", group="Black") 
    (dfTable3TXblack <- Profiles[[1]])
    (dfTable3TXTXblack <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Texas", group="Hispanic") 
    (dfTable3TXhispanic <- Profiles[[1]])
    (dfTable3TXTXhispanic <- Profiles[[2]])        
    
### Table 3.forTX -- Profile of Foreign in California  in 2015
    Profiles <- createListProfiles(dfForS1, dfForS2, state="Texas") 
    (dfTable3forTX <- Profiles[[1]]) 
    (dfTable3forTXTX <- Profiles[[2]]) 
    
### Table 3.forAsianTX -- Profile of Foreign Asian in California  in 2015
    Profiles <- createListProfiles(dfForRS1, dfForRS2, state="Texas", group="Asian") 
    (dfTable3forAsianTX <- Profiles[[1]]) 
    (dfTable3forAsianTXTX <- Profiles[[2]]) 
    
    
####################################
####################################
### Table 3NY -- Profile of Techs in New York in 2015

    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="New York") 
    (dfTable3NY <- Profiles[[1]])
    (dfTable3NYNY <- Profiles[[2]])    
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="New York", group="Black") 
    (dfTable3NYblack <- Profiles[[1]])
    (dfTable3NYNYblack <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="New York", group="Hispanic") 
    (dfTable3NYhispanic <- Profiles[[1]])
    (dfTable3NYNYhispanic <- Profiles[[2]])         
    
### Table 3forNY -- Profile of Foreign in New York  in 2015
    Profiles <- createListProfiles(dfForS1, dfForS2, state="New York") 
    (dfTable3forNY <- Profiles[[1]]) 
    (dfTable3forNYNY <- Profiles[[2]]) 
    
### Table 3.forAsian -- Profile of Foreign Asian in California  in 2015
    Profiles <- createListProfiles(dfForRS1, dfForRS2, state="New York", group="Asian") 
    (dfTable3forAsianNY <- Profiles[[1]]) 
    (dfTable3forAsianNYNY <- Profiles[[2]]) 
    
####################################
####################################
### Table 3FL -- Profile of Techs in Florida in 2015
    
    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="Florida") 
    (dfTable3FL <- Profiles[[1]])
    (dfTable3FLFL <- Profiles[[2]])    
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Florida", group="Black") 
    (dfTable3FLblack <- Profiles[[1]])
    (dfTable3FLFLblack <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Florida", group="Hispanic") 
    (dfTable3FLhispanic <- Profiles[[1]])
    (dfTable3FLFLhispanic <- Profiles[[2]])      
    
### Table 3forFL -- Profile of Foreign in California  in 2015
    Profiles <- createListProfiles(dfForS1, dfForS2, state="Florida") 
    (dfTable3forFL <- Profiles[[1]]) 
    (dfTable3forFLFL <- Profiles[[2]]) 
    
### Table 3.forAsian -- Profile of Foreign Asian in California  in 2015
    Profiles <- createListProfiles(dfForRS1, dfForRS2, state="Florida", group="Asian") 
    (dfTable3forAsianFL <- Profiles[[1]]) 
    (dfTable3forAsianFLFL <- Profiles[[2]]) 
    
####################################
####################################   
### Table 3VA -- Profile of Techs in Virginia in 2015
    
    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="Virginia") 
    (dfTable3VA <- Profiles[[1]])
    (dfTable3VAVA <- Profiles[[2]])    
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Virginia", group="Black") 
    (dfTable3VAblack <- Profiles[[1]])
    (dfTable3VAVAblack <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Virginia", group="Hispanic") 
    (dfTable3VAhispanic <- Profiles[[1]])
    (dfTable3VAVAhispanic <- Profiles[[2]])  
    
### Table 3forVA -- Profile of Foreign in Virginia  in 2015
    Profiles <- createListProfiles(dfForS1, dfForS2, state="Virginia") 
    (dfTable3forVA <- Profiles[[1]]) 
    (dfTable3forVAVA <- Profiles[[2]]) 
    
### Table 3.forAsian -- Profile of Foreign Asian in Virginia  in 2015
    Profiles <- createListProfiles(dfForRS1, dfForRS2, state="Virginia", group="Asian") 
    (dfTable3forAsianVA <- Profiles[[1]]) 
    (dfTable3forAsianVAVA <- Profiles[[2]])

########################################
########################################
### Table 3IL -- Profile of Techs in Illinois in 2015
    
    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="Illinois") 
    (dfTable3IL <- Profiles[[1]])
    (dfTable3ILIL <- Profiles[[2]])    
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Illinois", group="Black") 
    (dfTable3ILblack <- Profiles[[1]])
    (dfTable3ILILblack <- Profiles[[2]])  
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Illinois", group="Hispanic") 
    (dfTable3ILhispanic <- Profiles[[1]])
    (dfTable3ILILhispanic <- Profiles[[2]])  
 
### Table 3forIL -- Profile of Foreign in Illinois  in 2015
    Profiles <- createListProfiles(dfForS1, dfForS2, state="Illinois") 
    (dfTable3forIL <- Profiles[[1]]) 
    (dfTable3forILIL <- Profiles[[2]]) 
    
### Table 3.forAsian -- Profile of Foreign Asian in Illinois  in 2015
    Profiles <- createListProfiles(dfForRS1, dfForRS2, state="Illinois", group="Asian") 
    (dfTable3forAsianIL <- Profiles[[1]]) 
    (dfTable3forAsianILIL <- Profiles[[2]])  
    
########################################
########################################
### Table 3DC -- Profile of Techs in District of Columbia in 2015
    
    Profiles <- createListProfiles(dfCitS1, dfCitS2, state="Dist of Col") 
    (dfTable3DC <- Profiles[[1]])
    (dfTable3DCDC <- Profiles[[2]])    
    
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Dist of Col", group="Black") 
    (dfTable3DCblack <- Profiles[[1]])
    (dfTable3DCDCblack <- Profiles[[2]])  
      
    Profiles <- createListProfiles(dfCitRS1, dfCitRS2, state="Dist of Col", group="Hispanic") 
    (dfTable3DChispanic <- Profiles[[1]])
    (dfTable3DCDChispanic <- Profiles[[2]])  

########################################
########################################  
    ### Chart 5. Growth in Foreign Tech, 2010 to 2015"
    listDfs <- list(CA=dfTable3forCACA, TX=dfTable3forTXTX, NY=dfTable3forNYNY, FL=dfTable3forFLFL, VA=dfTable3forVAVA, IL=dfTable3forILIL)
    ggDfChart5 <- createXYZdf("Tech", "10", "15", listdDfs)
    chartTitle5 <- "Chart 5. Foreign Tech by States, 2010 to 2015"
    ggChart5 <- makeGroupedBarChart(ggDfChart5, chartTitle5)
    (ggChart5 <- ggChart5 + theme(axis.title.x = element_blank()))
    ggsave("ggChart5.png", width=4, height=2)    
     
########################################
########################################     
save(dfTable1A, dfTable1B, dfTable1C, dfTable1D, dfTable1E, dfTable2A, dfTable2B, dfTable3Z, dfTable3ZZ, dfTable3A, dfTable3AA, dfTable3B, dfTable3BB, dfTable3C, dfTable3CC, dfTable3D, dfTable3DD, dfTable3E, dfTable3EE, dfTable3F, dfTable3FF, file="dfTab1A1B2A2B3ABCDEF.rda")

save(dfTable3CA, dfTable3CACA, dfTable3CAblack, dfTable3CACAblack, dfTable3CAhispanic, dfTable3CACAhispanic, dfTable3forCA, dfTable3forCACA, dfTable3forAsianCA, dfTable3forAsianCACA, dfTable3TX, dfTable3TXTX, dfTable3TXblack, dfTable3TXTXblack, dfTable3TXhispanic, dfTable3TXTXhispanic, dfTable3forTX, dfTable3forTXTX, dfTable3forAsianTX, dfTable3forAsianTXTX, dfTable3NY, dfTable3NYNY, dfTable3NYblack, dfTable3NYNYblack, dfTable3NYhispanic, dfTable3NYNYhispanic, dfTable3forNY, dfTable3forNYNY, dfTable3forAsianNY, dfTable3forAsianNYNY, dfTable3FL, dfTable3FLFL, dfTable3FLblack, dfTable3FLFLblack, dfTable3FLhispanic, dfTable3FLFLhispanic, dfTable3forFL, dfTable3forFLFL, dfTable3forAsianFL, dfTable3forAsianFLFL, dfTable3VA, dfTable3VAVA, dfTable3VAblack, dfTable3VAVAblack, dfTable3VAhispanic, dfTable3VAVAhispanic, dfTable3forVA, dfTable3forVAVA, dfTable3forAsianVA, dfTable3forAsianVAVA, dfTable3IL, dfTable3ILIL, dfTable3ILblack, dfTable3ILILblack, dfTable3ILhispanic, dfTable3ILILhispanic, dfTable3forIL, dfTable3forILIL, dfTable3forAsianIL, dfTable3forAsianILIL, dfTable3DC, dfTable3DCDC, dfTable3DCblack, dfTable3DCDCblack, dfTable3DChispanic, dfTable3DCDChispanic, file="APPENDIX.rda")

