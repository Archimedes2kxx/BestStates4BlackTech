### Stat-2A Demographic tables and profiles ... Tables 1, 2, and 3
### Generate

################################################
################################################ THurs 9/29 @ 5"12 pm
### Use map_dat() in ggplot2 as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################

load(file="dfRaceSexCountAndShares.rda")
load(file="dfStatesPop3.rda") 
load("dfCensus3.rda")
load("dfCensus2.rda")
load("dfCensus2.2010.rda")
load("dfCensus3.2010.rda")

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
### Table 1A. White, Black, Asian, Hispanic Components of U.S. Population in 2014
dfTable1Apop <- dfStatesPop3[1,2:7]
colNames <- c("ALL", "White", "Black", "Asian", "Hispanic", "OTHERS")
colnames(dfTable1Apop) <- colNames
dfTable1Apop <- prettyNum(dfTable1Apop, big.mark = ",") 
### dfTable1Apop

dfTable1Aper <- dfStatesPop3[1,c(2,11:15)]
dfTable1Aper[1,1] <- 100 ### 100 percent for ALL
colNames_per <- c("ALL", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS")
colnames(dfTable1Aper) <- colNames_per
dfTable1Aper <- as.character(round(dfTable1Aper, digits=1))
### dfTable1Aper

dfTable1A <- data.frame(rbind(dfTable1Apop, dfTable1Aper))
rownames(dfTable1A) <- c("Num", "Per")
dfTable1A

### Table 1B. White, Black, Asian, Hispanic Components of U.S. Tech Sector in 2014
dfTable1Btech <- dfRaceSexCountAndShares[1,2:7]
colnames(dfTable1Btech) <- colNames
dfTable1Btech <- prettyNum(dfTable1Btech, big.mark = ",") 
### dfTable1Btech

### Percentages of total, white, black, asian, hispanic, and OTHERS
dfTable1Bper <- dfRaceSexCountAndShares[1,c(2,11:15)]
dfTable1Bper[1,1] <- 100 ### 100 percent for ALL
colNames_per <- c("ALL", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS")
colnames(dfTable1Bper) <- colNames_per
dfTable1Bper <- as.character(round(dfTable1Bper, digits=1))
### dfTable1Bper

dfTable1B <- data.frame(rbind(dfTable1Btech, dfTable1Bper))
rownames(dfTable1B) <- c("Num", "Per")
dfTable1B


### Table 2A. Female Components of U.S. U.S. Population  in 2014
(dfTable2Apop <- dfStatesPop3[1,c(2,8:10)])
dfTable2Apop$Male <- dfTable2Apop$TotPop - dfTable2Apop$Female
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

######################
### Table 3ABC Occupations by Sex ... more thanks to HW
census3OccSex <- group_by(dfCensus3, Occupation, Sex)
head(census3OccSex)
dfPtsPwtOccSex <- summarise(census3OccSex, ptsPwtOccSex = sum(personalWeight))
head(dfPtsPwtOccSex)
dfOccupationSex <- spread(dfPtsPwtOccSex, key=Sex, value=ptsPwtOccSex)
head(dfOccupationSex)
dfOccupationSex$Total <- dfOccupationSex$Male + dfOccupationSex$Female
dfOccupationSex$perMale <- round((100 * dfOccupationSex$Male) /(dfOccupationSex$Total), digits=1)
dfOccupationSex$perFemale <- round((100 * dfOccupationSex$Female) /(dfOccupationSex$Total), digits=1)
### head(dfOccupationSex)


dfOccupationSex <- dfOccupationSex[, c(1,4,2:3, 5:6)] ### Put total in second column
colnames(dfOccupationSex) <- c("Occupation", "Tech2015", "Male","Female","perMale", "perFemale")
dfOccupationSex <- as.data.frame(dfOccupationSex)
### dfOccupationSex

### Add total row for ALL
techSums <- as.vector(colSums(dfOccupationSex[2:4]))
perMaleTechSums <- as.numeric(round((100 * techSums[2]/techSums[1]), digits = 1))
perFemaleTechSums <- as.numeric(round((100 * techSums[3]/techSums[1]), digits = 1))

dfALL <- data.frame("ALL", t(techSums), perMaleTechSums, perFemaleTechSums) ### note the transpose "t"
colnames(dfALL) <- c("Occupation", "Tech2015", "Male","Female", "perMale", "perFemale")
### dfALL
dfOccupationSex$Occupation <- as.character(dfOccupationSex$Occupation)
### str(dfOccupationSex)
dfOccupationSex <- rbind(dfOccupationSex, dfALL)
### dfOccupationSex

### Order by decreasing occupation
index <- order(dfOccupationSex$Tech2015, decreasing=TRUE)
dfOccupationSex <- dfOccupationSex[index,] 
rownames(dfOccupationSex) <- NULL
dfOccupationSex$`T_%` <- round(100*dfOccupationSex$Tech2015/dfOccupationSex[1,"Tech2015"], digits=1)
dfOccupationSex <- dfOccupationSex[,c(1,2,7, 3, 5, 4, 6)]
colnames(dfOccupationSex) <- c("Occupation", "Tech2015", "TS_%", "Male", "M_%", "Female", "F15_%")
(dfTable3ABC <- dfOccupationSex)

##############################
##############################
### Evil twin from 2010
### Like Table 3ABC Occupations by Sex ... more thanks to HW
census3OccSex.2010 <- group_by(dfCensus3.2010, Occupation, Sex)
### head(census3OccSex.2010)
dfPtsPwtOccSex.2010 <- summarise(census3OccSex.2010, ptsPwtOccSex.2010 = sum(personalWeight))
### head(dfPtsPwtOccSex.2010)
dfOccupationSex.2010 <- spread(dfPtsPwtOccSex.2010, key=Sex, value=ptsPwtOccSex.2010)
### head(dfOccupationSex.2010)
dfOccupationSex.2010$Total <- dfOccupationSex.2010$Male + dfOccupationSex.2010$Female
###dfOccupationSex$perMale <- round((100 * dfOccupationSex$Male) /(dfOccupationSex$Male + dfOccupationSex$Female), digits=1)
dfOccupationSex.2010$perFemale <- round((100 * dfOccupationSex.2010$Female) /(dfOccupationSex.2010$Total), digits=1)
### head(dfOccupationSex.2010)

dfOccupationSex.2010 <- dfOccupationSex.2010[, c(1,4, 3, 5)] ### Put total in second column
colnames(dfOccupationSex.2010) <- c("Occupation", "Tech2010", "Female", "perFemale")
dfOccupationSex.2010 <- as.data.frame(dfOccupationSex.2010)
### dfOccupationSex.2010

### Add total row for ALL
techSums.2010 <- as.vector(colSums(dfOccupationSex.2010[2:3]))
###perMaleTechSums.2010 <- as.numeric(round((100 * techSums[2]/techSums[1]), digits = 1))
perFemaleTechSums.2010 <- as.numeric(round((100 * techSums.2010[2]/techSums.2010[1]), digits = 1))

dfALL.2010 <- data.frame("ALL", t(techSums.2010), perFemaleTechSums.2010) ### note the transpose "t"
colnames(dfALL.2010) <- c("Occupation", "Tech2010", "Female", "perFemale")

dfOccupationSex.2010$Occupation <- as.character(dfOccupationSex.2010$Occupation)
dfOccupationSex.2010 <- rbind(dfOccupationSex.2010, dfALL.2010)
### dfOccupationSex.2010

### Order by decreasing occupation
index <- order(dfOccupationSex.2010$Tech2010, decreasing=TRUE)
dfOccupationSex.2010 <- dfOccupationSex.2010[index,] 
rownames(dfOccupationSex.2010) <- NULL
###dfOccupationSex.2010$`T_%` <- round(100*dfOccupationSex$Tech2010/dfOccupationSex[1,"Tech2010"], digits=1)
dfOccupationSex.2010 <- dfOccupationSex.2010[,c(1,2, 4)]
colnames(dfOccupationSex.2010) <- c("Occupation", "Tech2010", "F2010_%")
### dfOccupationSex.2010

### Tables 3D  ... compare with 2010 with 2015 American
dfTable3D <- dfOccupationSex.2010
### Order by occupations
index <- order(dfTable3D$Occupation, decreasing=TRUE)
dfTable3D <- dfTable3D[index,]
dfTable3ABCcopy <- dfTable3ABC
index <- order(dfTable3ABC$Occupation, decreasing=TRUE)
dfTable3ABCcopy <- dfTable3ABCcopy[index,]
dfTable3D$Tech2015 <- dfTable3ABCcopy$Tech2015
dfTable3D$Change <- dfTable3D$Tech2015 - dfTable3D$Tech2010
dfTable3D$perChange <- round(100 * dfTable3D$Change / dfTable3D$Tech2010, digits=1)
index <- order(dfTable3D$Tech2015, decreasing=TRUE)
dfTable3D <- dfTable3D[index,]
colnames(dfTable3D) <- c("Occupation", "Tech10", "F10_%", "Tech15", "Change", "Ch_%")
(dfTable3D <- dfTable3D[,c(1,2,4,5,6,3)])


############################################
############################
### Table 3E . Occupations of foreign techs from Asia and elsewhere
dfCensus5 <- subset(dfCensus2, select=c("personalWeight", "Sex", "Occupation", "Birth"), Citizen==FALSE) 
str(dfCensus5) ### 4803 obs. of  3 variables
dfCensus5$Occupation <- as.character(dfCensus5$Occupation)

### WAOB = World Area of Birth = continent on which person was born
census5OccupationBirth <- group_by(dfCensus5, Occupation, Birth)
dfSumPwtOccupationBirth <- summarise(census5OccupationBirth, ptsPwtOccupationBirth = sum(personalWeight))
dfBirthPerOccupation <- spread(dfSumPwtOccupationBirth, key=Birth, value=ptsPwtOccupationBirth, fill=0, drop=FALSE)
indexBirth <- order(dfBirthPerOccupation$Occupation, decreasing=FALSE)
dfBirthPerOccupation <- dfBirthPerOccupation[indexBirth,]
dfBirthPerOccupation

### Add Sex
census5OccupationSex <- group_by(dfCensus5, Occupation, Sex)
dfSumPwtOccupationSex <- summarise(census5OccupationSex, ptsPwtOccupationSex = sum(personalWeight))
dfSexPerOccupation <- spread(dfSumPwtOccupationSex, key=Sex, value=ptsPwtOccupationSex, fill=0, drop=FALSE)
indexSex <- order(dfSexPerOccupation$Occupation, decreasing=FALSE)
dfSexPerOccupation <- dfSexPerOccupation[indexSex,]
head(dfSexPerOccupation)

### Combine all Non-Asia into Others dfBirthPerOccupation.2010$LatinAmerica + 
dfBirthPerOccupation$NotAsia <- dfBirthPerOccupation$"Latin America" + dfBirthPerOccupation$Europe + dfBirthPerOccupation$Africa + dfBirthPerOccupation$"North America" + dfBirthPerOccupation$Oceania 
dfForeignTechOccupations <- subset(dfBirthPerOccupation, select=-c(USA, `US Islands`, `Latin America`, Europe, Africa, `North America`, Oceania)) ### Delete the components of FrnOthers

dfForeignTechOccupations <- merge(dfForeignTechOccupations, dfSexPerOccupation) ### Occupations in same order
dfForeignTechOccupations

allForeign <- colSums(dfForeignTechOccupations[,2:5])
dfForeignTop <- dfForeignTechOccupations[1,] ### dummy copy first row to set the types
dfForeignTop$Occupation <- "ALL"
dfForeignTop[1,2:5] <- allForeign
dfForeignTechOccupations <- rbind(dfForeignTop, dfForeignTechOccupations)
dfForeignTechOccupations <- as.data.frame(dfForeignTechOccupations)

dfForeignTechOccupations$`AS_%` <- round(100*dfForeignTechOccupations$Asia/dfForeignTechOccupations[1,"Asia"], digits=1)
dfForeignTechOccupations$`NAS_%` <- round(100*dfForeignTechOccupations$NotAsia/dfForeignTechOccupations[1,"NotAsia"], digits=1)
dfForeignTechOccupations$Foreign <- dfForeignTechOccupations$Asia + dfForeignTechOccupations$NotAsia
dfForeignTechOccupations$`F15_%` <- round(100*dfForeignTechOccupations$Female/dfForeignTechOccupations$Foreign, digits=1)
dfForeignTechOccupations <- subset(dfForeignTechOccupations, select=-c(Male, Female))
dfForeignTechOccupations

dfTable3E <- dfForeignTechOccupations[, c(1, 6, 2, 4, 3, 5, 7)]
colnames(dfTable3E) <- c("Occupation", "Foreign", "Asia", "AS_%", "NAsia", "NAS_%", "F15_%")
index <- order(dfTable3E$Foreign, decreasing=TRUE)
(dfTable3E <- dfTable3E[index,])

#################################
### Tables 3F and 3G ... compare 2010 to 2015 for Asia (3F)   and Non-Asian (3G)
dfCensus5.2010 <- subset(dfCensus2.2010, select=c("personalWeight","Occupation", "Birth"), Citizen==FALSE) 
### str(dfCensus5.2010) ### 3687 obs. of  3 variables:

census5.2010OccupationBirth <- group_by(dfCensus5.2010, Occupation, Birth)
dfSumPwtOccupationBirth.2010 <- summarise(census5.2010OccupationBirth, ptsPwtOccupationBirth.2010 = sum(personalWeight))
dfBirthPerOccupation.2010 <- spread(dfSumPwtOccupationBirth.2010, key=Birth, value=ptsPwtOccupationBirth.2010, fill=0, drop=FALSE)
### head(dfBirthPerOccupation.2010)

### Combine all Non-Asia into Others dfBirthPerOccupation.2010$LatinAmerica + 
dfBirthPerOccupation.2010$NAsia <- dfBirthPerOccupation.2010$"Latin America" + dfBirthPerOccupation.2010$Europe + dfBirthPerOccupation.2010$Africa + dfBirthPerOccupation.2010$"North America" + dfBirthPerOccupation.2010$Oceania 

dfForeignTechOccupations.2010 <- subset(dfBirthPerOccupation.2010, select=-c(USA, `US Islands`, `Latin America`, Europe, Africa, `North America`, Oceania)) ### Delete the components of FrnOthers
### dfForeignTechOccupations.2010

allForeign.2010 <- colSums(dfForeignTechOccupations.2010[,2:3])
dfForeignTop.2010 <- dfForeignTechOccupations.2010[1,] ### dummy copy first row to set the types
dfForeignTop.2010$Occupation <- "ALL"
dfForeignTop.2010[1,2:3] <- allForeign.2010
dfForeignTechOccupations.2010 <- rbind(dfForeignTop.2010, dfForeignTechOccupations.2010)
dfForeignTechOccupations.2010 <- as.data.frame(dfForeignTechOccupations.2010)
### dfForeignTechOccupations.2010

dfForeignTechOccupations.2010$`AS_%` <- round(100*dfForeignTechOccupations.2010$Asia/dfForeignTechOccupations.2010[1,"Asia"], digits=1)
dfForeignTechOccupations.2010$`NAS_%` <- round(100*dfForeignTechOccupations.2010$NAsia/dfForeignTechOccupations.2010[1,"NAsia"], digits=1)
### dfForeignTechOccupations.2010

dfTable3FF <- subset(dfForeignTechOccupations.2010, select=c(Occupation, Asia, `AS_%`))
###index <- order(dfForeignTechOccupations.2010$Asia, decreasing=TRUE)
###(dfTable3FF <- dfTable3FF[index,])

dfTable3GG <- subset(dfForeignTechOccupations.2010, select=c(Occupation, NAsia, `NAS_%`))
###index <- order(dfForeignTechOccupations.2010$NAsia, decreasing=TRUE)
###(dfTable3GG <- dfTable3GG[index,])

### Tables 3F Change in Foreign Asian
dfTable3F <- dfTable3E ### set up dimensions and some cols and order of rows
dfTable3F <- dfTable3F[order(dfTable3F$Occupation),]
dfTable3F <- subset(dfTable3F, select=-c(Foreign, NAsia, `F15_%`))
dfTable3F$`AS_%`<- NULL
dfTable3F$`NAS_%` <- NULL
dfTable3FF <- dfTable3FF[order(dfTable3FF$Occupation),]
dfTable3F$Asia2010 <- dfTable3FF$Asia
dfTable3F$Change <- dfTable3F$Asia - dfTable3F$Asia2010
dfTable3F$Per <- round((100 * dfTable3F$Change / dfTable3F$Asia2010), digits=1)
dfTable3F <- dfTable3F[,c(1,3,2,4,5)] ### put 2010 before 2015
dfTable3F <- dfTable3F[order(dfTable3F$Asia, decreasing = TRUE),]
colnames(dfTable3F) <- c("Occupation", "As2010", "As2015", "Change", "Ch_%")
dfTable3F

############################
### Tables 3G Change in Foreign NotAsian
dfTable3G <- dfTable3E ### set up dimensions and some cols and order of rows
dfTable3G <- dfTable3G[order(dfTable3G$Occupation),]
dfTable3G <- subset(dfTable3G, select=-c(Foreign, Asia, `F15_%`))
dfTable3G$`AS_%`<- NULL
dfTable3G$`NAS_%` <- NULL
dfTable3GG <- dfTable3GG[order(dfTable3GG$Occupation),]
dfTable3G$NAsia2010 <- dfTable3GG$NAsia
dfTable3G$Change <- dfTable3G$NAsia - dfTable3G$NAsia2010
dfTable3G$Per <- round((100 * dfTable3G$Change / dfTable3G$NAsia2010), digits=1)
dfTable3G <- dfTable3G[,c(1,3,2,4,5)] ### put 2010 before 2015
dfTable3G <- dfTable3G[order(dfTable3G$NAsia, decreasing = TRUE),]
colnames(dfTable3G) <- c("Occupation", "NAs2010", "NAs2015", "Change", "Ch_%")
dfTable3G

save(dfTable1A, dfTable1B, dfTable2A, dfTable2B, dfTable3ABC, dfTable3D, dfTable3E, dfTable3F, dfTable3G, file="dfTab1A1B2A2B3ABCDEFGH.rda")
