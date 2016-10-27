### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

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
(dfTable1Apop <- dfStatesPop3[1,2:7])
colNames <- c("ALL", "White", "Black", "Asian", "Hispanic", "OTHERS")
colnames(dfTable1Apop) <- colNames
dfTable1Apop <- prettyNum(dfTable1Apop, big.mark = ",") 
dfTable1Apop

dfTable1Aper <- dfStatesPop3[1,c(2,11:15)]
dfTable1Aper[1,1] <- 100 ### 100 percent for ALL
colNames_per <- c("ALL", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS")
colnames(dfTable1Aper) <- colNames_per
dfTable1Aper <- as.character(round(dfTable1Aper, digits=1))
dfTable1Aper

dfTable1A <- data.frame(rbind(dfTable1Apop, dfTable1Aper))
rownames(dfTable1A) <- c("Number", "Percent")
dfTable1A

### Table 1B. White, Black, Asian, Hispanic Components of U.S. Tech Sector in 2014
dfTable1Btech <- dfRaceSexCountAndShares[1,2:7]
colnames(dfTable1Btech) <- colNames
dfTable1Btech <- prettyNum(dfTable1Btech, big.mark = ",") 
dfTable1Btech

### Percentages of total, white, black, asian, hispanic, and OTHERS
dfTable1Bper <- dfRaceSexCountAndShares[1,c(2,11:15)]
dfTable1Bper[1,1] <- 100 ### 100 percent for ALL
colNames_per <- c("ALL", "perWhite", "perBlack", "perAsian", "perHispanic", "perOTHERS")
colnames(dfTable1Bper) <- colNames_per
dfTable1Bper <- as.character(round(dfTable1Bper, digits=1))
dfTable1Bper

dfTable1B <- data.frame(rbind(dfTable1Btech, dfTable1Bper))
rownames(dfTable1B) <- c("Number", "Percent")
dfTable1B


### Table 2A. Female Components of U.S. U.S. Population  in 2014
(dfTable2Apop <- dfStatesPop3[1,c(2,8:10)])
dfTable2Apop$Male <- dfTable2Apop$TotPop - dfTable2Apop$Female
dfTable2Apop <- dfTable2Apop[, c(1, 5, 2:4)]
colNames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
colnames(dfTable2Apop) <- colNames
dfTable2Apop <- prettyNum(dfTable2Apop, big.mark = ",")
dfTable2Apop

(dfTable2Aper <- dfStatesPop3[1,c(2,16:18)])
dfTable2Aper[1,1] <- 100.0 ### 100 percent for ALL
dfTable2Aper$perMale <- round(100 - dfTable2Aper$perFemale, digits=1)
dfTable2Aper <- dfTable2Aper[, c(1,5, 2:4)]
colnames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
colnames(dfTable2Aper) <- colNames

dfTable2A <- data.frame(rbind(dfTable2Apop, dfTable2Aper))
rownames(dfTable2A) <- c("Number", "Percent")
dfTable2A


### Table 2B. Male/Female Components of of U.S. Tech Sector in 2014
(dfTable2Bpop <- dfRaceSexCountAndShares[1,c(2,8:10)])
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
rownames(dfTable2B) <- c("Number", "Percent")
dfTable2B

######################
### Table 3 Occupations by Sex ... more thanks to HW
census3OccSex <- group_by(dfCensus3, Occupation, Sex)
head(census3OccSex)
dfPtsPwtOccSex <- summarise(census3OccSex, ptsPwtOccSex = sum(personalWeight))
head(dfPtsPwtOccSex)
dfOccupationSex <- spread(dfPtsPwtOccSex, key=Sex, value=ptsPwtOccSex)
head(dfOccupationSex)
dfOccupationSex$perMale <- round((100 * dfOccupationSex$Male) /(dfOccupationSex$Male + dfOccupationSex$Female), digits=1)
dfOccupationSex$perFemale <- round((100 * dfOccupationSex$Female) /(dfOccupationSex$Male + dfOccupationSex$Female), digits=1)
head(dfOccupationSex)

dfOccupationSex$Total <- dfOccupationSex$Male + dfOccupationSex$Female
dfOccupationSex <- dfOccupationSex[, c(1,6,2:5)] ### Put total in second column
colnames(dfOccupationSex) <- c("Occupation", "TechStaff", "Male","Female","perMale", "perFemale")
dfOccupationSex <- as.data.frame(dfOccupationSex)
dfOccupationSex

### Add total row for ALL
techSums <- as.vector(colSums(dfOccupationSex[2:4]))
perMaleTechSums <- as.numeric(round((100 * techSums[2]/techSums[1]), digits = 1))
perFemaleTechSums <- as.numeric(round((100 * techSums[3]/techSums[1]), digits = 1))

dfALL <- data.frame("ALL", t(techSums), perMaleTechSums, perFemaleTechSums) ### note the transpose "t"
colnames(dfALL) <- c("Occupation", "TechStaff", "Male","Female", "perMale", "perFemale")
dfALL
dfOccupationSex$Occupation <- as.character(dfOccupationSex$Occupation)
str(dfOccupationSex)
dfOccupationSex <- rbind(dfOccupationSex, dfALL)
dfOccupationSex

### Order by decreasing occupation, drop Female, no row names, etc
index <- order(dfOccupationSex$TechStaff, decreasing=TRUE)
dfOccupationSex <- dfOccupationSex[index,] 
rownames(dfOccupationSex) <- NULL
dfOccupationSex$`T_%` <- round(100*dfOccupationSex$TechStaff/dfOccupationSex[1,"TechStaff"], digits=1)
dfOccupationSex <- dfOccupationSex[,c(1,2,7, 3, 5, 4, 6)]
colnames(dfOccupationSex) <- c("Occupation", "TechStaff", "TS_%", "Male", "M_%", "Female", "F_%")
(dfTable3ABC <- dfOccupationSex)

############################
### Table 3D and 3E. Occupations of foreign techs from Asia and elsewhere
dfCensus5 <- subset(dfCensus2, select=c("personalWeight","Occupation", "Birth"), Citizen==FALSE) 
str(dfCensus5) ### 4803 obs. of  3 variables

census5OccupationBirth <- group_by(dfCensus5, Occupation, Birth)
dfSumPwtOccupationBirth <- summarise(census5OccupationBirth, ptsPwtOccupationBirth = sum(personalWeight))
dfBirthPerOccupation <- spread(dfSumPwtOccupationBirth, key=Birth, value=ptsPwtOccupationBirth, fill=0, drop=FALSE)
head(dfBirthPerOccupation)

### Combine all Non-Asia into Others dfBirthPerOccupation.2010$LatinAmerica + 
dfBirthPerOccupation$NotAsia <- dfBirthPerOccupation$"Latin America" + dfBirthPerOccupation$Europe + dfBirthPerOccupation$Africa + dfBirthPerOccupation$"North America" + dfBirthPerOccupation$Oceania 

dfForeignTechOccupations <- subset(dfBirthPerOccupation, select=-c(USA, `US Islands`, `Latin America`, Europe, Africa, `North America`, Oceania)) ### Delete the components of FrnOthers
dfForeignTechOccupations

allForeign <- colSums(dfForeignTechOccupations[,2:3])
dfForeignTop <- dfForeignTechOccupations[1,] ### dummy copy first row to set the types
dfForeignTop$Occupation <- "ALL"
dfForeignTop[1,2:3] <- allForeign
dfForeignTechOccupations <- rbind(dfForeignTop, dfForeignTechOccupations)
dfForeignTechOccupations <- as.data.frame(dfForeignTechOccupations)

dfForeignTechOccupations$`AOS_%` <- round(100*dfForeignTechOccupations$Asia/dfForeignTechOccupations[1,"Asia"], digits=1)
dfForeignTechOccupations$`NAOS_%` <- round(100*dfForeignTechOccupations$NotAsia/dfForeignTechOccupations[1,"NotAsia"], digits=1)
dfForeignTechOccupations

dfTable3D <- subset(dfForeignTechOccupations, select=c(Occupation, Asia, `AOS_%`))
index <- order(dfForeignTechOccupations$Asia, decreasing=TRUE)
(dfTable3D <- dfTable3D[index,])

dfTable3E <- subset(dfForeignTechOccupations, select=c(Occupation, NotAsia, `NAOS_%`))
index <- order(dfForeignTechOccupations$NotAsia, decreasing=TRUE)
(dfTable3E <- dfTable3E[index,])

############################
### Evil Twin from 2010 time ... again


########################
#### Tables 3F  ... compare with 2010 with 2015 American


dfCensus5.2010 <- subset(dfCensus2.2010, select=c("personalWeight","Occupation", "Birth"), Citizen==FALSE) 
str(dfCensus5.2010) ### 3687 obs. of  3 variables:

census5.2010OccupationBirth <- group_by(dfCensus5.2010, Occupation, Birth)
dfSumPwtOccupationBirth.2010 <- summarise(census5.2010OccupationBirth, ptsPwtOccupationBirth.2010 = sum(personalWeight))
dfBirthPerOccupation.2010 <- spread(dfSumPwtOccupationBirth.2010, key=Birth, value=ptsPwtOccupationBirth.2010, fill=0, drop=FALSE)
head(dfBirthPerOccupation.2010)

### Combine all Non-Asia into Others dfBirthPerOccupation.2010$LatinAmerica + 
dfBirthPerOccupation.2010$NotAsia <- dfBirthPerOccupation.2010$"Latin America" + dfBirthPerOccupation.2010$Europe + dfBirthPerOccupation.2010$Africa + dfBirthPerOccupation.2010$"North America" + dfBirthPerOccupation.2010$Oceania 

dfForeignTechOccupations.2010 <- subset(dfBirthPerOccupation.2010, select=-c(USA, `US Islands`, `Latin America`, Europe, Africa, `North America`, Oceania)) ### Delete the components of FrnOthers
dfForeignTechOccupations.2010

allForeign.2010 <- colSums(dfForeignTechOccupations.2010[,2:3])
dfForeignTop.2010 <- dfForeignTechOccupations.2010[1,] ### dummy copy first row to set the types
dfForeignTop.2010$Occupation <- "ALL"
dfForeignTop.2010[1,2:3] <- allForeign.2010
dfForeignTechOccupations.2010 <- rbind(dfForeignTop.2010, dfForeignTechOccupations.2010)
dfForeignTechOccupations.2010 <- as.data.frame(dfForeignTechOccupations.2010)

dfForeignTechOccupations.2010$`AOS_%` <- round(100*dfForeignTechOccupations.2010$Asia/dfForeignTechOccupations.2010[1,"Asia"], digits=1)
dfForeignTechOccupations.2010$`NAOS_%` <- round(100*dfForeignTechOccupations.2010$NotAsia/dfForeignTechOccupations.2010[1,"NotAsia"], digits=1)
dfForeignTechOccupations.2010

dfTable3GG <- subset(dfForeignTechOccupations.2010, select=c(Occupation, Asia, `AOS_%`))
index <- order(dfForeignTechOccupations.2010$Asia, decreasing=TRUE)
(dfTable3GG <- dfTable3GG[index,])

dfTable3HH <- subset(dfForeignTechOccupations.2010, select=c(Occupation, NotAsia, `NAOS_%`))
index <- order(dfForeignTechOccupations.2010$NotAsia, decreasing=TRUE)
(dfTable3HH <- dfTable3HH[index,])

### Tables 3G Change in Foreign Asian
dfTable3G <- dfTable3D ### set up dimensions and some cols and order of rows
(dfTable3G <- dfTable3G[order(dfTable3G$Occupation),])
dfTable3G$`AOS_%`<- NULL
(dfTable3GG <- dfTable3GG[order(dfTable3GG$Occupation),])
dfTable3G$Asia.2010 <- dfTable3GG$Asia
dfTable3G$Change <- dfTable3G$Asia - dfTable3G$Asia.2010
dfTable3G$Per <- round((100 * dfTable3G$Change / dfTable3G$Asia.2010), digits=1)
dfTable3G <- dfTable3G[,c(1,3,2,4,5)] ### put 2010 before 2015
dfTable3G <- dfTable3G[order(dfTable3G$Asia, decreasing = TRUE),]
colnames(dfTable3G) <- c("Occupation", "As.2010", "As.2015", "Chng", "Per")
dfTable3G

############################
### Tables 3H Change in Foreign NotAsian
dfTable3H <- dfTable3E ### set up dimensions and some cols and order of rows
(dfTable3H <- dfTable3H[order(dfTable3H$Occupation),])
dfTable3H$`NAOS_%`<- NULL
(dfTable3HH <- dfTable3HH[order(dfTable3HH$Occupation),])
dfTable3H$NotAsia.2010 <- dfTable3HH$NotAsia
dfTable3H$Change <- dfTable3H$NotAsia - dfTable3H$NotAsia.2010
dfTable3H$Per <- round((100 * dfTable3H$Change / dfTable3H$NotAsia.2010), digits=1)
dfTable3H <- dfTable3H[,c(1,3,2,4,5)] ### put 2010 before 2015
colnames(dfTable3H) <- c("Occupation", "NAs.2010", "NAs.2015", "Chng", "Per")
dfTable3H <- dfTable3H[order(dfTable3H$NAsia, decreasing = TRUE),]
dfTable3H


save(dfTable1A, dfTable1B, dfTable2A, dfTable2B, dfTable3ABC, dfTable3D, dfTable3E, dfTable3G, dfTable3H, file="dfTab1A1B2A2B3ABCDEFGH.rda")

##################################
######################
### Tables 4A, 4B, 4C, 4D, 4E, 4F Racial, ethnic, female groups in each state  
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show link to full tables in page on git-io

makeTechPopTable <- function(Race){
    perRace <- paste0("per", Race)
    popRace <- paste0("pop", Race)
    dfEmp <- dfRaceSexCountAndShares[, c("State", "Totals", Race, perRace)]
    dfPop <- dfStatesPop3[, c("State", Race, perRace)]
    ### Example ==> c("State", "Black", "perBlack")
    
    ### Must change DC name to short form in dfPop before this merge
    dfPop[dfPop$State=="District of Columbia", "State"] <- "Dist of Col"
    dfTechPop <- merge(dfEmp, dfPop, by="State")
    
    RaceTech <- paste0(Race, "Tech")
    perRaceTech <- paste0("per", RaceTech)
    RacePop <- paste0(Race, "Pop")
    perRacePop <- paste0("per", RacePop)
    colnames(dfTechPop) <- c("State", "TotalTech", RaceTech, perRaceTech, RacePop, perRacePop)
    ### Example ==> c("State", "TotalTech", "BlackTech", "perBlackTech", "BlackPop", "perBlackPop")
    rownames(dfTechPop) <- c(dfTechPop[,"State"]) 
    
    dfTechPop$Parity <- round((dfTechPop[,perRaceTech]/dfTechPop[,perRacePop]), digits=2)
    index <- order(dfTechPop[, RaceTech], decreasing=TRUE)
    dfTechPop <- dfTechPop[index,]
    
    ### Calculate the percentage of the total for each race is in each state
    dfTechPop$perState <- round(100 * dfTechPop[,RaceTech]/dfTechPop[1,RaceTech[1]], digits=2)
    dfTechPop <- data.frame(dfTechPop[,c(1:2,8,3:7)]) ### move perState to 3rd column
    return(dfTechPop)
}

dfTechPopWhite <- makeTechPopTable("White")
dfTechPopBlack <- makeTechPopTable("Black")
dfTechPopHispanic <- makeTechPopTable("Hispanic")
dfTechPopAsian <- makeTechPopTable("Asian")
dfTechPopOTHERS <-makeTechPopTable("OTHERS")
dfTechPopFemale <-makeTechPopTable("Female")
dfTechPopFemAsian <-makeTechPopTable("FemAsian")
dfTechPopFemNonAsian <-makeTechPopTable("FemNonAsian")

head(dfTechPopFemAsian, 10)
head(dfTechPopBlack,10)
head(dfTechPopWhite,10)
head(dfTechPopHispanic,10)
head(dfTechPopAsian,10)
head(dfTechPopFemNonAsian, 10)

dfTable4A <- dfTechPopWhite 
dfTable4B <- dfTechPopBlack
dfTable4C <- dfTechPopAsian
dfTable4D <- dfTechPopHispanic 
dfTable4E <- dfTechPopFemAsian
dfTable4F <- dfTechPopFemNonAsian

##### Make foreign tech tables
makeForeignTechTable <- function(Area){
    perArea <- paste0("per", Area)
    dfTech <- dfForeignTechStates[, c("State", "Foreign", Area, perArea)]
    ### Example ==> c("State", "Foreign", "Asia", "perAsia")
    
    AreaTech <- paste0(Area, "Tech")
    perAreaTech <- paste0("per", AreaTech)
 
    colnames(dfTech) <- c("State", "Foreign", AreaTech, perAreaTech)
    ### Example ==> c("State", "TotalTech", "AsiaTech", "perAsiaTech")
    rownames(dfTech) <- c(dfTech[,"State"]) 
    
    index <- order(dfTech[, AreaTech], decreasing=TRUE)
    dfTech <- dfTech[index,]
    
    ### Calculate the percentage of the total for each Area is in each state
    dfTech$perState <- round(100 * dfTech[,AreaTech]/dfTech[1,AreaTech[1]], digits=2)
    dfTech <- data.frame(dfTech[,c(1:2,5,3:4)]) ### move perState to 3rd column
    return(dfTech)
}

dfTable4G <- makeForeignTechTable("Asia")
head(dfTable4G)
dfTable4H <- makeForeignTechTable("NotAsia")
head(dfTable4H)



#################################
################################
### Table 4 ... Big Six
dfTab <- subset(dfRaceSexCountAndShares, select=c("State","Totals"))
dfTab <- dfTab[order(dfTab$"Totals", decreasing=TRUE),]
rownames(dfTab) <- NULL
(dfTab<- (head(dfTab,7)))

sum6 <- sum(dfTab[2:7,2])
perTop6 <- round(100*sum6/dfTab[1,2], digits=1)
vec <- as.vector(c(dfTab[,2], sum6))
(dfTable4 <- data.frame(t(vec), perTop6))
colnames(dfTable4) <- c(dfTab[,1], "SumTop6", "Top6_%")
rownames(dfTable4) <- ""
dfTable4

save(dfTable4, dfTable4A, dfTable4B, dfTable4C, dfTable4D, dfTable4E, dfTable4F, dfTable4G, dfTable4H, file="dfTab4.rda")

#######################################
#######################################
### handy tool for spot checking data
selectParityDF <- function(Race, State){
   if (Race == "Black") {
       return(dfTechPopBlack)
   } else {
       if (Race =="White") {
           return(dfTechPopWhite)
       } else {
           if (Race =="Hispanic") {
               return(dfTechPopHispanic)
           } else {
               if (Race == "Asian") {
                    return(dfTechPopAsian)
               } else {
                   if (Race == "Female") {
                       return(dfTechPopFemale)
                   } else {
                        return (0)
                   }
               }
           }
       }
   }
}

getEmploymentRank <- function(Race, State) {
    df <- selectParityDF(Race, State)
    if (is.null(dim(df))) { 
        print(paste("Bad race input ... "))
        return(df)
    }
    df <- df[-1,] ### drop the top ALL row
    R <- which(df$State == State) 
    if (length(R) != 0) {
        return(R)
    } else {
        print(paste("Bad state input"))
    }
}

### Example of use of tool
R <- getEmploymentRank("White", "Texas")
R       

######################
### Maps 4A, B, C, D, E, F ... state maps of white, black, asian, hispanic, female Asians, and female non-Asians in  tech 
### Follow W. Chang's cookbook p 313 for U.S. with lower 48 states
states_map <- map_data("state") ### from ggplot]
theme_clean <- function(base_size = 12) {
    require(grid) # Needed for unit() function
    theme_grey(base_size) %+replace%
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        complete = TRUE
    )
}

### Explorations of the data showed that Asians had the highest concentration in California of any group in any state. So make their California concentratration the brightest color on all six maps. Store this concentration in the District of Columbia on each map because it is too small to be visible on these maps
(maxAsianPerState <- max(dfTechPopAsian[-1,"perState"])) ### omit total row
    
makeTechPopMap <- function(df, Group, maxPer, title) {
   
    ### Insert dummy max value into District of Columbia, too small to be visible
    df[df$State=="Dist of Col", "perState"] <- maxPer 
    
    ### and use full name of District, not short form used in these scripts
    df[df$State=="Dist of Col", "State"] <- "District of Columbia"
    
    legend = paste0("%")
    dfMap <- subset(df, select=c("State", "perState"), State!= c("ALL STATES"))     
    dfMap$state <- tolower(dfMap$State)
    dfMap <- merge(states_map, dfMap, by.x="region", by.y= "state")
    dfMap <- arrange(dfMap, group, order) 
    GroupData <- dfMap[,"perState"]
    
    ggMap <- ggplot(data=dfMap, aes(map_id=region, fill=GroupData))
    ggMap <- ggMap + geom_map(map=states_map, colour="black")
    ggMap <- ggMap + scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint= median(GroupData))       
    ggMap <- ggMap + expand_limits(x=states_map$long, y=states_map$lat) 
    ggMap <- ggMap + coord_map("polyconic") + labs(fill=legend) + theme_clean()
    ggMap <- ggMap + ggtitle(title) 
    ggMap <- ggMap + guides(fill=guide_legend(title.position = "left"))
    ggmap <- ggMap + theme(legend.title=element_blank(), plot.margin=unit(c(10,10,1,10), "cm")) 
    return(ggMap)
}

(dfMap4A <-makeTechPopMap(dfTechPopWhite,"White", maxAsianPerState, "A. White Techs"))
(dfMap4B <-makeTechPopMap(dfTechPopBlack,"Black", maxAsianPerState, "B. Black Techs"))
(dfMap4C <- makeTechPopMap(dfTechPopAsian,"Asian", maxAsianPerState, "C. Asian Techs"))
(dfMap4D <- makeTechPopMap(dfTechPopHispanic,"Hispanic", maxAsianPerState, "D. Hispanic Techs"))
(dfMap4E <- makeTechPopMap(dfTechPopFemAsian,"FemAsian", maxAsianPerState, "E. FemAsian Techs"))
(dfMap4F <- makeTechPopMap(dfTechPopFemNonAsian,"FemNonAsian", maxAsianPerState, "F. FemNonAsian Techs"))
(dfMap4G <- makeTechPopMap(dfTable4G,"Asia", maxAsianPerState, "G. Foreign (Asia) Techs"))
(dfMap4H <- makeTechPopMap(dfTable4H,"NotAsia", maxAsianPerState, "H. Foreign (not Asia) Techs"))

save(dfMap4A, dfMap4B, dfMap4C, dfMap4D, dfMap4E, dfMap4F, dfMap4G, dfMap4H, file="dfMap4.rda")

###########################
##########################
### Plots 5 ...
### Run regressions before plots so we can display beta values in upper left of each plot and impose the full regression line on the scatterplot ... smooth_geom only producesshort stubs for some groups

### Regression racial population vs. racial Tech 

makeLM <- function(df, Group) {
    df <- subset(df, State != "ALL STATES")
    f <- paste0("I(", Group, "Tech) ~ I(" , Group, "Pop/1000)")
    lmGroup <- lm(f, data = df)
    return(lmGroup)
}

lmBlack <- makeLM(dfTechPopBlack, "Black")
lmWhite <- makeLM(dfTechPopWhite, "White")
lmAsian <- makeLM(dfTechPopAsian, "Asian")
lmHispanic <-makeLM(dfTechPopHispanic, "Hispanic")
lmOTHERS <- makeLM(dfTechPopOTHERS, "OTHERS")
lmFemale <- makeLM(dfTechPopFemale, "Female")
lmFemAsian <- makeLM(dfTechPopFemAsian, "FemAsian")
lmFemNonAsian <- makeLM(dfTechPopFemNonAsian, "FemNonAsian")

### Save betas for later tables
beta1000 <- c(lmWhite$coef[2], lmBlack$coef[2], lmAsian$coef[2], lmHispanic$coef[2], lmOTHERS$coef[2], lmFemale$coef[2], lmFemAsian$coef[2], lmFemNonAsian$coef[2])
names(beta1000) <- c("White", "Black", "Asian", "Hispanic", "OTHERS", "Female",  "FemAsian", "FemNonAsian")
beta1000 <- round(beta1000, digits=2)
beta1000["Black"]
beta1000["Asian"]
beta1000["FemAsian"]
beta1000["Female"]


#####################
### Now the plots
###################
plotEmpVsPop <- function(df, Group, lmGroup, maxPlotPop, maxPlotTech, alpha){
    
    ### Note: geom_smooth drew short stubby lines for some groups that
    ### had small max populations ... so use geom_abline to draw 
    ### full lines for all groups based on regression slope and intercept
    AB <- summary(lmGroup)$coefficients[,1]
    A <- AB[1] ### intercept of regression line
    B <- AB[2] ### slope of regressionline
    
    GroupPop <- paste0(Group, "Pop")
    GroupTech <- paste0(Group, "Tech")
    annot <- paste0("Beta = ", beta1000[Group])
   
    ### Example ==> aes(df[-1,x=I(df[-1,"BlackPop"]/1000), y = df[-1,"BlackTech"]
    ggScatter <- ggplot(df[-1,], aes(x=I(df[-1,GroupPop]/1000), y=df[-1,GroupTech])) + geom_point(shape=1) 
    ggScatLine <- ggScatter + xlim(0, maxPlotPop) + ylim(0, maxPlotTech)
    ggScatLine <- ggScatLine + geom_abline(intercept=A, slope=B, colour="blue",size=0.8)
    
    ### Example ==> xlab("BlackPop/1000) + ylab("BlackTech")
    ggScatLine <- ggScatLine + xlab("Pop/1000") + ylab("Tech")
    ggScatLine <- ggScatLine + ggtitle(paste0(alpha, " ", Group," -- Tech vs Pop/1000")) + theme(plot.title = element_text(size=12))
    ggScatLine <- ggScatLine + annotate("text", label=annot, x=-Inf, y=Inf, hjust=-.2, vjust=2)
    return(ggScatLine)
}

(maxPlotTech <- max(dfRaceSexCountAndShares[-1, c(3:6)]))

### mqx value for white, black, asian, hispanic, femAsian, femNonAsian
(maxPlotPop <- max(dfStatesPop3[-1,c(3:6,9:10)])/1000)

(ggPlotWhite <- plotEmpVsPop(dfTechPopWhite, "White", lmWhite, maxPlotPop, maxPlotTech, "A."))
(ggPlotBlack <- plotEmpVsPop(dfTechPopBlack, "Black", lmBlack, maxPlotPop, maxPlotTech, "B. ")) 
(ggPlotAsian <- plotEmpVsPop(dfTechPopAsian, "Asian", lmAsian, maxPlotPop, maxPlotTech, "C."))
(ggPlotHispanic <- plotEmpVsPop(dfTechPopHispanic, "Hispanic", lmHispanic, maxPlotPop, maxPlotTech, "D."))
(ggPlotFemAsian <- plotEmpVsPop(dfTechPopFemAsian, "FemAsian", lmFemAsian, maxPlotPop, maxPlotTech, "E.")) 
(ggPlotFemNonAsian <- plotEmpVsPop(dfTechPopFemNonAsian, "FemNonAsian", lmFemNonAsian, maxPlotPop, maxPlotTech, "F."))


###############################################################
##########################
### Table 5 ... summary of national advantages for each group
makeSummary <- function(rList, beta){
    rows <- length(rList)
    cols <- dim(rList[[1]])[2] ### use cols in first df in list
    mat <- matrix(nrow = rows, ncol = cols-3)
    for(I in 1:rows){
       df <- rList[[I]]
       vec <- c(unlist(df[1, -c(1:3)])) ### not the first three cols
       mat[I,] <- vec
    }
    rownames(mat) <- names(rList)
    mat <- cbind(mat, beta)
    colnames(mat) <- c("Tech", "T_%", "Population", "P_%", "Par", "beta1000")
    
    dfTable <- as.data.frame(mat)
    return(dfTable)
}

rList <- list(dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopOTHERS, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian)
names(rList) <- c("White", "Black", "Asian", "Hispanic", "OTHERS", "Female", "FemAsian", "FemNonAsian")
(dfTable5 <- makeSummary(rList, beta1000))

save(ggPlotAsian, ggPlotWhite, ggPlotBlack, ggPlotHispanic, ggPlotFemAsian, ggPlotFemNonAsian, beta1000, dfTable5, file="dfPlot5Tab5beta1000.rda")

#######################################
######################
#### Conclusions
### Table 6. Stats for parity variable of racial groups in each state  
makeParity <- function(listDFs){
    ng <- length(listDFs)
    matParity <- matrix(NA, nrow=ng, ncol = 6)
    dfP <- data.frame(matParity)
    colnames(dfP) <- c("Min", "Q1", "Med", "Mean", "Q3", "Max")
    for (i in 1:ng) {
        df<- listDFs[[i]]
        dfP[i,] <- summary(df$Parity)
        
        ### Change min to minimum value > 0 ... Census said NA because not enough
        ### observations in sample to estimate techs in some small states
        ### ... my code converted NA's to 0s for Techs ==> 0s for percent Techs
        dfP[i,1] <- min(df[df[,"Parity"]> 0, "Parity"])
    }
    dfP <- subset(dfP, select=-c(Mean)) ### Drop mean values
    dfP <- round(dfP, digits=2)
    rownames(dfP) <- names(listDFs)
    return(dfP)
}

listDFs <- list(dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian)
names(listDFs) <- c("White",  "Black", "Asian", "Hispanic",  "Female",  "FemAsian", "FemNonAsian")
(dfParity <- makeParity(listDFs))

dfTable6 <- dfParity


##################################
####### Tables 7 ... Finalists for black, hispanic, femAsians, and femNonAsians
makeTable7 <- function(dfIn, dfParity, Group, letter) {
    dfFinal <- subset(dfIn[2:11,], Parity >= dfParity[Group, "Med"]) 
    perGroupTech <- paste0("per",Group, "Tech")
    
    dfOut <- subset(dfFinal, select=c("State", perGroupTech, "Parity"))
    L <- dim(dfOut)[1]
    L <- min(L, 5) ### min(L,5)
    dfOut <- head(dfOut[order(-dfOut$Parity),],L)
    rows <- as.character(seq(1:L)) ### some tables may be shorter than 5
    rownames(dfOut) <- rows
    tabName <- paste0("7", letter, ". ", Group)
    colnames(dfOut) <- c(tabName, "Tech %", "Parity")

    return(dfOut)
}
### Tables 7 ... sorted by parity
(dfTable7A <- makeTable7(dfTechPopWhite , dfParity, "White", "A"))
(dfTable7B <- makeTable7(dfTechPopBlack , dfParity, "Black", "B"))
(dfTable7C <- makeTable7(dfTechPopAsian , dfParity, "Asian", "C"))
(dfTable7D <- makeTable7(dfTechPopHispanic , dfParity, "Hispanic", "D"))
(dfTable7E <- makeTable7(dfTechPopFemAsian , dfParity, "FemAsian", "E"))
(dfTable7F <- makeTable7(dfTechPopFemNonAsian , dfParity, "FemNonAsian","F"))


### Sorted by tech share of the info tech sector 
makeTable8 <- function(dfIn, Group, letter){
    vec <- dfIn[, "Tech %"] 
    dfOut <- dfIn[order(-vec),]

    L <- dim(dfOut)[1]
    rows <- as.character(seq(1:L)) ### some tables may be shorter than 5
    rownames(dfOut) <- rows
    tabName <- paste0("8", letter, ". ",  Group)
    colnames(dfOut) <- c(tabName, "Tech %", "Parity")
    return(dfOut)
}

(dfTable8A <- makeTable8(dfTable7A,"White", "A")) 
(dfTable8B <- makeTable8(dfTable7B, "Black", "B")) 
(dfTable8C <- makeTable8(dfTable7C, "Asian", "C")) 
(dfTable8D <- makeTable8(dfTable7D, "Hispanic", "D")) 
(dfTable8E <- makeTable8(dfTable7E, "FemAsian", "E"))
(dfTable8F <- makeTable8(dfTable7F, "FemNonAsian", "F")) 


###########################
### Table 9 ... Asians and H-1B techs
dfTable9 <- data.frame(t(dfTable4[,2:7]))

AsianTech <- dfTechPopAsian[c("California", "Texas", "New York", "Florida", "Virginia", "Illinois"), "AsianTech"]
dfTable9$TotAsian <- AsianTech

### H-1B data from Website
h1bTech <- c(98457, 51814, 	48836, 17629, 15467, 27399)
dfTable9$h1bTech <- h1bTech
dfTable9

perH1B <- round(c(100 * dfTable9$h1bTech/dfTable9$TotAsian), digits=1)
dfTable9$perH1B <- perH1B

colnames(dfTable9) <- c("Total_T", "Asian_T", "H-1B", "H-1B_%")
dfTable9

save(dfTable6, dfTable7A, dfTable7B, dfTable7C, dfTable7D, dfTable7E, dfTable7F, dfTable8A, dfTable8B, dfTable8C, dfTable8D, dfTable8E, dfTable8F, dfTable9, file="dfTab67A7B7C7D7E7F8A8B8C8D8E8F.rda")
