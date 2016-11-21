
### Functions used by scripts inData and Stat files   

##################################
### Data-1

readCodeBooks <- function() {
    ### Read manually edited codebooks 
    ###  ... commas added between codes and labels ... commas deleted within labels ... and 99 Hispanic added manually
    file = "Codes-Race.txt"
    raceCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
    raceCodes
    
    file = "Codes-State.txt"
    ### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping
    stateCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
    stateCodes$State <-gsub("/.*","",stateCodes$State) ### Drop state initials, e.g., "New York/NY
    stateCodes
    
    file="Codes-Sex.txt"
    sexCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
    sexCodes
    
    file="Codes-Citizen.txt"
    citizenCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
    citizenCodes
    
    file="Codes-Occupation.txt"
    occupationCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
    occupationCodes
    
    file="Codes-Area.txt"
    areaCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
    areaCodes
    
    listCodes <- list(Birth=areaCodes, Occupation=occupationCodes, Citizen=citizenCodes, Sex=sexCodes, State=stateCodes, Race=raceCodes)
    return(listCodes)
}

addTotCol <- function(df, colSeq, totName){
### Add a totals column named via totName, inserted after first column, e.g., "State"
### e.g. State, Total, C2, C3, ... C6
    
    L <- length(colSeq)
    colnames <- colnames(df[,colSeq]) ### Get names of colums to be added together, column by column
    df$Total <- rowSums(df[,colSeq])
    df2 <- df[, c(1, L+2, colSeq)]
    colnames(df2) <- c(colnames(df[,1]), totName, colnames)
    return(df2)
}

addPerCols <- function(df, denomCol, numerCols) {
### Add percentage columns, where denomCol is the denominator and numerCols are the numerators
### Creates standard names for percentage cols = "per" + oroginal col name
### denomCol should always be 2, the Totals column
   
    df <- as.data.frame(df)
    colnames <- colnames(df[, numerCols])
    dfNumerCols <- subset(df, select=c(numerCols))
    denomVec <- t(df[, denomCol]) ### note the t() transposing to row vector to a column
    perCols <- round((100 * dfNumerCols/denomVec), digits=1)
    colnames(perCols) <- paste0("per", colnames)
    df2 <- cbind(df, perCols)
    
    ### For foreign techs and other data frames the denominator might be zero, which will generate a NAN value for percentage ... convert NANs to zeros   
    df2[is.na(df2)] <- 0 
    return(df2)
}

addTotColSharePerRowCol <- function(df, target) {
### Given a col with a total in the top row, calculate the percent of each row of the total ... insert before target col ... new col called "perShare"
    total <- df[1,target]
    cols <- dim(df)[2]
    df$perShare <- round(100 * df[,target]/total, digits=1)
    
    ### position shares before target
    df <-df[,c(1:(target-1), (cols + 1), target: cols)]
    return(df)
}

addTotalsRow <- function(df, totCol, numCols, perCols, nameTotRow){
### Add totals row to a data.frame 
### Typical ... State, Col with totals for each row, columns with numbers, cols with percents
    sumNumCols <- colSums(df[, numCols])
    sumTotCol <- sum(df[,totCol])
    perColsShares <- round(100 * sumNumCols/sumTotCol, digits=1)
    
    dfTotalsRow <- df[1,] ### dummy copy to get columns and types
    levels <- levels(df[,1])
    levels(dfTotalsRow[,1]) <- c(nameTotRow, levels)

    dfTotalsRow[1,1] <- nameTotRow
    dfTotalsRow[1,totCol] <- sumTotCol
    dfTotalsRow[1,numCols] <- sumNumCols
    dfTotalsRow[1,perCols] <- perColsShares

    df2 <- rbind(dfTotalsRow, df)  
    rownames(df2) <- df2[, 1]
    return(df2)
}

createPopRaceAndShares <- function(df, bCitizen=TRUE){
### Create race, sex, count, and shares dataframe with values derived from the Census file
###
### Function expects a data frame whose columns are named with the following names, no variations
###   "personalWeight", "Race", "Sex", "Citizen", "State", "Hisp", "Citizen", Birth", "Occupation"
### But the data frame for some data frames will not contain Citizen or Birth or Occupation
### 
### If the subset of Citizens is required, bCitizen bool = TRUE
### If foreigners are required, bCitizen = FALSE
### Current versions of Best States will not required both citizens and foreigners
###
### User renames the dataframe upon return   

### 1. Add new category to race = "hisp"
    ### ... ACS coded HISP = "1" for "not Hispanic" --change race values to 99 ("hispanic") when hisp != 1
    rows <- df$Hisp != "1"
    df$Race[rows] <- 99
    head(df) 
    
### 2. Codebooks
    listCodes <- readCodeBooks()
    
### 3. Convert coded categorical variables to factors ... sex, race, state, occupation ... drop padding/blanks before/after each category 
    df$Race <- as.factor(df$Race)
    ### listCodes [[ ]] required to dig out the second column in the code table, and for other codes
    levels(df$Race) <- trimws(listCodes[["Race"]][,2]) 
    
    df$Sex <- as.factor(df$Sex)
    levels(df$Sex) <- trimws(listCodes[["Sex"]][,2])
    
    df$State <- as.factor(df$State)
    levels(df$State) <- trimws(listCodes[["State"]][,2])
    ### Note: District of Columbia abbreviated "Dist of Col" ... let table fit on blog page without wrapping
    
    if("Occupation" %in% names(df)) {
        df$Occupation <- as.factor(df$Occupation)
        levels(df$Occupation) <- trimws(listCodes[["Occupation"]][,2])
    }
    
    if("Birth" %in% names(df)) {    
        df$Birth <- as.factor(df$Birth)
        levels(df$Birth) <- trimws(listCodes[["Birth"]][,2])
    }
    
    if("Citizen" %in% names(df)) {
        df$Citizen <- as.factor(df$Citizen)
        levels(df$Citizen) <- trimws(listCodes[["Citizen"]][,2])   
    }
    
    
### 4. Get citizen subset or foreign subset ... but never both citizens and foreign
    if(bCitizen == TRUE) {
        df3 <- subset(df, Citizen!="No") 
    } else {
        df3 <- subset(df, Citizen=="No")
    }

### 5. Calculate racial group's Count per each state
    census3StateRace <- group_by(df3, State, Race) 
    dfPtsPwtStateRace <- summarise(census3StateRace, ptsPwtStateRace = sum(personalWeight))
    dfRaceCountPerState <- spread(dfPtsPwtStateRace, key=Race, value=ptsPwtStateRace, fill=0, drop=FALSE)

### 6. Combine all groups other than black, white, asian, and hispanic into OTHERS
    columnNames <- c("State", "White", "Black", "amIn", "alNat", "amInAlNat", "Asian", "pacific", "other", "many" , "Hispanic")
    colnames(dfRaceCountPerState) <- columnNames
    dfRaceCountPerState$OTHERS <- dfRaceCountPerState$amIn + dfRaceCountPerState$alNat + dfRaceCountPerState$amInAlNat + dfRaceCountPerState$pacific + dfRaceCountPerState$other + dfRaceCountPerState$many
    dfRaceCountPerState <- subset(dfRaceCountPerState, select=-c(amIn, alNat, amInAlNat, pacific, other, many))

### 7. Add "totals" column after "state" ... 
    dfRaceCountPerState <- addTotCol(dfRaceCountPerState, 2:6, "Totals")
          
### 8 Calculate the Count each sex per state ... Thank you, Hadley ... :-)
    census3StateSex <- group_by(df3, State, Sex) 
    dfPtsPwtStateSex <- summarise(census3StateSex, ptsPwtStateSex = sum(personalWeight))
    dfSexCountPerState <- spread(dfPtsPwtStateSex, key=Sex, value=ptsPwtStateSex)
    dfSexCountPerState[is.na(dfSexCountPerState)] <- 0 ### Replace NAs with zeros
    dfFemale <- subset(dfSexCountPerState, select=c(State, Female))
    colnames(dfFemale) =  c("State", "Female")
 
### 9. Asian females
    census3StateRaceSex <- group_by(df3, State, Sex, Race) 
    dfSumPwtStateRaceSex <- summarise(census3StateRaceSex, SumPwtStateRaceSex = sum(personalWeight))
    dfRaceSexPerState <- spread(dfSumPwtStateRaceSex, key=Race, value=SumPwtStateRaceSex)
    dfRaceSexPerState[is.na(dfRaceSexPerState)] <- 0 ### Replace NAs with zeros
    dfFemAsian <- subset(dfRaceSexPerState, Sex=="Female", select=c(State,Asian))
    colnames(dfFemAsian) =  c("State", "FemAsian")
    
### 10. Combine the two female dfs, create FemNonAsian
    dfFemale <- merge(dfFemale, dfFemAsian)
    ### head(dfFemale)
    dfFemale$FemNonAsian <- dfFemale$Female - dfFemale$FemAsian

    ##### column merge dfFemales at this point to end 
    dfRaceSexCountPerState <- merge(dfRaceCountPerState, dfFemale)
    ### head(dfRaceSexCountPerState)
    ### print(head(dfRaceSexCountPerState))
    
### 11/12. Calculate each racial group's share of total tech Count in each state
    dfRaceSexCountAndShares <- addPerCols(dfRaceSexCountPerState, 2, 3:10)
    
### 13. Add a totals row 
    dfRaceSexCountAndShares  <- addTotalsRow(dfRaceSexCountAndShares, 2, 3:10, 11:18, "ALL STATES")
    
    ### print(head(dfRaceSexCountAndShares))
    return(dfRaceSexCountAndShares)
}

createOccupationRaceSexProfiles <- function(df){
    ### Add new category to race = "hisp"
    ### ... ACS coded HISP = "1" for "not Hispanic" --change race values to 99 ("hispanic") when hisp != 1
    rows <- df$Hisp != "1"
    df$Race[rows] <- 99
    head(df) 
    
    listCodes <- readCodeBooks()
    
    ### Convert coded categorical variables to factors
    df$Race <- as.factor(df$Race)
    ### listCodes [[ ]] required to dig out the second column in the code table, and for other codes
    levels(df$Race) <- trimws(listCodes[["Race"]][,2]) 
    
    df$Sex <- as.factor(df$Sex)
    levels(df$Sex) <- trimws(listCodes[["Sex"]][,2])

    df$Occupation <- as.factor(df$Occupation)
    levels(df$Occupation) <- trimws(listCodes[["Occupation"]][,2])
    
    OccRaceSex <- group_by(df, Occupation, Race, Sex)
    dfPtsPwtOccRaceSex <- summarise(OccRaceSex, ptsPwtOccRaceSex = sum(personalWeight))
    dfOccupationRaceSexProfiles <- spread(dfPtsPwtOccRaceSex, key=Sex, value=ptsPwtOccRaceSex, fill=0, drop=FALSE)
    dfOccupationRaceSexProfiles <- as.data.frame((dfOccupationRaceSexProfiles))
    
    ### Occupation Race Male Female
    return(dfOccupationRaceSexProfiles)
}

createOccupationStateRaceSexProfiles <- function(df){
    ### Add new category to race = "hisp"
    ### ... ACS coded HISP = "1" for "not Hispanic" --change race values to 99 ("hispanic") when hisp != 1
    rows <- df$Hisp != "1"
    df$Race[rows] <- 99
    head(df) 
    
    listCodes <- readCodeBooks()
    
    ### Convert coded categorical variables to factors
    df$State <- as.factor(df$State)
    levels(df$State) <- trimws(listCodes[["State"]][,2])
    ### Note: District of Columbia abbreviated "Dist of Col" ... let table fit on blog page without wrapping
    
    df$Race <- as.factor(df$Race)
    ### listCodes [[ ]] required to dig out the second column in the code table, and for other codes
    levels(df$Race) <- trimws(listCodes[["Race"]][,2]) 
    
    df$Sex <- as.factor(df$Sex)
    levels(df$Sex) <- trimws(listCodes[["Sex"]][,2])
    
    df$Occupation <- as.factor(df$Occupation)
    levels(df$Occupation) <- trimws(listCodes[["Occupation"]][,2])
    
    OccStateSex <- group_by(df, Occupation, State, Sex)
    df2 <- summarise(OccStateSex, ptsPwtOccSex = sum(personalWeight))
    df2 <- spread(df2, key=Sex, value=ptsPwtOccSex, fill=0, drop=FALSE)
    
### Roll up the state
    OccState <- group_by(df2, State)
    df3 <- summarise(df2, Male=sum(Male), Female=sum(Female), fill=0, drop=FALSE)
    df3 <- subset(df3, select=-c(fill, drop))
   
    ### Occupation State Male Female
    return(df3)
}


##################################
### Stats-2A

makeNumPerTable <- function(df, Total=NULL){
### Function expects a data frame with one row and named columns
### Calculates total; then computes percentages of Total for each value
### BInds percentages as second row, then adds rownames "Num" and "Per"
    
    names <- colnames(df)
    if(is.null(Total)) { ### Make total if user does not provide
        Total <- as.data.frame(sum(df))
        df1 <- cbind(Total, df)
        colnames(df1) <- c("Total", unlist(names))
    } else {
        df1 <- df
    }
    
    perRow <- round(100 * df1[1,]/df1[1,1], digits=1)
    df2 <- (as.character(perRow))

    df1 <- prettyNum(df1, big.mark = ",")
    df <- rbind(df1, df2)
    rownames(df) <- c("Num", "Per")
    
    return(df)
}

createProfile <- function(df, group=NULL, state=NULL) {
### Input data frame = Occupation State Race Male Female for the specified group   
### Output data frame =  Occupation, Tech15, perTS, Fem, Per15 for the specified group
print(paste("group in createProfile = ", group))
      
#1. Select the group's records
    if (!is.null(group)) {
        df <- subset(df, Race == group)
    }
    
#2. Select data columns
    if (!is.null(state)) {
print(paste("state in createProfile = ", state))
        df <- subset(df, State==state, select=c("Occupation", "Male", "Female"))
        df$State <- NULL 
        
    } else {
        df <- subset(df, select=c("Occupation", "Male", "Female"))
    }

    eachOcc <- group_by(df, Occupation)
    df <- summarise(eachOcc, Male=sum(Male), Female=sum(Female), fill=0, drop=FALSE)
    df <- subset(df, select=-c(fill, drop))

    df <- addTotCol(df, 2:3, "Total")
    df <- addPerCols(df, 2, 3:4)
    df <- addTotalsRow(df, 2, 3:4, 5:6, "All Occupations")
    df <- addTotColSharePerRowCol(df, 2)
    index <- order(df[,2], decreasing = TRUE)
    dfProfile <- df[index,]
    return(dfProfile)
}

createCompareProfile <- function(df1, df2) {
### Input ==> Occupation, Total, share, Male, Female, perMale, perFemale
### Output ==> Occupation, Tech10, Tech15, Change, PerChange, PerF10
### df1 is earlier year, e.g., 2010 ... df2 is later year, e.g., 2015
    
### 1. Impose same order on both data frames
    index <- order(df1$Occupation)
    df1 <- df1[index,]
    index <- order(df2$Occupation)
    df2 <- df2[index,]

### 2. Rename columns with suffixes to tell variables from each other
    colnames1 <- colnames(df1)
    colnames(df1) <- c(paste0(colnames1, "1"))
    colnames2 <- colnames(df2)
    colnames(df2) <- c(paste0(colnames2, "2"))   

### 3. Merge and create Change and perChange
    df3 <- merge(df1, df2, by.x="Occupation1", by.y="Occupation2")
    df3$Change <- df3$Total2 - df3$Total1
    df3$perChange <- round(100 * df3$Change/ df3$Total1, digits=1)
    df4 <- subset(df3, select=c(Occupation1, Total1, Total2, Change, perChange, perFemale1))
    
    index = order(df4$Total2, decreasing = TRUE) ### Order by later year, 2015
    df4 <- df4[index,]
    
    dfCompProfile <- df4
    return(dfCompProfile)
}

createListProfiles <- function(df1, df2, group=NULL, state=NULL) {
### df1 is data frame that contains the early year, df2 contains the second year
### Returns list of data frames, 1 = display Table for year 1, 2 = compare table for years 1 and 2, full data frame for year 1, and full data frame for year 2
    
    print(paste("group in createListProfiles=", group))
    print(is.null(group))
    
    dfFullTab1 <- createProfile(df1, group=group, state=state)
    dfFullTab2 <- createProfile(df2, group=group, state=state)
    
    dfDisplayTab2 <- subset(dfFullTab2, select=-c(Male, perMale))
    colnames(dfDisplayTab2) <- c("Occupation", "perTS", "Tech15", "Fem", "perF15")
    
    ### Compare year1 to year2
    dfCompTab <- createCompareProfile(dfFullTab1, dfFullTab2)
    colnames(dfCompTab) <- c("Occupation", "Tech10", "Tech15", "Change", "perCh", "perF10")
    listProfiles <-list(dfDisplayTab2, dfCompTab, dfFullTab1, dfFullTab2) 
    
    return(listProfiles)
}

############################
############################
### Stats2B

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
    
    colnames <- as.vector(colnames(dfTechPop))
    L <- length(colnames)
    dfTechPop <- addTotColSharePerRowCol(dfTechPop, 3)
    colnames(dfTechPop) <- c(colnames[1:2], "perState", c(colnames[3:L]))
    return(dfTechPop)
}

makeForeignTechTable <- function(Area){
    perArea <- paste0("per", Area)
    dfTech <- dfForeignRaceSexCountAndShares[, c("State", "Totals", Area, perArea)]
    ### Example ==> c("State", "Foreign", "Asia", "perAsia")
    
    AreaTech <- paste0(Area, "Tech")
    perAreaTech <- paste0("per", AreaTech)
    
    colnames(dfTech) <- c("State", "Foreign", AreaTech, perAreaTech)
    ### Example ==> c("State", "TotalTech", "AsiaTech", "perAsiaTech")
    rownames(dfTech) <- c(dfTech[,"State"]) 
    
    index <- order(dfTech[, AreaTech], decreasing=TRUE)
    dfTech <- dfTech[index,]
    dfTech <- addTotColSharePerRowCol(dfTech, 3)
    
    return(dfTech)
}

makeForeignNonAsianTechTable <- function(dfAsian){
### Handle this as special case ... derive from Asian tech table
    dfNonAsian <- subset(dfAsian, select=c(State, Foreign, AsianTech))
    dfNonAsian <- as.data.frame(dfNonAsian)
    dfNonAsian$NonAsianTech <- dfNonAsian$Foreign - dfNonAsian$AsianTech
    dfNonAsian$AsianTech <- NULL
    
    dfNonAsian <- addPerCols(dfNonAsian, 2, 3)
    dfNonAsian <- addTotColSharePerRowCol(dfNonAsian, 3) 
    
    return(dfNonAsian)
}

addMissingStatesToTable <- function(dfFor, dfAll){
### Special function adds states missing from Foreign Asian and Foreign NonAsian 4 Table
###    with zero values
### dfAll is a data frame whose State column contains all states
    
    dfAll <- dfAll[1:5]
    dfAll[, c(2:5)] <- 0 ### initialize all data slots to zeros
    colnames(dfAll) <- colnames(dfFor)
    
    index1 <- order(dfAll$State)
    dfAll <- dfAll[index1,]
    
    index2 <- order(dfFor$State)
    dfFor <- dfFor[index2,]

    allStates <- as.character(dfAll$State) ### list of all states
    fStates <- as.character(dfFor$State)
    
    j <- 1
    bComplete <- as.vector(allStates %in% fStates)
    for (i in 1:52) {
        if (bComplete[i]){ ### Copy data for states that are complete, no missing
            dfAll[i,] <- dfFor[j,] 
            j <- j + 1
        }
    }
    index3 <- order(dfAll$perState, decreasing = TRUE)
    dfAll <- dfAll[index3,]
    return(dfAll)
}

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

makeTechPopMap <- function(df, Group, maxPer, title) {
    
    ### Insert dummy max value into District of Columbia, too small to be visible
    df$State <- as.character(df$State) ### convert to char to enable change in DC
    df[df$State=="Dist of Col", "perState"] <- maxPer 

    ### and use full name of District, not short form used in these scripts
    df[df$State=="Dist of Col", "State"] <- "District of Columbia"
    
    legend = paste0("%")
    dfMap <- subset(df, select=c("State", "perState"), State!= c("ALL STATES"))     
    dfMap$state <- tolower(dfMap$State)
    dfMap <- merge(states_map, dfMap, by.x="region", by.y= "state")
    ####dfMap <- arrange(dfMap, group, order) 
    GroupData <- dfMap[,"perState"]
    
    ### high="#BB650B"
    
    ggMap <- ggplot(data=dfMap, aes(map_id=region, fill=GroupData))
    ggMap <- ggMap + geom_map(map=states_map, colour="black")
    ggMap <- ggMap + scale_fill_gradient2(low="#559999", mid="grey90", high="#FF0000", midpoint= median(GroupData))       
    ggMap <- ggMap + expand_limits(x=states_map$long, y=states_map$lat) 
    ggMap <- ggMap + coord_map("polyconic") + labs(fill=legend) + theme_clean()
    ggMap <- ggMap + ggtitle(title) 
    ggMap <- ggMap + guides(fill=guide_legend(title.position = "left"))
    ggmap <- ggMap + theme(legend.title=element_blank(), plot.margin=unit(c(10,10,1,10), "cm")) 
    return(ggMap)
}


makeLM <- function(df, Group) {
    df <- subset(df, State != "ALL STATES")
    f <- paste0("I(", Group, "Tech) ~ I(" , Group, "Pop/1000)")
    lmGroup <- lm(f, data = df)
    return(lmGroup)
}

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

makeSummary <- function(rList, beta){
### Creates Table 5 summary of Tech, perTech, Pop, perPop, Parity, betas for each state
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


####################################
### Stats-2C
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

###################################
save(readCodeBooks, addTotCol, addPerCols, addTotColSharePerRowCol, addTotalsRow, addMissingStatesToTable, createOccupationRaceSexProfiles, createOccupationStateRaceSexProfiles, createPopRaceAndShares, makeNumPerTable, createProfile, createCompareProfile, createListProfiles, makeSummary, plotEmpVsPop, makeLM, makeTechPopMap, theme_clean, makeForeignTechTable, makeForeignNonAsianTechTable, makeTechPopTable, makeParity, makeTable7, makeTable8, file="functions-0.rda")

