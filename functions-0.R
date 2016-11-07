### Functions used by other files    


##################################
### Data-1A & Data-1B
addTotCol <- function(df, colSeq, totName){
### Add a totals column named via totName, inserted after first column, e.g., "State"
### e.g. State, Total, C2, C3, ... C6
    
    L <- length(colSeq)
    colnames <- colnames(df[,colSeq])
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
    denomVec <- t(df[, denomCol]) ### note the t() transposing to row vector
    perCols <- round((100 * dfNumerCols/denomVec), digits=1)
    colnames(perCols) <- paste0("per", colnames)
    df2 <- cbind(df, perCols)
    return(df2)
### For foreign techs and other data frames the denominator will be zero, which will generate a NAN value for percentage ... convert NANs to zeros
}

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

addTotalsRow <- function(df, totCol, numCols, perCols, nameTotRow){
### Add totals row to a data.frame 
### Typical ... State, Col with totals for each row, columns with numbers, cols with percents
    sumNumCols <- colSums(df[, numCols])
    sumTotCol <- sum(df[,totCol])
    perColsShares <- round(100 * sumNumCols/sumTotCol, digits=1)
    
    dfTotalsRow <- df[1,] ### dummy copy to get columns and types
    ### dfTotalsRow[1,1] <- nameTotRow
    dfTotalsRow$State <- nameTotRow
    dfTotalsRow[1,totCol] <- sumTotCol
    dfTotalsRow[1,numCols] <- sumNumCols
    dfTotalsRow[1,perCols] <- perColsShares

    df2 <- rbind(dfTotalsRow, df)  
    rownames(df2) <- df2$State
    return(df2)
}


##################################
### Stats-2A


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
    
    ### Calculate the percentage of the total for each race is in each state
    dfTechPop$perState <- round(100 * dfTechPop[,RaceTech]/dfTechPop[1,RaceTech[1]], digits=1)
    dfTechPop <- data.frame(dfTechPop[,c(1:2,8,3:7)]) ### move perState to 3rd column
    return(dfTechPop)
}

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
    dfTech$perState <- round(100 * dfTech[,AreaTech]/dfTech[1,AreaTech[1]], digits=1)
    dfTech <- data.frame(dfTech[,c(1:2,5,3:4)]) ### move perState to 3rd column
    
    return(dfTech)
}

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
    df[df$State=="Dist of Col", "perState"] <- maxPer 
    
    ### and use full name of District, not short form used in these scripts
    df[df$State=="Dist of Col", "State"] <- "District of Columbia"
    
    legend = paste0("%")
    dfMap <- subset(df, select=c("State", "perState"), State!= c("ALL STATES"))     
    dfMap$state <- tolower(dfMap$State)
    dfMap <- merge(states_map, dfMap, by.x="region", by.y= "state")
    dfMap <- arrange(dfMap, group, order) 
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
save(addTotCol, addPerCols, readCodeBooks, addTotalsRow, makeSummary, plotEmpVsPop, makeLM, makeTechPopMap, theme_clean, getEmploymentRank, selectParityDF, makeForeignTechTable, makeTechPopTable, makeParity, makeTable7, makeTable8, file="functions-0.rda")

