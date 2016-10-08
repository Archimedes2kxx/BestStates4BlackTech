### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

### Version 0.9 analyzes residence/employment by sex for Asian group 

################################################
################################################ THurs 9/29 @ 5"12 pm
### Use map_dat() in ggplot2 as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################

load(file="dfEmploymentAndShares.rda")
load(file="dfStatesPop3.rda") 
load("dfCensus2.rda") 
load("dfSexResidenceAndShares.rda")

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
colNames_per <- c("ALL", "per_white", "per_black", "per_asian", "per_hispanic", "per_OTHERS")
colnames(dfTable1Aper) <- colNames_per
dfTable1Aper <- as.character(round(dfTable1Aper, digits=1))
dfTable1Aper

dfTable1A <- data.frame(rbind(dfTable1Apop, dfTable1Aper))
rownames(dfTable1A) <- c("Number", "Percent")
dfTable1A


### Table 1B. White, Black, Asian, Hispanic Components of U.S. Tech Sector in 2014
dfTable1Btech <- dfEmploymentAndShares[1,2:7]
colnames(dfTable1Btech) <- colNames
dfTable1Btech

dfTable1Btech <- prettyNum(dfTable1Btech, big.mark = ",")
dfTable1Btech

### Percentages of total, white, black, asian, hispanic, and OTHERS
dfTable1Bper <- dfEmploymentAndShares[1,c(2,11:15)]
dfTable1Bper[1,1] <- 100 ### 100 percent for ALL
colNames_per <- c("ALL", "per_white", "per_black", "per_asian", "per_hispanic", "per_OTHERS")
colnames(dfTable1Bper) <- colNames_per
dfTable1Bper <- as.character(round(dfTable1Bper, digits=1))
dfTable1Bper

dfTable1B <- data.frame(rbind(dfTable1Btech, dfTable1Bper))
rownames(dfTable1B) <- c("Number", "Percent")
dfTable1B


### Table 2A. Female Components of U.S. U.S. Population  in 2014
(dfTable2Apop <- dfStatesPop3[1,c(2,8:10)])
dfTable2Apop$Male <- dfTable2Apop$totpop - dfTable2Apop$female
dfTable2Apop <- dfTable2Apop[, c(1, 5, 2:4)]
colNames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
colnames(dfTable2Apop) <- colNames
dfTable2Apop <- prettyNum(dfTable2Apop, big.mark = ",")
dfTable2Apop

(dfTable2Aper <- dfStatesPop3[1,c(2,16:18)])
dfTable2Aper[1,1] <- 100.0 ### 100 percent for ALL
dfTable2Aper$per_Male <- round(100 - dfTable2Aper$per_female, digits=1)
dfTable2Aper <- dfTable2Aper[, c(1,5, 2:4)]
colnames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
colnames(dfTable2Aper) <- colNames

dfTable2A <- data.frame(rbind(dfTable2Apop, dfTable2Aper))
rownames(dfTable2A) <- c("Number", "Percent")
dfTable2A


### Table 2B. Male/Female Components of of U.S. Tech Sector in 2014
(dfTable2Bpop <- dfEmploymentAndShares[1,c(2,8:10)])
dfTable2Bpop$male <- dfTable2Bpop$totals - dfTable2Bpop$female
dfTable2Bpop <- dfTable2Bpop[,c(1, 5, 2:4)] ### place males col 2
colnames <- c("ALL", "Male", "Female", "FemAsian", "FemNonAsian")
colnames(dfTable2Bpop) <- colnames
(dfTable2Bpop <- prettyNum(dfTable2Bpop, big.mark = ",") )

### Percentages of Male/Female 
(dfTable2Bper <- dfEmploymentAndShares[1,c(2,16:18)])
(dfTable2Bper$per_male <- 100 - as.numeric(dfTable2Bper$per_female))
dfTable2Bper <- dfTable2Bper[, c(1,5,2:4)] ### males in col 2
dfTable2Bper[1,1] <- 100 ### 100 percent for ALL
colNames_per <- colnames
colnames(dfTable2Bper) <- colNames_per

dfTable2B <- data.frame(rbind(dfTable2Bpop, dfTable2Bper))
rownames(dfTable2B) <- c("Number", "Percent")
dfTable2B



######################
### Table 3 Occupations by Sex ... more thanks to HW
census2OccSex <- group_by(dfCensus2, occupation, sex)
head(census2OccSex)
dfPtsPwtOccSex <- summarise(census2OccSex, ptsPwtOccSex = sum(personalWeight))
head(dfPtsPwtOccSex)
dfOccupationSex <- spread(dfPtsPwtOccSex, key=sex, value=ptsPwtOccSex)
head(dfOccupationSex)
dfOccupationSex$perMale <- round((100 * dfOccupationSex$Male) /(dfOccupationSex$Male + dfOccupationSex$Female), digits=1)
dfOccupationSex$perFemale <- round((100 * dfOccupationSex$Female) /(dfOccupationSex$Male + dfOccupationSex$Female), digits=1)
head(dfOccupationSex)

dfOccupationSex$Total <- dfOccupationSex$Male + dfOccupationSex$Female
dfOccupationSex <- dfOccupationSex[, c(1,6,2:5)] ### Put total in second column
colnames(dfOccupationSex) <- c("occupation", "AllTech", "Male","Female","perMale", "perFemale")
dfOccupationSex <- as.data.frame(dfOccupationSex)
dfOccupationSex

### Add total row for ALL
(techSums <- as.vector(colSums(dfOccupationSex[2:4])))
perMaleTechSums <- as.numeric(round((100 * techSums[2]/techSums[1]), digits = 1))
perFemaleTechSums <- as.numeric(round((100 * techSums[3]/techSums[1]), digits = 1))

dfALL <- data.frame("ALL", t(techSums), perMaleTechSums, perFemaleTechSums) ### note the transpose "t"
colnames(dfALL) <- c("occupation", "AllTech", "Male","Female", "perMale", "perFemale")
dfALL
dfOccupationSex$occupation <- as.character(dfOccupationSex$occupation)
str(dfOccupationSex)
dfOccupationSex <- rbind(dfOccupationSex, dfALL)
dfOccupationSex
str(dfOccupationSex)

### Order by decreasing occupation, drop Female, no row names, etc
index <- order(dfOccupationSex$AllTech, decreasing=TRUE)
dfOccupationSex <- dfOccupationSex[index,] 
rownames(dfOccupationSex) <- NULL
colnames(dfOccupationSex) <- c("Occupation", "AllTech", "Male", "Female","M_%", "F_%")
(dfTable3 <- dfOccupationSex)

save(dfTable1A, dfTable1B, dfTable2A, dfTable2B, dfTable3, file="dfTab1A1B2A2B3.rda")



##################################
######################
### Tables 4A, 4B, 4C, 4D, 4E, 4F Racial, ethnic, female groups in each state  
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show link to full tables in page on git-io

makeTechPopTable <- function(race){
    per_race <- paste0("per_", race)
    pop_race <- paste0("pop_", race)
    dfEmp <- dfEmploymentAndShares[, c("state", "totals", race, per_race)]
    dfPop <- dfStatesPop3[, c("state", race, per_race)]
    ### Example ==> c("state", "black", "per_black")
    
    ### Must change DC name to short form in dfPop before this merge
    dfPop[dfPop$state=="District of Columbia", "state"] <- "Dist of Col"
    dfTechPop <- merge(dfEmp, dfPop, by="state")
    
    raceTech <- paste0(race, "Tech")
    per_raceTech <- paste0("per_", raceTech)
    racePop <- paste0(race, "Pop")
    per_racePop <- paste0("per_", racePop)
    colnames(dfTechPop) <- c("state", "totalTech", raceTech, per_raceTech, racePop, per_racePop)
    ### Example ==> c("state", "totalTech", "blackTech", "per_blackTech", "blackPop", "per_blackPop")
    rownames(dfTechPop) <- c(dfTechPop[,"state"]) 
    
    dfTechPop$parity <- round((dfTechPop[,per_raceTech]/dfTechPop[,per_racePop]), digits=2)
    index <- order(dfTechPop[, raceTech], decreasing=TRUE)
    dfTechPop <- dfTechPop[index,]
    
    ### Calculate the percentage of the total for each race is in each state
    dfTechPop$per_state <- round(100 * dfTechPop[,raceTech]/dfTechPop[1,raceTech[1]], digits=1)
    dfTechPop <- data.frame(dfTechPop[,c(1:2,8,3:7)]) ### move per_state to 3rd column
    return(dfTechPop)
}

dfTechPop_white <- makeTechPopTable("white")
dfTechPop_black <- makeTechPopTable("black")
dfTechPop_hispanic <- makeTechPopTable("hispanic")
dfTechPop_asian <- makeTechPopTable("asian")
dfTechPop_OTHERS <-makeTechPopTable("OTHERS")
dfTechPop_female <-makeTechPopTable("female")
dfTechPop_femAsian <-makeTechPopTable("femAsian")
dfTechPop_femNonAsian <-makeTechPopTable("femNonAsian")

head(dfTechPop_femAsian, 10)
head(dfTechPop_black,10)
head(dfTechPop_white,10)
head(dfTechPop_hispanic,10)
head(dfTechPop_asian,10)
head(dfTechPop_femNonAsian, 10)

dfTable4A <- dfTechPop_white 
dfTable4B <- dfTechPop_black
dfTable4C <- dfTechPop_asian
dfTable4D <- dfTechPop_hispanic 
dfTable4E <- dfTechPop_femAsian
dfTable4F <- dfTechPop_femNonAsian

### Don't display row names on printable copies of tables, same as state names
rownames(dfTable4A) <- NULL
rownames(dfTable4B) <- NULL
rownames(dfTable4C) <- NULL
rownames(dfTable4D) <- NULL
rownames(dfTable4E) <- NULL
rownames(dfTable4F) <- NULL

#################################
################################
### Table 4 ... Big Five
dfTab <- subset(dfTechPop_white, select=c("state","totalTech"))
dfTab <- dfTab[order(dfTab$"totalTech", decreasing=TRUE),]
rownames(dfTab) <- NULL
(dfTab<- (head(dfTab,7)))

sum6 <- sum(dfTab[2:7,2])
perTop6 <- round(100*sum6/dfTab[1,2], digits=1)
vec <- as.vector(c(dfTab[,2], sum6))
(dfTable4 <- data.frame(t(vec), perTop6))
colnames(dfTable4) <- c(dfTab[,1], "SumTop5", "%Top6")
rownames(dfTable4) <- ""
dfTable4

save(dfTable4, dfTable4A, dfTable4B, dfTable4C, dfTable4D, dfTable4E, dfTable4F, dfTechPop_white, dfTechPop_black, dfTechPop_asian, dfTechPop_hispanic, dfTechPop_femAsian, dfTechPop_femNonAsian, file="dfTab4.rda")

#######################################
#######################################
### handy tool for spot checking data
selectParityDF <- function(race, state){
   if (race == "black") {
       return(dfTechPop_black)
   } else {
       if (race =="white") {
           return(dfTechPop_white)
       } else {
           if (race =="hispanic") {
               return(dfTechPop_hispanic)
           } else {
               if (race == "asian") {
                    return(dfTechPop_asian)
               } else {
                   if (race == "female") {
                       return(dfTechPop_female)
                   } else {
                        return (0)
                   }
               }
           }
       }
   }
}

getEmploymentRank <- function(race, state) {
    df <- selectParityDF(race, state)
    if (is.null(dim(df))) { 
        print(paste("Bad race input ... "))
        return(df)
    }
    df <- df[-1,] ### drop the top ALL row
    R <- which(df$state == state) 
    if (length(rank) != 0) {
        return(R)
    } else {
        print(paste("Bad state input"))
    }
}

### Example of use of tool
R <- getEmploymentRank("female", "Washington")
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
(maxAsianPerState <- max(dfTechPop_asian[-1,"per_state"])) ### omit total row
    
makeTechPopMap <- function(df, race, maxPer, title) {
   
    ### Insert dummy max value into District of Columbia, too small to be visible
    df[df$state=="Dist of Col", "per_state"] <- maxPer 
    
    ### and use full name of District, not short form used in these scripts
    df[df$state=="Dist of Col", "state"] <- "District of Columbia"
    
    ###Race <- paste(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race)), sep="") ### capitalize first letter of race ... aarrrrrrrrrrrggghhh!!!
    legend = paste0("%")
    dfMap <- subset(df, select=c("state", "per_state"), state!= c("ALL STATES"))     
    dfMap$state <- tolower(dfMap$state)
    dfMap <- merge(states_map, dfMap, by.x="region", by.y= "state")
    dfMap <- arrange(dfMap, group, order) 
    raceData <- dfMap[,"per_state"]
    
    ggMap <- ggplot(data=dfMap, aes(map_id=region, fill=raceData))
    ggMap <- ggMap + geom_map(map=states_map, colour="black")
    ggMap <- ggMap + scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint= median(raceData))       
    ggMap <- ggMap + expand_limits(x=states_map$long, y=states_map$lat) 
    ggMap <- ggMap + coord_map("polyconic") + labs(fill=legend) + theme_clean()
    ggMap <- ggMap + ggtitle(title) 
    ggMap <- ggMap + guides(fill=guide_legend(title.position = "left"))
    ggmap <- ggMap + theme(legend.title=element_blank(), plot.margin=unit(c(10,10,1,10), "cm")) 
    return(ggMap)
}

(dfMap4A <-makeTechPopMap(dfTechPop_white,"white", maxAsianPerState, "A. White Techs"))
(dfMap4B <-makeTechPopMap(dfTechPop_black,"black", maxAsianPerState, "B. Black Techs"))
(dfMap4C <- makeTechPopMap(dfTechPop_asian,"asian", maxAsianPerState, "C. Asian Techs"))
(dfMap4D <- makeTechPopMap(dfTechPop_hispanic,"hispanic", maxAsianPerState, "D. Hispanic Techs"))
(dfMap4E <- makeTechPopMap(dfTechPop_femAsian,"femAsian", maxAsianPerState, "E. FemAsian Techs"))
(dfMap4F <- makeTechPopMap(dfTechPop_femNonAsian,"femNonAsian", maxAsianPerState, "F. FemNonAsian Techs"))

save(dfMap4A, dfMap4B, dfMap4C, dfMap4D, dfMap4E, dfMap4F, file="dfMap4.rda")

###########################
##########################
### Plots 5 ...
### Run regressions before plots so we can display beta values in upper left of each plot and impose the full regression line on the scatterplot ... smooth_geom only producesshort stubs for some groups

### Regression racial population vs. racial Tech 

makeLM <- function(df, race) {
    ###df <- subset(df, state != "District of Columbia") 
    df <- subset(df, state != "ALL STATES")
    f <- paste0("I(", race, "Tech) ~ I(" , race, "Pop/1000)")
    lm_race <- lm(f, data = df)
    
    f <- paste0("I(per_", race, "Pop) ~ I(per_", race, "Tech)")
    lmModel <- lm(f, data = df)
    return(lm_race)
}

lm_black <- makeLM(dfTechPop_black, "black")
lm_white <- makeLM(dfTechPop_white, "white")
lm_asian <- makeLM(dfTechPop_asian, "asian")
lm_hispanic <-makeLM(dfTechPop_hispanic, "hispanic")
lm_OTHERS <- makeLM(dfTechPop_OTHERS, "OTHERS")
lm_female <- makeLM(dfTechPop_female, "female")
lm_femAsian <- makeLM(dfTechPop_femAsian, "femAsian")
lm_femNonAsian <- makeLM(dfTechPop_femNonAsian, "femNonAsian")

### Save betas for later tables
beta1000 <- c(lm_white$coef[2], lm_black$coef[2], lm_asian$coef[2], lm_hispanic$coef[2], lm_OTHERS$coef[2], lm_female$coef[2], lm_femAsian$coef[2], lm_femNonAsian$coef[2])
names(beta1000) <- c("white", "black", "asian", "hispanic", "OTHERS", "female",  "femAsian", "femNonAsian")
beta1000 <- round(beta1000, digits=2)
beta1000["black"]
beta1000["asian"]
beta1000["femAsian"]
beta1000["female"]


#####################
### Now the plots
###################
plotEmpVsPop <- function(df, race, lmGroup, maxPlotPop, maxPlotTech, alpha){
    
    ### Note: geom_smooth drew short stubby lines for some groups that
    ### had small max populations ... so use geom_abline to draw 
    ### full lines for all groups based on regression slope and intercept
    AB <- summary(lmGroup)$coefficients[,1]
    A <- AB[1] ### intercept of regression line
    B <- AB[2] ### slope of regressionline
    
    racePop <- paste0(race, "Pop")
    raceTech <- paste0(race, "Tech")
    annot <- paste0("Beta = ", beta1000[race])
    Race <- paste(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race)), sep="") ### capitalize first letter of race ... aarrrrrrrrrrrggghhh!!!    
    ### Example ==> aes(df[-1,x=I(df[-1,"blackPop"]/1000), y = df[-1,"blackTech"]
    ggScatter <- ggplot(df[-1,], aes(x=I(df[-1,racePop]/1000), y=df[-1,raceTech])) + geom_point(shape=1) 
    ggScatLine <- ggScatter + xlim(0, maxPlotPop) + ylim(0, maxPlotTech)
    ggScatLine <- ggScatLine + geom_abline(intercept=A, slope=B, colour="blue",size=0.8)
    
    ### Example ==> xlab("blackPop/1000) + ylab("blackTech")
    ggScatLine <- ggScatLine + xlab("Pop/1000") + ylab("Tech")
    ggScatLine <- ggScatLine + ggtitle(paste0(alpha, " ", Race," -- Tech vs Pop/1000")) + theme(plot.title = element_text(size=12))
    ggScatLine <- ggScatLine + annotate("text", label=annot, x=-Inf, y=Inf, hjust=-.2, vjust=2)
    return(ggScatLine)
}

(maxPlotTech <- max(dfEmploymentAndShares[-1, c(3:6)]))

### mqx value for white, black, asian, hispanic, femAsian, femNonAsian
(maxPlotPop <- max(dfStatesPop3[-1,c(3:6,9:10)])/1000)

(ggPlot_white <- plotEmpVsPop(dfTechPop_white, "white", lm_white, maxPlotPop, maxPlotTech, "A."))
(ggPlot_black <- plotEmpVsPop(dfTechPop_black, "black", lm_black, maxPlotPop, maxPlotTech, "B. ")) 
(ggPlot_asian <- plotEmpVsPop(dfTechPop_asian, "asian", lm_asian, maxPlotPop, maxPlotTech, "C."))
(ggPlot_hispanic <- plotEmpVsPop(dfTechPop_hispanic, "hispanic", lm_hispanic, maxPlotPop, maxPlotTech, "D."))
(ggPlot_femAsian <- plotEmpVsPop(dfTechPop_femAsian, "femAsian", lm_femAsian, maxPlotPop, maxPlotTech, "E.")) 
(ggPlot_femNonAsian <- plotEmpVsPop(dfTechPop_femNonAsian, "femNonAsian", lm_femNonAsian, maxPlotPop, maxPlotTech, "F."))


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
    
    ### Sort by betas decreasing order
    ### index <- order(mat[, "beta1000"], decreasing=TRUE)
    ### mat <- mat[index,]
    
    dfTable <- as.data.frame(mat)
    return(dfTable)
}

raceList <- list(dfTechPop_white, dfTechPop_black, dfTechPop_asian, dfTechPop_hispanic, dfTechPop_OTHERS, dfTechPop_female, dfTechPop_femAsian, dfTechPop_femNonAsian)
names(raceList) <- c("White", "Black", "Asian", "Hispanic", "OTHERS", "Female", "FemAsian", "FemNonAsian")
(dfTable5 <- makeSummary(raceList, beta1000))


save(ggPlot_asian, ggPlot_white, ggPlot_black, ggPlot_hispanic, ggPlot_femAsian, ggPlot_femNonAsian, beta1000, dfTable5, file="dfPlot5Tab5beta1000.rda")

#######################################
######################
#### Conclusions
### Table 6. Stats for parity variable of racial groups in each state  
makeParity <- function(listDFs){
    ng <- length(listDFs)
    matParity <- matrix(NA, nrow=ng, ncol = 6)
    dfP <- data.frame(matParity)
    colnames(dfP) <- c("min", "Q1", "med", "mean", "Q3", "max")
    for (i in 1:ng) {
        df<- listDFs[[i]]
        dfP[i,] <- summary(df$parity)
        
        ### Change min to minimum value > 0 ... Census said NA because not enough
        ### observations in sample to estimate techs in some small states
        ### ... my code converted NA's to 0s for Techs ==> 0s for percent Techs
        dfP[i,1] <- min(df[df[,"parity"]> 0, "parity"])
    }
    dfP <- subset(dfP, select=-c(mean)) ### Drop mean values
    dfP <- round(dfP, digits=2)
    rownames(dfP) <- names(listDFs)
    return(dfP)
}

listDFs <- list(dfTechPop_white, dfTechPop_black, dfTechPop_asian, dfTechPop_hispanic, dfTechPop_female, dfTechPop_femAsian, dfTechPop_femNonAsian)
names(listDFs) <- c("white",  "black", "asian", "hispanic",  "female",  "femAsian", "femNonAsian")
(dfParity <- makeParity(listDFs))

dfTable6 <- dfParity
rownames(dfTable6) <- c("White", "Black", "Asian", "Hispanic", "Female", "FemAsian","FemNonAsian") ### Caps on 1st letters for inclusion in report
(dfTable6)

##################################
####### Tables 7 ... Finalists for black, hispanic, femAsians, and femNonAsians
makeTable7 <- function(dfIn, dfParity, race, letter) {
    dfFinal <- subset(dfIn[2:11,], parity >= dfParity[race, "med"]) 
    perRaceTech <- paste0("per_",race, "Tech")
    Race <- paste(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race)), sep="") ### capitalize first letter of race ... aarrrrrrrrrrrggghhh!!! 
    
    dfOut <- subset(dfFinal, select=c("state", perRaceTech, "parity"))
    L <- dim(dfOut)[1]
    L <- min(L,5)
    dfOut <- head(dfOut[order(-dfOut$parity),],L)
    rows <- as.character(seq(1:L)) ### some tables may be shorter than 5
    rownames(dfOut) <- rows
    tabName <- paste0("7", letter, ". ", Race)
    colnames(dfOut) <- c(tabName, "Tech", "Parity")

    return(dfOut)
}
### Tables 7 ... sorted by parity
(dfTable7A <- makeTable7(dfTechPop_white , dfParity, "white", "A"))
(dfTable7B <- makeTable7(dfTechPop_black , dfParity, "black", "B"))
(dfTable7C <- makeTable7(dfTechPop_asian , dfParity, "asian", "C"))
(dfTable7D <- makeTable7(dfTechPop_hispanic , dfParity, "hispanic", "D"))
(dfTable7E <- makeTable7(dfTechPop_femAsian , dfParity, "femAsian", "E"))
(dfTable7F <- makeTable7(dfTechPop_femNonAsian , dfParity, "femNonAsian","F"))


### Sorted by tech share of the info tech sector 
makeTable8 <- function(dfIn, Race, letter){
    vec <- dfIn[, "Tech"] ###dfIn[,perRaceTech]
    dfOut <- dfIn[order(-vec),]

    L <- dim(dfOut)[1]
    rows <- as.character(seq(1:L)) ### some tables may be shorter than 5
    rownames(dfOut) <- rows
    tabName <- paste0("8", letter, ". ", Race)
    colnames(dfOut) <- c(tabName, "Tech", "Parity")
    return(dfOut)
}

(dfTable8A <- makeTable8(dfTable7A,"White", "A")) ###, "White"
(dfTable8B <- makeTable8(dfTable7B, "Black", "B")) ###, "Black"
(dfTable8C <- makeTable8(dfTable7C, "Asian", "C")) ###, "Asian"
(dfTable8D <- makeTable8(dfTable7D, "Hispanic", "D")) ###, "Hispanic"
(dfTable8E <- makeTable8(dfTable7E, "FemAsian", "E")) ###, "FemAsian"
(dfTable8F <- makeTable8(dfTable7F, "FemNonAsian", "F")) ###, "FemNonAsian"

save(dfTable6, dfTable7A, dfTable7B, dfTable7C, dfTable7D, dfTable7E, dfTable7F, dfTable8A, dfTable8B, dfTable8C, dfTable8D, dfTable8E, dfTable8F, file="dfTab67A7B7C7D7E7F8A8B8C8D8E8F.rda")
