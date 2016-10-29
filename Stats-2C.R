### Stat 2-C ... Best States

################################################
################################################ THurs 9/29 @ 5"12 pm
### Use map_dat() in ggplot2 as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################

###load(file="dfRaceSexCountAndShares.rda")
###load(file="dfStatesPop3.rda") 
###load("dfCensus3.rda")
###load("dfCensus2.rda")
###load("dfCensus2.2010.rda")
###load("dfCensus3.2010.rda")

load("dfPlot5Tab5beta1000.rda")
load("dfTab4.rda")
load("dfTechPop.rda")

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
