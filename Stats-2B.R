### Stat 2-B ... Maps and Plots

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
    dfTechPop$perState <- round(100 * dfTechPop[,RaceTech]/dfTechPop[1,RaceTech[1]], digits=1)
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
    dfTech$perState <- round(100 * dfTech[,AreaTech]/dfTech[1,AreaTech[1]], digits=1)
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
### Maps 4A, B, C, D, E, F, G ... state maps of white, black, asian, hispanic, female Asians, and female non-Asians in  tech 
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
(dfMap4H <- makeTechPopMap(dfTable4H,"Asia", maxAsianPerState, "G. Foreign (Not Asia) Techs"))

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

save(dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian, file="dfTechPop.rda")
