### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

################################################
### use map_dat() as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################
  

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
load(file="dfEmploymentAndShares.rda")
load(file="dfStatesPop3.rda") 
load("dfCensus2.rda")  
 
### install.packages("tidyr")
### install.packages("maps")
### install.packages("ggplot2")
### install.packages("dplyr") 
### install.packages("mapproj")

library(tidyr)
library(maps)
library(mapproj) ### needed by ggplot2, but not installed automaticallly
library(ggplot2)
library(maps)
library(dplyr)
library(grid)
library(gridExtra)

### Table 1A   Context -- How many people were living in the US in 2014 -- total, black, white, ### asian, hispanic, and OTHERS 
(dfTable1A <- dfStatesPop3[1,2:7])
colNames <- c("ALL", "White", "Black", "Asian", "Hispanic", "OTHERS")
colnames(dfTable1A) <- colNames
dfTable1A

### Percentages of total, white, black, asian, hispanic, and OTHERS
dfTable1B <- dfStatesPop3[1,c(2,8:12)]
dfTable1B[1,1] <- 100 ### 100 percent for ALL
colNames_per <- c("per_white", "per_black", "per_asian", "per_hispanic", "per_OTHERS")
colnames(dfTable1B) <- colNames
dfTable1B <- round(dfTable1B, digits=1)
dfTable1B

### Table 2 ... racial breakdown of tech employment
dfTable2A <- dfEmploymentAndShares[1,2:7]
colnames(dfTable2A) <- colNames
dfTable2A

dfTable2B <- dfEmploymentAndShares[1,c(2,8:12)]
dfTable2B[1,1] <- 100 ### 100 percent for ALL
colnames(dfTable2B) <- colNames_per
rownames(dfTable2B) <- NULL
dfTable2B <- round(data.frame(dfTable2B), digits=1)
dfTable2B


### Table 3 Occupations by Sex
census2OccSex <- group_by(dfCensus2, occupation, sex)
head(census2OccSex)
dfPtsPwtOccSex <- summarise(census2OccSex, ptsPwtOccSex = sum(personalWeight))
head(dfPtsPwtOccSex)
dfOccupationSex <- spread(dfPtsPwtOccSex, key=sex, value=ptsPwtOccSex)
head(dfOccupationSex)
dfOccupationSex$perMale <- round((100 * dfOccupationSex$Male) /(dfOccupationSex$Male + dfOccupationSex$Female), digits=1)
head(dfOccupationSex)

dfOccupationSex$Total <- dfOccupationSex$Male + dfOccupationSex$Female
dfOccupationSex <- dfOccupationSex[, c(1,5,2:4)] ### Put total in second column
colnames(dfOccupationSex) <- c("occupation", "Total", "Male","Female", "perMale")
dfOccupationSex <- as.data.frame(dfOccupationSex)
dfOccupationSex

### Add total row for ALL
techSums <- as.vector(colSums(dfOccupationSex[2:4]))
perMaleTechSums <- as.numeric(round((100 * techSums[2]/techSums[1]), digits = 1))
dfALL <- data.frame("ALL", t(techSums), perMaleTechSums) ### note the transpose "t"
colnames(dfALL) <- c("occupation", "Total", "Male","Female", "perMale")

dfOccupationSex$occupation <- as.character(dfOccupationSex$occupation)
str(dfOccupationSex)
dfOccupationSex <- rbind(dfOccupationSex, dfALL)
dfOccupationSex
str(dfOccupationSex)

### Order by decreasing emploment, drop Female, no row names, etc
index <- order(dfOccupationSex$Total, decreasing=TRUE)
dfOccupationSex <- dfOccupationSex[index,] 
dfOccupationSex$Female <- NULL
rownames(dfOccupationSex) <- NULL
colnames(dfOccupationSex) <- c("Occupation", "Total", "Male", "%-Male")
(dfTable3 <- dfOccupationSex)

save(dfTable1A, dfTable1B, dfTable2A, dfTable2B, dfTable3, file="dfTab1A1B2A2B3.rda")

### Tables 4A, 4B, 4C, 4D. Racial groups in each state  
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show full tables linked to report on git-io

makeParityTable <- function(race){
    per_race <- paste0("per_", race)
    pop_race <- paste0("pop_", race)
    dfEmp <- dfEmploymentAndShares[, c("state", "totals", race, per_race)]
    dfPop <- dfStatesPop3[, c("state", race, per_race)]
    
    ### Must change DC name to short form in dfPop before this merge
    dfPop[dfPop$state=="District of Columbia", "state"] <- "Dist of Col"
    
    dfParity <- merge(dfEmp, dfPop, by="state")
    
    raceTech <- paste0(race, "Tech")
    per_raceTech <- paste0("per_", raceTech)
    racePop <- paste0(race, "Pop")
    per_racePop <- paste0("per_", racePop)
    colnames(dfParity) <- c("state", "totalTech", raceTech, per_raceTech, racePop, per_racePop)
    rownames(dfParity) <- c(dfParity[,"state"]) 
    
    dfParity$parity <- round((dfParity[,per_raceTech]/dfParity[,per_racePop]), digits=2)
    index <- order(dfParity[, raceTech], decreasing=TRUE)
    dfParity <- dfParity[index,]
    
    ### Calculate the percentage of the total for each race is in each state
    dfParity$per_state <- round(100 * dfParity[,raceTech]/dfParity[1,raceTech[1]], digits=1)
    dfParity <- data.frame(dfParity[,c(1:2,8,3:7)]) ### move per_state to 3rd column
    return(dfParity)
}

dfParity_white <- makeParityTable("white")
dfParity_black <- makeParityTable("black")
dfParity_hispanic <- makeParityTable("hispanic")
dfParity_asian <- makeParityTable("asian")

head(dfParity_black,10)
head(dfParity_white,10)
head(dfParity_hispanic,20)
head(dfParity_asian,20)
tail(dfParity_asian,20)

save(dfParity_white, dfParity_black, dfParity_asian, dfParity_hispanic, file="dfTab4.rda")

### save(dfTable4A, dfTable4B, dfTable4C, dfTable4D, dfParity_white, dfParity_black, dfParity_asian, dfParity_hispanic, file="dfTab4.rda")

############
### handy tool for spot checking data
selectParityDF <- function(race, state){
   if (race == "black") {
       return(dfParity_black)
   } else {
       if (race =="white") {
           return(dfParity_white)
       } else {
           if (race =="hispanic") {
               return(dfParity_hispanic)
           } else {
               if (race == "asian") {
                    return(dfParity_asian)
               } else {
                   return (0)
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

R <- getEmploymentRank("black", "California")
R       


### Maps 4A, B, C, D ... state maps of white, black, asian, hispanics in  tech 
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

maxAsianPerState <- max(dfParity_asian[-1,"per_state"]) ### omit total row
maxAsianPerState 
    
makeTechMap <- function(df, race, maxPer, title) {
    ### raceTech <- paste0(race,"Tech")
    
    ### These maps will only show the lower 48 and DC; they don't process values for Hawaii and Alska
    ### DC is so small that its colors are invisible. We want all the colors 
    ### on the maps to be comparable. So if we place the same max value into
    ### DC, it won't be visible ... but all other states will be colored
    ### against this maximum
    ### Observation and calculation shows that the highest concentration value
    ### for any group was the Asian 29.1 value for California
    ###  ... so place this max into DC for all groups, including Asians
    
    ### Insert dummy max value into District of Columbia
    df[df$state=="Dist of Col", "per_state"] <- maxPer 
    
    ### and use full name of District, not short form used in these scripts
    df[df$state=="Dist of Col", "state"] <- "District of Columbia"
    
    legend <- paste(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race)), sep="") ### capitalize first letter of race ... aarrrrrrrrrrrggghhh!!!
    legend = paste0(legend, " %")
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
    ggmap <- ggMap + theme(legend.title=element_blank(), plot.margin=unit(c(1,1,1,1), "cm")) 
    return(ggMap)
}

(black_ggMap<-makeTechMap(dfParity_black,"black", maxAsianPerState, "Map B -- Black Techs"))
(white_ggMap <-makeTechMap(dfParity_white,"white", maxAsianPerState, "Map A -- White Techs"))
(hispanic_ggMap <- makeTechMap(dfParity_hispanic,"hispanic", maxAsianPerState, "Map D -- Hispanic Techs"))
(asian_ggMap <- makeTechMap(dfParity_asian,"asian", maxAsianPerState, "Map C -- Asian Techs"))

dfMap4A <- white_ggMap
dfMap4B <- black_ggMap
dfMap4C <- asian_ggMap
dfMap4D <- hispanic_ggMap

save(dfMap4A, dfMap4B, dfMap4C, dfMap4D, file="dfMap4.rda")

##########################
### Table 5. summary stats for racial groups in each state  
summary(dfParity_asian$parity) 
summary(dfParity_white$parity)
summary(dfParity_black$parity)
summary(dfParity_hispanic$parity)

matParity <- matrix(NA, nrow=4, ncol = 6)
rownames(matParity) <- c("black", "white", "hispanic", "asian")
colnames(matParity) <- c("min", "Q1", "median", "mean", "Q3", "max")
matParity["black",] <- summary(dfParity_black$parity)
matParity["white",] <- summary(dfParity_white$parity)
matParity["hispanic",] <- summary(dfParity_hispanic$parity)
matParity["asian",] <- summary(dfParity_asian$parity)
matParity

### Zeros in original ACS PLUMS data merely meant not enough cases in a sample
### So zero minimums are not informative. Replace 0s with min values above 0
matParity["black", "min"]<-head(min(dfParity_black[dfParity_black[,"parity"]>0,"parity"]))
matParity["white", "min"]<-head(min(dfParity_white[dfParity_white[,"parity"]>0,"parity"]))
matParity["hispanic","min"]<-head(min(dfParity_hispanic[dfParity_hispanic[,"parity"]>0,"parity"]))
matParity["asian", "min"]<-head(min(dfParity_asian[dfParity_asian[,"parity"]>0,"parity"]))
matParity

dfParity <- as.data.frame(matParity)
dfParity

##########################
### Regression racial population vs. racial Tech 
makeLM <- function(df, race) {
    ###df <- subset(df, state != "District of Columbia") 
    df <- subset(df, state != "ALL STATES")
    f <- paste0("I(", race, "Tech) ~ I(" , race, "Pop/1000)")
    lm_race <- lm(f, data = df)
    pred <- paste0(race, "Pop")
    resp <- paste0(race, "Tech")
    
    f <- paste0("I(per_", race, "Pop) ~ I(per_", race, "Tech)")
    lmModel <- lm(f, data = df)
    return(lm_race)
}

lm_black <- makeLM(dfParity_black, "black")
lm_white <- makeLM(dfParity_white, "white")
lm_hispanic <-makeLM(dfParity_hispanic, "hispanic")
lm_asian <- makeLM(dfParity_asian, "asian")

beta1000 <- c(lm_black$coef[2], lm_white$coef[2], lm_hispanic$coef[2], lm_asian$coef[2])
dfParity <- round(cbind(dfParity, beta1000), digits=2)
dfParity <- dfParity[c("hispanic", "black", "white", "asian"),] ### reorder the rows
rownames(dfParity) <- c("hispanic", "black", "white", "asian")
(dfTable5 <- dfParity)
rownames(dfTable5) <- c("Hispanic", "Black", "White", "Asian")
save(dfTable5, dfParity, file="dfTab5.rda")

### Calculate max values for plots (below)
###############
blackMax <- c(max(dfParity_black[-1,]$blackTech), max(dfParity_black[-1,]$blackPop))
whiteMax <- c(max(dfParity_white[-1,]$whiteTech), max(dfParity_white[-1,]$whitePop))
hispanicMax <- c(max(dfParity_hispanic[-1,]$hispanicTech), max(dfParity_hispanic[-1,]$hispanicPop))
asianMax <- c(max(dfParity_asian[-1,]$asianTech), max(dfParity_asian[-1,]$asianPop))

matMaxVals <- matrix(NA, nrow=4, ncol = 2)
rownames(matMaxVals) <- c("black", "white", "hispanic", "asian")
colnames(matMaxVals) <- c("Tech", "Pop")
matMaxVals["black",] <- blackMax
matMaxVals["white",] <- whiteMax
matMaxVals["hispanic",] <- hispanicMax
matMaxVals["asian",] <- asianMax

### Max values overall are the max values for whites, of course, but the general code may be useful at some later date ... :-()
(maxTech <- max(matMaxVals[,"Tech"]))
(maxPop <- max(matMaxVals[,"Pop"])/1000)

### Add a phony extra point to the black data frame to extrapolate the regression line to the end of
### the plot frame ... otherwise it's too short and stubby to compare its slope to the other lines
df_maxPop <- data.frame(maxPop * 1000)
colnames(df_maxPop) <- "blackPop"
y_black_ex <- predict(lm_black, df_maxPop)
dfEx_black <- data.frame(dfParity_black[,"blackPop"], dfParity_black[,"blackTech"])
colnames(dfEx_black) <- c("blackPop", "blackTech")
dfEx_black <- rbind(dfEx_black, c(maxPop * 1000, y_black_ex))

###################
plotEmpVsPop <- function(df, race){
    racePop <- paste0(race, "Pop")
    racePopLab <- paste0(racePop, "/1000")
    raceTech <- paste0(race, "Tech")
    annot <- paste0("Beta = ", dfParity[race, "beta1000"])
    
    ### Example: aes(df[-1,x=I(df[-1,"blackPop"]/1000), y = df[-1,"blackTech"]
    ggScatter <- ggplot(df[-1,], aes(x=I(df[-1,racePop]/1000), y=df[-1,raceTech])) + geom_point(shape=1) 
    ggScatLine <- ggScatter + stat_smooth(method=lm, se=FALSE) + xlim(0, maxPop) + ylim(0, maxTech)
    
    ### Example: xlab("blackPop/1000) + ylab("blackTech")
    ggScatLine <- ggScatLine + xlab(racePopLab) + ylab(raceTech)
    ggScatLine <- ggScatLine + ggtitle(paste0(raceTech, " vs ", racePopLab))
    ggScatLine <- ggScatLine + annotate("text", label=annot, x=-Inf, y=Inf, hjust=-.2, vjust=2)
    return(ggScatLine)
}

(ggPlot_asian <- plotEmpVsPop(dfParity_asian, "asian"))
(ggPlot_white <- plotEmpVsPop(dfParity_white, "white"))
(ggPlot_black <- plotEmpVsPop(dfEx_black, "black"))
(ggPlot_hispanic <- plotEmpVsPop(dfParity_hispanic, "hispanic"))

save(ggPlot_asian, ggPlot_white, ggPlot_black, ggPlot_hispanic, file="ggPlot6.rda")

