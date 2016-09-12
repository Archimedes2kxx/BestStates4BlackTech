### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

###################################################
### use map_dat() as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
###################################################

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
load(file="dfEmploymentAndShares.RData")
load(file="dfStatesPop3.RData") 
load("dfCensus2.RData")

### install.packages("tidyr")
### install.packages("maps")
### install.packages("ggplot2")
### install.packages("dplyr") 
### install.packages("mapproj")

library(tidyr)
library(maps)
library(mapproj) ### needed by ggplot2, but not install automaticallly
library(ggplot2)
library(dplyr)

### Table 1.1  Context -- How many people in the US in 2014 -- total, black, white, ### asian, hispanic, and OTHERS and percentages of total, white, black, asian, hispanic, and OTHERS
table1p1top <- dfStatesPop3[52,2:7]
colNames <- c("ALL", "white", "black", "asian", "hispanic", "OTHERS")
colnames(table1p1top) <- colNames
dfTable1p1top <- data.frame(table1p1top)
rownames(dfTable1p1top) <- "Totals"
dfTable1p1top

table1p1bottom <- dfStatesPop3[52,c(2,8:12)]
table1p1bottom <- as.vector(table1p1bottom)
table1p1bottom[1,1] <- 1.0 ### 1.0 = 100 percent for ALL
colnames(table1p1bottom) <- colNames
dfTable1p1bottom <- round(data.frame(table1p1bottom), digits=3) * 100
rownames(dfTable1p1bottom) <- "Percent"
dfTable1p1bottom

### Table 1.2 ... racial breakdown of tech employment
table1p2top <- dfEmploymentAndShares[52,2:7]
colNames <- c("ALL", "white", "black", "asian", "hispanic", "OTHERS")
colnames(table1p2top) <- colNames
dfTable1p2top <- data.frame(table1p2top)
rownames(dfTable1p2top) <- "Totals"
dfTable1p2top

table1p2bottom <- dfEmploymentAndShares[52,c(2,8:12)]
table1p2bottom <- as.vector(table1p2bottom)
table1p2bottom[1,1] <- 1.0 ### 1.0 = 100 percent for ALL
colnames(table1p2bottom) <- colNames
dfTable1p2bottom <- round(data.frame(table1p2bottom), digits=3) * 100
rownames(dfTable1p2bottom) <- "Percent"
dfTable1p2bottom

### Table 1.3 Sex by Occupations ... two rows
censusGroups <- group_by(dfCensus2, occupation, sex)
dfPtsPerSex <- summarise(censusGroups, ptsPerSex = sum(personalWeight))
dfOccupationPerSex <- spread(dfPtsPerSex, key=sex, value=ptsPerSex)
dfOccupationPerSex <- data.frame(dfOccupationPerSex)
dfOccupationPerSex$perMale <- round(dfOccupationPerSex$Male /(dfOccupationPerSex$Male + dfOccupationPerSex$Female), digits=3) * 100

dfOccupationPerSex$Total <- dfOccupationPerSex$Male + dfOccupationPerSex$Female
dfOccupationPerSex <- dfOccupationPerSex[, c(1,5,2:4)] ### Put total in second column
### dfOccupationPerSex

### Add total row for ALL
techSums <- as.vector(colSums(dfOccupationPerSex[2:4]))
perMaleTechSums <- as.numeric(round((techSums[2]/techSums[1]), digits = 3) * 100)
dfALL <- data.frame("ALL", t(techSums), perMaleTechSums)
colnames(dfALL) <- c("occupation", "Total", "Male","Female", "perMale")
dfOccupationPerSex <- rbind(dfOccupationPerSex, dfALL)
nRows <- dim(dfOccupationPerSex)[1]
dfOccupationPerSex

### Finish making things "nice"
index <- order(dfOccupationPerSex$Total, decreasing=TRUE)
dfOccupationPerSex <- dfOccupationPerSex[index,] 
dfOccupationPerSex$Female <- NULL
rownames(dfOccupationPerSex) <- NULL
colnames(dfOccupationPerSex) <- c("occupation", "Total", "Male", "%-Male")
dfOccupationPerSex


### Tables 2A, 2B, 2C, 2D. Racial groups in each state  
### ... state, <racial>TechEmp, totalTechEmp, per<Racial>TechEmp, totPop, <racialPop>, 
###        per<Racial>Pop, <racial>Parity ... 
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show full tables 2WW, 2BB, 2AA, 2HH in appendices on GitHub

makeParityTable <- function(race){
    per_race <- paste0("per_", race)
    pop_race <- paste0("pop_", race)
    
    dfEmp <- dfEmploymentAndShares[, c("state", "totals", race, per_race)]
    dfPop <- dfStatesPop3[, c("state", race, per_race)]
    dfParity <- merge(dfEmp, dfPop, by="state")
    
    raceTech <- paste0(race, "Tech")
    per_raceTech <- paste0("per_", raceTech)
    racePop <- paste0(race, "Pop")
    per_racePop <- paste0("per_", racePop)
    colnames(dfParity) <- c("state", "totalTech", raceTech, per_raceTech, racePop, per_racePop)
    dfParity$parity <- round((dfParity[,per_raceTech]/dfParity[,per_racePop]), digits=3)
    
    index <- order(dfParity[, raceTech], decreasing=TRUE)
    dfParity <- dfParity[index,]
    return(dfParity)
}

dfParity_black <- makeParityTable("black")
dfParity_white <- makeParityTable("white")
dfParity_hispanic <- makeParityTable("hispanic")
dfParity_asian <- makeParityTable("asian")

head(dfParity_black,20)
head(dfParity_white,20)
head(dfParity_hispanic,20)
head(dfParity_asian,20)
tail(dfParity_asian,20)

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
    if (is.null(dim(df))) { ####### NOT WORKING
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


### Maps 2A, 2B, 2C, 2D ... maps of white, black, asian, hispanics in state tech sectors
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
        ###axis.ticks.length = unit(0, "cm"),
        ###axis.ticks.margin = unit(0, "cm"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        complete = TRUE
    )
}
    
makeTechMap <- function(df, race) {
    raceTech <- paste0(race,"Tech")
    legend <- paste(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race)), sep="") ### capitalize first letter ... aarrrrrrrrrrrggghhh!!!
    dfMap <- subset(df, select=c("state", raceTech), state!= c("ALL STATES"))  
    dfMap$state <- tolower(dfMap$state)
    dfMap <- merge(states_map, dfMap, by.x="region", by.y= "state")
    dfMap <- arrange(dfMap, group, order) 
    raceData <- dfMap[,raceTech]

    ggMap <- ggplot(data=dfMap, aes(map_id=region, fill=raceData))
    ggMap <- ggMap + geom_map(map=states_map, colour="black")
    ggMap <- ggMap + scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint= median(raceData))       
    ggMap <- ggMap + expand_limits(x=states_map$long, y=states_map$lat) 
    ggMap <- ggMap + coord_map("polyconic") + labs(fill=legend) + theme_clean()
    
    return(ggMap)
}

black_ggMap<-makeTechMap(dfParity_black,"black")
white_ggMap <-makeTechMap(dfParity_white,"white")
hispanic_ggMap <- makeTechMap(dfParity_hispanic,"hispanic")
asian_ggMap <- makeTechMap(dfParity_asian,"asian")

black_ggMap
white_ggMap
hispanic_ggMap
asian_ggMap

### Tables 2.1A, 2.1B, 2.1C, 2.1D. summary stats for racial groups in each state  
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

dfParity <- as.data.frame(matParity)
dfParity


### Plots 2A, 2B, 2C, 2D ... regression racial population vs. racial Tech 
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
dfParity <- dfParity[c("hispanic", "black", "white", "asian"),]
dfParity

### Prefer to scatterplot via ggplot  
pred_black <- predict(lm_black)
regLine_black <- data.frame(dfParity_black$blackPop[-1], pred_black)
colnames(regLine_black) <- c("blackPop", "blackTech")

ggScatter_black <- ggplot(dfParity_black[-1,], aes(x=I(blackPop/1000), y=I(blackTech), colour="black")) + geom_point(shape=1) + 
    scale_colour_hue(l=50) # Use a slightly darker palette than normal
ggLine_black <- ggScatter_black + geom_line(data=regLine_black, size=1)
ggLine_black

pred_asian <- predict(lm_asian)
regLine_asian <- data.frame(dfParity_asian$asianPop[-1], pred_asian)
colnames(regLine_asian) <- c("asianPop", "asianTech")

ggScatter_asian <- ggplot(dfParity_asian[-1,], aes(x=I(asianPop/1000), y=I(asianTech), colour="green")) + geom_point(shape=1) + 
    scale_colour_hue(l=50) # Use a slightly darker palette than normal
ggLine_asian <- ggScatter_asian + geom_line(data=regLine_asian, size=1)
ggLine_asian


head(regLine_black)
head(dfParity_black$blackPop[-1])


### Question: What are the best states for Asians in tech?
### Answer:   All of them ... :-)

