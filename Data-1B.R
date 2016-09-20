### B. Read Amdrican Fact Finder (AFF) parameters about racial population of states from AFF pages for 2014 
###    Compute percent white, black, asian, hispanic, and OTHERS

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

### 1. Read data ... American Fact finder URL ... table PEPSR6H
### http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=PEP_2015_PEPSR6H&prodType=table
file = "PEP_2014_PEPSR6H_with_ann.csv"
dfStatesPop1 <- read.csv(file, header=FALSE, stringsAsFactors = FALSE)

### Get var names 
varNames <- as.vector(unlist(dfStatesPop1[1,]))
colnames(dfStatesPop1) <- varNames

### Drop display labels and var names in 1st, 2nd rows
dfStatesPop1 <- dfStatesPop1[c(-1, -2),]

### 2. Get data for all non-hispanics
dfNonHispanics <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="totsex"& Hisp.id=="nhisp", select=c(wa:tom))
dfNonHispanics$totpop <- NULL

### 3. Get data for hispanics
dfHispanics <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="totsex"& Hisp.id=="hisp", select=c(totpop))
dfHispanics$hisp <- dfHispanics$totpop
dfHispanics$totpop <- NULL

### 4. Get total pop of the state
dfTotPop <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id=="totsex" & Hisp.id=="tothisp", select=c(`GEO.display-label`, totpop))

### 5. cbind the columns
dfStatesPop2 <- cbind(dfTotPop, dfNonHispanics, dfHispanics)

### 6. Confer short names on all variables
columnNames <- c("state", "totpop", "white", "black", "amInAlNat", "asian", "pacific", "mixed" , "hispanic")
colnames(dfStatesPop2) <- columnNames

### 7. Convert state to factor
dfStatesPop2$state <- as.factor(dfStatesPop2$state)

### 8. Convert data columns to integers
vec <- as.integer(unlist(dfStatesPop2[,2:9]))
mat <- matrix(vec, nrow=51, ncol=8)
dfStatesPop2 <- data.frame(dfStatesPop2[,1], mat)
colnames(dfStatesPop2) <- columnNames ### ... yes, again

### 9. Combine races that will not be analyzed, then delete their columns
dfStatesPop2$OTHERS <- dfStatesPop2$amInAlNat + dfStatesPop2$pacific + dfStatesPop2$mixed
dfStatesPop2$amInAlNat <- NULL
dfStatesPop2$pacific <- NULL
dfStatesPop2$mixed <- NULL

### 10. Calculate derived parameters ... percentage of each racial group in the total population ... add to data frame
dfStatesPop3 <- dfStatesPop2
dfStatesPop3$per_white <- round(100* dfStatesPop3$white / dfStatesPop3$totpop, digits = 1)
dfStatesPop3$per_black <- round(100 * dfStatesPop3$black / dfStatesPop3$totpop, digits = 1)
dfStatesPop3$per_asian <- round(100 * dfStatesPop3$asian / dfStatesPop3$totpop, digits = 1)
dfStatesPop3$per_hispanic <- round(100 * dfStatesPop3$hisp / dfStatesPop3$totpop, digits = 1)
dfStatesPop3$per_OTHERS <- round(100 * dfStatesPop3$OTHERS / dfStatesPop3$totpop, digits = 1)

### 11. Add a totals row and save
allRacesInUS <- colSums(dfStatesPop3[,3:7])
allRaces <- sum(allRacesInUS) ### 4125164
raceSharesInUS <- round(100 * allRacesInUS/allRaces, digits=1)

dfTotalsRow <- dfStatesPop3[1,] ### dummy copy to get columns and types
dfTotalsRow$state <- "ALL STATES"
dfTotalsRow[1,3:7] <- allRacesInUS
dfTotalsRow[1,8:12] <- raceSharesInUS
dfTotalsRow[1,2] <- allRaces
dfStatesPop3 <- rbind( dfTotalsRow, dfStatesPop3)

head(dfStatesPop3)
tail(dfStatesPop3)
save(dfStatesPop3, file = "dfStatesPop3.rda")

### 12. Examples of state AFF parameters ... must add rownames
dfStatesPop4 <- dfStatesPop3
rownames(dfStatesPop4) <- c(dfStatesPop4$state)
dfStatesPop4["California",]
sum(dfStatesPop4["California", 3:10])

dfStatesPop4["New York",]
sum(dfStatesPop4["New York", 3:10])

### Note the URL for Census data about racial shares of total U.S. pop
### https://www.census.gov/quickfacts/table/PST045215/00  

