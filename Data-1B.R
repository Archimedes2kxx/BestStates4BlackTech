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

### 9. Add row names = states .... convenient for subsetting with brackets
rownames(dfStatesPop2) <- dfStatesPop2$state

### 10. Combine races that will not be analyzed, then delete their columns
dfStatesPop2$OTHERS <- dfStatesPop2$amInAlNat + dfStatesPop2$pacific + dfStatesPop2$mixed
dfStatesPop2$amInAlNat <- NULL
dfStatesPop2$pacific <- NULL
dfStatesPop2$mixed <- NULL

### 11. Calculate derived parameters ... percentage of each racial group in the total population ... add to data frame
dfStatesPop3 <- dfStatesPop2
dfStatesPop3$per_white <- round(dfStatesPop3$white / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$per_black <- round(dfStatesPop3$black / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$per_asian <- round(dfStatesPop3$asian / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$per_hispanic <- round(dfStatesPop3$hisp / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$per_OTHERS <- round(dfStatesPop3$OTHERS / dfStatesPop3$totpop, digits = 3)

### 12. Add a totals row and save
allRacesInUS <- colSums(dfStatesPop3[,3:7])
allRaces <- sum(allRacesInUS) ### 4125164
raceSharesInUS <- round((allRacesInUS/allRaces), digits=3)

dfTotalsRow <- dfStatesPop3[1,] ### dummy to get columns and types
dfTotalsRow$state <- "ALL STATES"
dfTotalsRow[1,3:7] <- allRacesInUS
dfTotalsRow[1,8:12] <- raceSharesInUS
dfTotalsRow[1,2] <- allRaces
dfStatesPop3 <- rbind(dfStatesPop3, dfTotalsRow)

head(dfStatesPop3)
tail(dfStatesPop3)
save(dfStatesPop3, file = "dfStatesPop3.RData")

### 13. Examples of state AFF parameters
dfStatesPop3["California",]
sum(dfStatesPop3["California", 3:10])

dfStatesPop3["New York",]
sum(dfStatesPop3["New York", 3:10])

dfStatesPop3["Georgia",] 
sum(dfStatesPop3["Georgia", 3:10])

### Note the URL for Census data about racial shares of total U.S. pop
### https://www.census.gov/quickfacts/table/PST045215/00



