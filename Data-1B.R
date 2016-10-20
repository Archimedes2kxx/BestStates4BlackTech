### B. Read Amdrican Fact Finder (AFF) parameters about racial population of states from AFF pages for 2014 
###    Compute percent white, black, asian, hispanic, OTHERS, and female (vers 0.9)
######################
####### Upgrading from vers 0.8 to vers 0.9, treat "female" as a new ethnic group 
###     Add its column (or row) to end of each table 

### 1. Read downloaded data ... American Fact finder URL ... table PEPSR6H
### http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=PEP_2015_PEPSR6H&prodType=table
file = "PEP_2014_PEPSR6H_with_ann.csv"
dfStatesPop1 <- read.csv(file, header=FALSE, stringsAsFactors = FALSE)

### s=Set column names = variable names 
varNames <- as.vector(unlist(dfStatesPop1[1,]))
colnames(dfStatesPop1) <- varNames
str(dfStatesPop1)

### Drop display labels and var names in 1st, 2nd rows
dfStatesPop1 <- dfStatesPop1[c(-1, -2),]
head(dfStatesPop1)

### Rationale for subsetting ... The variables in the download mimic the Webpage. On the WebPage a user has to select the year, select hispanic or not hispanic (not hispanic yields white/not hispanic, black/not hispanic, etc ... to get hispanic select "hispanic" ... and to get both, select "tothisp), sex (sex.id ... this choice is not on the page, but = "totsex, otherwise select only "males"  or only "females"; to subset after the data is downloaded, set sex.id == "female", etc), 

### 2. Get data for all non-hispanics
dfNonHispanics <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="totsex"  & Hisp.id=="nhisp", select=c(wa:tom)) ### wa = white americans ... tom = two or more groups
head(dfNonHispanics)

### 3. Get data for hispanics
dfHispanics <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="totsex"  & Hisp.id=="hisp", select=totpop)
head(dfHispanics$TotPop )
dfHispanics$hisp <- dfHispanics$TotPop 
dfHispanics$TotPop <- NULL
head(dfHispanics)

### 4. Get total pop of the state
dfTotPop <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id=="totsex" & Hisp.id=="tothisp", select=c(`GEO.display-label`, totpop)) ### get states and total population
head(dfTotPop)

### 4F. Get total female pop of the states
dfFemale <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id=="female" & Hisp.id=="tothisp", select=c(`GEO.display-label`, totpop))
colnames(dfFemale) <- c("State", "Female")
head(dfFemale)

### 4AF. Get data for all Asian females
dfFemAsian <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="female"  & Hisp.id=="nhisp", select=c(`GEO.display-label`,aa)) ### Asian American females
colnames(dfFemAsian) <- c("State", "FemAsian")
head(dfFemAsian)

##############################
### Modify code to add Female, femAsian, femNonAsian, per_female, per_femAsian, per_femNonAsian

dfTotPopFemale <- merge(dfFemale, dfFemAsian)
colnames(dfTotPopFemale) <- c("State", "Female", "FemAsian")
head(dfTotPopFemale)

### Variables are character, can't subtract yet
### dfTotPopFemale$FemNonAsian <- dfTotPopFemale$Female - dfTotPopFemale$femAsian
### head(dfTotPopFemale)

###############################
###############################

### 5. cbind the columns
dfStatesPop2 <- cbind(dfTotPop, dfNonHispanics, dfHispanics, dfTotPopFemale[,2:3]) ###,dfAsianFemales)
head(dfStatesPop2)

### 6. Confer short names on all variables
columnNames <- c("State", "TotPop", "White", "Black", "amInAlNat", "Asian", "pacific", "mixed" , "Hispanic", "Female", "FemAsian")
colnames(dfStatesPop2) <- columnNames
head(dfStatesPop2)

### 7. Convert state to factor
dfStatesPop2$State <- as.factor(dfStatesPop2$State)
head(dfStatesPop2)

### 8. Note that all variables are character, not integer, so convert
rC <- dim(dfStatesPop2)
vec <- as.integer(unlist(dfStatesPop2[,-1]))
mat <- matrix(vec, nrow=rC[1], ncol=rC[2]-1)
dfStatesPop2 <- data.frame(dfStatesPop2[,1], mat)
colnames(dfStatesPop2) <- columnNames ### ... yes, again
head(dfStatesPop2)

### 9. Add non-Asian females
dfStatesPop2$emNonAsian <- dfStatesPop2$Female - dfStatesPop2$FemAsian

### 10. Combine races that will not be analyzed, then delete their columns
dfStatesPop2$OTHERS <- dfStatesPop2$amInAlNat + dfStatesPop2$pacific + dfStatesPop2$mixed
dfStatesPop2$amInAlNat <- NULL
dfStatesPop2$pacific <- NULL
dfStatesPop2$mixed <- NULL
head(dfStatesPop2)

### 11. Move female back to last position
dfStatesPop2 <- cbind(dfStatesPop2[,1:6], dfStatesPop2[,10], dfStatesPop2[,7:9])
colnames(dfStatesPop2) <- c("State", "TotPop", "White", "Black","Asian", "Hispanic", "OTHERS", "Female", "FemAsian", "FemNonAsian")
head(dfStatesPop2)

### 12. Calculate derived parameters ... percentage of each racial group in the total population ... add to data frame
dfStatesPop3 <- dfStatesPop2
dfStatesPop3$perWhite <- round(100* dfStatesPop3$White / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perBlack <- round(100 * dfStatesPop3$Black / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perAsian <- round(100 * dfStatesPop3$Asian / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perHispanic <- round(100 * dfStatesPop3$Hisp / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perOTHERS <- round(100 * dfStatesPop3$OTHERS / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perFemale <- round(100 * dfStatesPop3$Female / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perFemAsian <- round(100 * dfStatesPop3$FemAsian / dfStatesPop3$TotPop, digits = 1)
dfStatesPop3$perFemNonAsian <- round(100 * dfStatesPop3$FemNonAsian / dfStatesPop3$TotPop, digits = 1)
head(dfStatesPop3)

### 13. Add a totals row and save
totEachRace <- colSums(dfStatesPop3[,3:10])
totEachRace
(allRacesInUS <- sum(totEachRace[1:5])) ### Don't include females in overallsum
(raceSharesInUS <- round(100 * totEachRace/allRacesInUS, digits=1))
 
dfTotalsRow <- dfStatesPop3[1,] ### dummy copy to get columns and types
dfTotalsRow$State <- "ALL STATES"
dfTotalsRow[1,3:10] <- totEachRace
dfTotalsRow[1,11:18] <- raceSharesInUS
dfTotalsRow[1,2] <- sum(allRacesInUS)
dfStatesPop3 <- rbind( dfTotalsRow, dfStatesPop3)

head(dfStatesPop3)
tail(dfStatesPop3)
save(dfStatesPop3, file = "dfStatesPop3.rda")

### 14. Examples of state AFF parameters ... add rownames
dfStatesPop4 <- dfStatesPop3
rownames(dfStatesPop4) <- c(dfStatesPop4$State)
dfStatesPop4["California",]
sum(dfStatesPop4["California", 3:7])

dfStatesPop4["New York",]
sum(dfStatesPop4["New York", 3:7])

### Note the URL for Census data about racial shares of total U.S. pop
### https://www.census.gov/quickfacts/table/PST045215/00  

