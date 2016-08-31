### B. Read Amdrican Fact Finder (AFF) parameters about states from AFF pages for 2014

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
library(dplyr)

### 1. Read data ... American Fact finder URL ... table PEPSR6H
### http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=PEP_2015_PEPSR6H&prodType=table
file = "PEP_2014_PEPSR6H_with_ann.csv"
dfStatesPop1 <- read.csv(file, header=FALSE, stringsAsFactors = FALSE)
### str(statesPop)
### head(statesPop,5)

### Get var names from second row
varNames <- as.vector(unlist(dfStatesPop1[1,]))
colnames(dfStatesPop1) <- varNames
### str(dfStatesPop1)

### Drop display labels and var names in 1st, 2nd rows
dfStatesPop1 <- dfStatesPop1[c(-1, -2),]
### str(dfStatesPop1)
### head(dfStatesPop1)
save(dfStatesPop1, file="dfStatesPop1")

### 2. Get data for all non-hispanics
dfNonHispanics <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="totsex"& Hisp.id=="nhisp", 
                         select=c(wa:tom))
dfNonHispanics$totpop <- NULL

### 3. Get data for hispanics
dfHispanics <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id =="totsex"& Hisp.id=="hisp", 
                      select=c(totpop))
dfHispanics$hisp <- dfHispanics$totpop
dfHispanics$totpop <- NULL

### 4. Get total pop of the state
dfTotPop <- subset(dfStatesPop1, Year.id=="est72014" & Sex.id=="totsex" & Hisp.id=="tothisp", 
                   select=c(`GEO.display-label`, totpop))

### 5. cbind the columns
dfStatesPop2 <- cbind(dfTotPop, dfNonHispanics, dfHispanics)
### str(dfStatesPop2)

### 6. Confer final names on all variables
columnNames <- c("state", "totpop", "white", "black", "amInAlNat", "asian", "pacific", "mixed" , "hisp")
colnames(dfStatesPop2) <- columnNames
### str(dfStates1)

### 7. Convert state to factor ... useful for tapply functions and other stuff
dfStatesPop2$state <- as.factor(dfStatesPop2$state)
### str(dfStates2[,1])

### 8. Convert data columns to integers
vec <- as.integer(unlist(dfStatesPop2[,2:9]))
mat <- matrix(vec, nrow=51, ncol=8)
dfStatesPop2 <- data.frame(dfStatesPop2[,1], mat)
colnames(dfStatesPop2) <- columnNames ### ... yes, again
### str(dfStates2)
### head(dfStates2)

### 9. Add row names = states .... convenient for subsetting with brackets
rownames(dfStatesPop2) <- dfStatesPop2$state
### head(dfStates2)

### 10. Combine races that will not be analyzed, then delete their columns
dfStatesPop2$other <- dfStatesPop2$amInAlNat + dfStatesPop2$pacific + dfStatesPop2$mixed
dfStatesPop2$amInAlNat <- NULL
dfStatesPop2$pacific <- NULL
dfStatesPop2$mixed <- NULL
str(dfStatesPop2)

### 11. Calculate derived parameters ... percentage of each racial group in the total population
dfStatesPop3 <- dfStatesPop2
### str(dfStates3)
dfStatesPop3$perWhite <- round(dfStatesPop3$white / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$perBlack <- round(dfStatesPop3$black / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$perAsian <- round(dfStatesPop3$asian / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$perHisp <- round(dfStatesPop3$hisp / dfStatesPop3$totpop, digits = 3)
dfStatesPop3$perOther <- round(dfStatesPop3$other / dfStatesPop3$totpop, digits = 3)
str(dfStatesPop3)
head(dfStatesPop3)

save(dfStatesPop3, file = "dfStatesPop3.RData")

### 11. Examples of state AFF parameters
dfStatesPop3["California",]
sum(dfStatesPop3["California", 3:10])

dfStatesPop3["New York",]
sum(dfStatesPop3["New York", 3:10])

dfStatesPop3["Georgia",] 
sum(dfStatesPop3["Georgia", 3:10])




