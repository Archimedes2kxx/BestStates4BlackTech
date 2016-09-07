### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

###################################################
### use map_dat() as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
###################################################

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
load(file="dfEmploymentAndShares.RData")
load(file="dfStatesPop3.RData") 
load("dfCensus2.RData")

library(tidyr)

### Table 1.1  Context -- How many people in the US in 2014 -- total, black, white, ### asian, hispanic, and OTHERS and percentages of total, white, black, asian, hispanic, and OTHERS
table1p1top <- dfStatesPop3[52,2:7]
colNames <- c("ALL", "White", "Black", "Asian", "Hispanic", "OTHERS")
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
colNames <- c("ALL", "White", "Black", "Asian", "Hispanic", "OTHERS")
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
censusGroups <- group_by(dfCensus3, occupation, sex)
dfPtsPerSex <- summarise(censusGroups, ptsPerSex = sum(personalWeight))
dfOccupationPerSex <- spread(dfPtsPerSex, key=sex, value=ptsPerSex)
dfOccupationPerSex <- data.frame(dfOccupationPerSex)
dfOccupationPerSex$perMale <- round(dfOccupationPerSex$Male /(dfOccupationPerSex$Male + dfOccupationPerSex$Female), digits=3) * 100

dfOccupationPerSex$Total <- dfOccupationPerSex$Male + dfOccupationPerSex$Female
dfOccupationPerSex <- dfOccupationPerSex[, c(1,5,2:4)] ### Put total in second column

### Add total row for ALL
nRows <- dim(dfOccupationPerSex)[1]
techSums <- as.vector(colSums(dfOccupationPerSex[2:4]))
perMaleTechSums <- as.numeric(round((techSums[2]/techSums[1]), digits = 3) * 100)
dfALL <- data.frame("ALL", t(techSums), perMaleTechSums)
colnames(dfALL) <- c("occupation", "Total", "Male","Female", "perMale")
dfOccupationPerSex <- rbind(dfOccupationPerSex, dfALL)

### Finish making things "nice"
index <- order(dfOccupationPerSex$Total, decreasing=TRUE)
dfOccupationPerSex <- dfOccupationPerSex[index,] 
dfOccupationPerSex <- dfOccupationPerSex[c(2:nRows,1),] ### ALL at the bottom
dfOccupationPerSex$Female <- NULL
rownames(dfOccupationPerSex) <- NULL
colnames(dfOccupationPerSex) <- c("occupation", "Total", "Male", "%-Male")
dfOccupationPerSex

### Tables 2A, 2B, 2C, 2D. Racial groups in each state  
### ... state, <racial>TechEmp, totalTechEmp, per<Racial>TechEmp, totPop, <racialPop>, 
###        per<Racial>Pop, <racial>Parity ... 
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show full tables 2WW, 2BB, 2AA, 2HH in appendices on GitHub

### Table 2A Black tech employment vs Black population count
dfEmploymentAndShares_black <- subset(dfEmploymentAndShares, select=c(state, black, perBlack))
head(dfEmploymentAndShares_black)
dfStatePop3_black <- subset(dfStatesPop3, select=c(state, black, perBlack))
head(dfStatePop3_black)
### Use subset( ..., select ) ... or maybe use dplyr and calculate parity within the table generation
dfStateParity_black <- merge(dfEmploymentAndShares_black, dfStatePop3_black, by = "state")
head(dfEmploymentAndShares)
head(dfStatesPop3)
head(dfStateParity_black)
dfStateParity_black$parity <- round(dfStateParity_black$perBlack.x/dfStateParity_black$perBlack.y, digits=3)
head(dfStateParity_black)
columnNames <- c("state", "blackTech", "perBlackTech", "blackPop", "perBlackPop", "blackParity")
colnames(dfStateParity_black) <- columnNames
head(dfStateParity_black)
index <- order(dfStateParity_black$blackTech, decreasing=TRUE)
dfStateParity_black <- dfStateParity_black[index,]
head(dfStateParity_black, 11)
tail(dfStateParity_black,20)

### Maps 2A, 2B, 2C, 2D ... maps of white, black, asian, hispanics in state tech sectors
### Maps 2.1A, 2.1B, 2.1C, 2.1D ... maps of % white, black, asian, hispanics in states.

### Plots 2A, 2B, 2C, 2D ... regression lines for racial tech employment vsl 
### racial population in each state. 
### The Beta slopes are printed on the graphs
### ... ALL ON THE SAME PLOT FRAME so user can see that asian slope is much 
### steeper than white, black, and hispanic 

### Plots 3A, etc ... regression racial pop vs. parity

