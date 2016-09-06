### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

###################################################
### use map_dat() as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
###################################################

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
load(file="dfEmploymentAndShares.RData")
load(file="dfStatesPop3.RData") 
load("dfCensus2.RData")

library(dplyr)
##library(tidyr)

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

### Table 1.3 Sex ... must sum up the person weights
censusSex <- group_by(dfCensus2, sex)
dfPtsPerSex <- summarise(censusSex, ptsPerSex = sum(personalWeight))
colnames(dfPtsPerSex) <- c("Sex", "Employees")
dfSex <- data.frame(dfPtsPerSex)
### colnames(dfSex) <- NULL
dfSex

### Table 1.4 Occupations
censusOccupation <- group_by(dfCensus2, occupation)
dfPtsPerOccupation <- summarise(censusOccupation, ptsPerOccupation = sum(personalWeight))
colnames(dfPtsPerOccupation) <- c("Occupation", "Employees")
dfOccupation <- data.frame(dfPtsPerOccupation)
index <- order(dfOccupation$Employees, decreasing=TRUE)
dfOccupation[index,]


### Tables 2A, 2B, 2C, 2D. Racial groups in each state  
### ... state, <racial>TechEmp, totalTechEmp, per<Racial>TechEmp, totPop, <racialPop>, per<Racial>Pop, <racial>Parity ... bottom row shows racial betas described below
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show full tables 2AA, 2BB, 2CC, 2DD in appendices on GitHub

dfEmploymentAndShares
dfStatesPop3

### Maps 2W, 2B, 2A, 2H ... maps of white, black, asian, hispanics in tech employment states.
### Maps 2.1W, 2.1B, 2.1A, 2.1H ... maps of % white, black, asian, hispanics in states.

### Plots 2W, 2B, 2A, 2H ... regression lines for racial component ot each state's tech sector vs. ### the racial component of each state's population. The Beta slopes are in Tables 2A, 2B, 2C, 2D 
### ... ALL ON THE SAME PLOT FRAME so user can see that asian slope is much 
### steeper than white, black, and hispanic 


