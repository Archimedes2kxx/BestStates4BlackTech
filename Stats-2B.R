### Stat 2-B ... Maps and Plots

################################################
################################################ THurs 9/29 @ 5"12 pm
### Use map_dat() in ggplot2 as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################

load(file="functions-0.rda")
load(file="dfRaceSexCountAndShares.rda")
load(file="dfStatesPop3.rda") 

###load("dfCensus3.rda")
###load("dfCensus2.rda")
###load("dfCensus2.2010.rda")
###load("dfCensus3.2010.rda")

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

####################################
### Tables 4A, 4B, 4C, 4D, 4E, 4F Racial, ethnic, female groups in each state  
### ... sorted by decreasing racialTechEmp so users can see "Top 10"
### ... Only show top 10 in report, show link to full tables in page on git-io
dfEmp <- dfRaceSexCountAndShares
dfPop <- dfStatesPop3

dfTechPopWhite <- makeTechPopTable(dfEmp, dfPop, "White")
dfTechPopBlack <- makeTechPopTable(dfEmp, dfPop, "Black")
dfTechPopHispanic <- makeTechPopTable(dfEmp, dfPop, "Hispanic")
dfTechPopAsian <- makeTechPopTable(dfEmp, dfPop, "Asian")
dfTechPopOTHERS <-makeTechPopTable(dfEmp, dfPop, "OTHERS")
dfTechPopFemale <-makeTechPopTable(dfEmp, dfPop, "Female")
dfTechPopFemAsian <-makeTechPopTable(dfEmp, dfPop, "FemAsian")
dfTechPopFemNonAsian <-makeTechPopTable(dfEmp, dfPop, "FemNonAsian")

dfTable4A <- dfTechPopWhite 
dfTable4B <- dfTechPopBlack
dfTable4C <- dfTechPopAsian
dfTable4D <- dfTechPopHispanic 
dfTable4E <- dfTechPopFemAsian
dfTable4F <- dfTechPopFemNonAsian
head(dfTable4A)

######################################
##### Make foreign tech tables ... Asian and non-Asian
dfTable4G <- makeForeignTechTable(dfForeignRaceSexCountAndShares, "Asian")
colnames(dfTable4G) <- c("State", "AllForeign", "perState", "Asian", "perAsian")
head(dfTable4G)

### A few states employ no foreign techs, so the denominator of perState is zero
dfTable4G <- addMissingStatesToTable(dfTable4G, dfStatesPop3)
head(dfTable4G)
tail(dfTable4G)

######### Handle as special case ... subtract Asian from Foreign
dfTable4H <- makeForeignNonAsianTechTable(dfTable4G) ### special case
colnames(dfTable4H) <- c("State", "AllForeign", "perState", "NonAsian", "perNonAsian")
head(dfTable4H)

################################
### Table 4 ... Big Sixes
dfTable4 <- makeTable4(dfRaceSexCountAndShares)
dfTable4 ### Citizens

dfTable4.1 <- makeTable4(dfForeignRaceSexCountAndShares)
dfTable4.1 ### Foreigners


save(dfTable4, dfTable4.1, dfTable4A, dfTable4B, dfTable4C, dfTable4D, dfTable4E, dfTable4F, dfTable4G, dfTable4H, dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian, file="dfTab4.rda")

#######################################
### Maps 4A, B, C, D, E, F, G ... state maps of white, black, asian, hispanic, female Asians, and female non-Asians in  tech 
### Follow W. Chang's cookbook p 313 for U.S. with lower 48 states

### Explorations of the data showed that Asians had the highest concentration in California of any group in any state. So make their California concentratration the brightest color on all six maps. Store this concentration in the District of Columbia on each map because it is too small to be visible on these maps
(maxAsianPerState <- max(dfTechPopAsian[-1,"perState"])) ### omit total row

states_map <- map_data("state") ### for ggplot calls in the makeTechPopMap function

(dfMap4A <- makeTechPopMap(dfTechPopWhite,"White", maxAsianPerState, "A. White Techs"))
(dfMap4B <- makeTechPopMap(dfTechPopBlack,"Black", maxAsianPerState, "B. Black Techs"))
(dfMap4C <- makeTechPopMap(dfTechPopAsian,"Asian", maxAsianPerState, "C. Asian Techs"))
(dfMap4D <- makeTechPopMap(dfTechPopHispanic,"Hispanic", maxAsianPerState, "D. Hispanic Techs"))
(dfMap4E <- makeTechPopMap(dfTechPopFemAsian,"FemAsian", maxAsianPerState, "E. FemAsian Techs"))
(dfMap4F <- makeTechPopMap(dfTechPopFemNonAsian,"FemNonAsian", maxAsianPerState, "F. FemNonAsian Techs"))
(dfMap4G <- makeTechPopMap(dfTable4G,"Asian", maxAsianPerState, "Map 4G. Foreign (Asian) Techs"))
(dfMap4H <- makeTechPopMap(dfTable4H,"Asian", maxAsianPerState, "Map 4H. Foreign (NonAsian) Techs"))

save(dfMap4A, dfMap4B, dfMap4C, dfMap4D, dfMap4E, dfMap4F, dfMap4G, dfMap4H, file="dfMap4.rda")

### This will save a 400x400 file at 100 ppi
(dfMap4G)
ggsave("dfMap4G.png", width=5, height=5, dpi=200)

###########################
### Plots 5 ...
### Run regressions before plots so we can display beta values in upper left of each plot and impose the full regression line on the scatterplot ... smooth_geom only producesshort stubs for some groups

### Regression racial population vs. racial Tech 

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
beta1000["Hispanic"]
beta1000["White"]
#####################
### Now the plots

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
### Table 5 ... summary of national advantages for each group

rList <- list(dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopOTHERS, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian)
names(rList) <- c("White", "Black", "Asian", "Hispanic", "OTHERS", "Female", "FemAsian", "FemNonAsian")
(dfTable5 <- makeSummary(rList, beta1000))

save(ggPlotAsian, ggPlotWhite, ggPlotBlack, ggPlotHispanic, ggPlotFemAsian, ggPlotFemNonAsian, beta1000, dfTable5, file="dfPlot5Tab5beta1000.rda")

save(dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian, file="dfTechPop.rda")
