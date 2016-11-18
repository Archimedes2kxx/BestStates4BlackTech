### Stat 2-C ... Best States

################################################
################################################ THurs 9/29 @ 5"12 pm
### Use map_dat() in ggplot2 as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
##############################################

load(file="functions-0.rda")
load("dfTab4.rda")

#######################################
######################
#### Conclusions
### Table 6. Stats for parity variable of racial groups in each state  

listDFs <- list(dfTechPopWhite, dfTechPopBlack, dfTechPopAsian, dfTechPopHispanic, dfTechPopFemale, dfTechPopFemAsian, dfTechPopFemNonAsian)
names(listDFs) <- c("White",  "Black", "Asian", "Hispanic",  "Female",  "FemAsian", "FemNonAsian")
dfParity <- makeParity(listDFs)
(dfTable6 <- dfParity)

##################################
####### Tables 7 ... Finalists for black, hispanic, femAsians, and femNonAsians

### Tables 7 ... sorted by parity
(dfTable7A <- makeTable7(dfTechPopWhite , dfParity, "White", "A"))
(dfTable7B <- makeTable7(dfTechPopBlack , dfParity, "Black", "B"))
(dfTable7C <- makeTable7(dfTechPopAsian , dfParity, "Asian", "C"))
(dfTable7D <- makeTable7(dfTechPopHispanic , dfParity, "Hispanic", "D"))
(dfTable7E <- makeTable7(dfTechPopFemAsian , dfParity, "FemAsian", "E"))
(dfTable7F <- makeTable7(dfTechPopFemNonAsian , dfParity, "FemNonAsian","F"))

####### Tables 7 
### Sorted by tech share of the info tech sector 
(dfTable8A <- makeTable8(dfTable7A,"White", "A")) 
(dfTable8B <- makeTable8(dfTable7B, "Black", "B")) 
(dfTable8C <- makeTable8(dfTable7C, "Asian", "C")) 
(dfTable8D <- makeTable8(dfTable7D, "Hispanic", "D")) 
(dfTable8E <- makeTable8(dfTable7E, "FemAsian", "E"))
(dfTable8F <- makeTable8(dfTable7F, "FemNonAsian", "F")) 

##############################
#############################
save(dfTable6, dfTable7A, dfTable7B, dfTable7C, dfTable7D, dfTable7E, dfTable7F, dfTable8A, dfTable8B, dfTable8C, dfTable8D, dfTable8E, dfTable8F, file="dfTab67A7B7C7D7E7F8A8B8C8D8E8F.rda")


