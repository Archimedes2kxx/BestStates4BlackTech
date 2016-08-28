### Read census data from files downloaded from U.S. Census DataWeb site PUMA 2014

############
### A. Original data ...
############
### 1. Read in Codebook variables/labels and input population parameters for each state
###     All info in one column ... Code in first column, space, then text for label
###     Files for each variable prepared by copying the part of the full codebook
###         into separate .txt files

library(dplyr)
setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

### Ignore header rows in .txt files, i.e, first row contains "header" information, but ignore
###   because columns for all label files are not separated by same number of blanks
file = "Race-Short-Code.txt"
raceCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)

file = "SEX-Code.txt"
sexCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)

file = "State-Code.txt"
stateCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)

file = "SOCP-Code.txt"
occupationCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)

makeCodeBook = function(labelsMatrix){
    ### Split the single label column into two cols: code and  label
    R = nrow(labelsMatrix)
    mat = matrix(data=NA, nrow=R-1, ncol=2) 
    ### Initialize empty matrix, omitting phony "header" in first row
    ###     That's why R-1, instead of R rows 
    
    for (i in 2:R) {
        line = sub("  ", "~", labelsMatrix[i,]) ### ~ not used in labels
        parts = strsplit(line, "~", fixed=TRUE)
        mat[i-1,] = c(unlist(parts))
    }
    colnames(mat) = c("codes", "labels")
    df = as.data.frame(mat)  
    df$codes = as.character(df$codes)
    df$labels = as.character(df$labels)
    return(df)
}

raceCodeBook = makeCodeBook(raceCodeLabels)
stateCodeBook = makeCodeBook(stateCodeLabels)
occupationCodeBook = makeCodeBook(occupationCodeLabels) 
sexCodeBook = makeCodeBook(sexCodeLabels)

### Drop the state initials, i.e., everything from "/" to end of line
stateCodeBook$labels <-gsub("/.*","",stateCodeBook$labels)

### 2. Read data ... read all variables as strings 
file = "Sex-Race-Hisp-AllOccupations-AllStates-PersonalWeight-PUMS-2014-Data.csv"
### file = "Sex-Race-Hisp-Occupation-State-PersonalWeight-PUMS-2014-Data.txt"
### file = "Race-CA-DC-GA-NC-NY-WA-Occupation-PUMS-2014.csv"
census1 = read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE, colClasses = "character")
### str(census) ### 45347 obs. of  6 variables for all states
str(census1) ### 3131680 obs. of  6 variables: for all states, all occupations, all races,
        ### all Hispanic subgroups, and sex

### Use comfortable variable names and convert weights to integers
colnames(census1) = c("perWeight", "hisp", "race", "state", "sex", "occupation")
census2$perWeight <- as.integer(census2$perWeight)

### Create data frame that contains the sum of all personal weights for each state
### ... will be used in Stats-2 to estimate the actual number of blacks, asians, etc in tech in each state
dfStateTotPopPerWeights <- census2 %>%
    group_by(state) %>%
    summarise(statTotPersonWeights = sum(perWeight))
dfStateTotPopPerWeights
head(census2$perWeight)
str(dfStateTotPopPerWeights)
write.csv(census1, file="census1.csv")
census2 = census1

### 3. Now delete all of the folk who are not in tech
###     First get the tech codes ... mix of integers and letters, at least one "X"
file = "TECH-Code.txt"
techCodeLabels <- read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)
str(techCodeLabels)
colnames(techCodeLabels) <- "techCodes"
techCodeLabels$techCodes

head(levels(census2$occupation))




### 4. Convert variables to factors with labels from codebook
### In other words, convert data from integers to category names, e.g., black, California, etc
### xCodes are comprehensive dictionaries that contains all codes, not just the ones for this report
### Loop through values in each char variable, selecting matching label in xCodes
###     Returns factor version with labels on values
createFactorsWithLabels = function(xVar, xCodes){
    xValues = sort(unique(xVar))
    nValues = length(xValues)
    
    ### Read all codes as characters, not intergers or 
    codeVec = as.character(xCodes[,1])
    xLabels = vector(mode="character") ### empty char vector
    i = 1
    while(i <= nValues){
        ### Find first occurence of value in codeVec
        k = match(xValues[i], codeVec, nomatch = -1) 
        if (k!= -1) {
            xLabels = c(xLabels, xCodes[k,2])
        } else {
            xLabels = c(xLabels, paste("Label not found for i = ", i, " ", xValues[i]))
        }
        i = i + 1
    }
    facVar = factor(xVar, levels=xValues, labels=xLabels)
    return(facVar)
}    

census2$race = createFactorsWithLabels(census2$race, raceCodeBook)
census2$state = sprintf("%03s", census2$state) ### states are 3-digit codes, some with leading zeros
census2$state = createFactorsWithLabels(census2$state, stateCodeBook)
census2$occupation = createFactorsWithLabels(census2$occupation, occupationCodeBook)
census2$sex = createFactorsWithLabels(census2$sex, sexCodeBook)

### 4. Save census2 df into files
write.csv(census2, file="census2.csv")
save(census2, file="census2.RData")

#############
###################################

### B. In census3, add new category to race factor = "hispanic"
census3 <- census2
head(census3$race)

### In census3, add new category to race factor = "hispanic
### ... hispanic = "1" for "not Hispanic"
### ... so change race values to "hispanic" when hisp != 1

census3$race <- as.character(census3$race)
rows <- census3$hisp != "1"
census3$race[rows] <- "Hispanic"
census3$race <- as.factor(census3$race)
levels(census3$race)

