setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

### 1. Read in Codebook variables/labels 
###     All info in one column ... Code in first column, space, then text for label
###     Files for each variable prepared by copying the part of the full codebook
###         into separate .txt files

file = "Race-Short-Code.txt"
raceCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)
file = "State-Code.txt"
stateCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)
file = "SOCP-Code.txt"
occupationCodeLabels = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE)

makeCodeBook = function(labelsMatrix){
    ### Split the single label column into two cols: code and  label
    R = nrow(labelsMatrix)
    mat = matrix(data=NA, nrow=R-1, ncol=2) 
    ### Initialize empty matrix, without header row
    ###     That's why R-1, instead of R rows
    
    for (i in 2:R) {
        line = sub(" ", "$$$", labelsMatrix[i,])
        parts = strsplit(line, "$$$", fixed=TRUE)
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

### 2. Read data ... read all variables as strings 
file = "Race-CA-DC-GA-NC-NY-WA-Occupation-PUMA-2014.csv"
census = read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE, colClasses = "character")
str(census)
census2 = census

### 3. Use comfortable variable names
colnames(census2) = c("race", "state", "occupation")

### 4. Convert variables to factors with labels from codebook
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
            xLabels = c(xLabels, paste("Label not found for i = ", xValues[i]))
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

### 5. Save census2 df into file 
file="census2.csv"
write.csv(census2, file) 

### Blacks in states
getPercentages = function(perBlackPop, dfCensus, stateLabel, totalPop) {
df = subset(dfCensus, dfCensus$state == stateLabel)
options("scipen"=10) ## suppress scientific format
raceVec = df$race
totalVec = table(raceVec)
grandTotal = sum(totalVec)
perBlack = totalVec[" Black"]/grandTotal
ratio = perBlack/perBlackPop
blackPop = round(perBlackPop * totalPop, digits = 0)
outVec = c(perBlack, perBlackPop, ratio, totalPop, blackPop)
names(outVec) = c("BlackTechsPer", "BlackPopPer", "RatioToPop", "TotalPop", "BlackPop")
return(outVec)
### return all values that will appear in the same row for each state in the report
}

perBlackPop = .483
totalPop = 672228
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/11
dcLabel = " District of Columbia/DC"
dcBlackPop = getPercentages(perBlackPop, census2, dcLabel, totalPop)

perBlackPop = .065 ###percent of CA pop
totalPop = 39144818
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/06
caLabel = " California/CA"
caBlackPop = getPercentages(perBlackPop, census2, caLabel, totalPop)

perBlackPop = .176 ###percent of NY population
totalPop = 19795791
### CENSUS link = http://www.census.gov/quickfacts/table/BZA110214/36
nyLabel = " New York/NY"
nyBlackPop = getPercentages(perBlackPop, census2, nyLabel, totalPop)

perBlackPop = .221 ###percent of NC population
totalPop = 10042802
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/37
ncLabel = " North Carolina/NC"
ncBlackPop = getPercentages(perBlackPop, census2, ncLabel, totalPop)

perBlackPop = .317 ###percent of GA population
totalPop = 10214860
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/13
gaLabel = " Georgia/GA"
gaBlackPop = getPercentages(perBlackPop, census2, gaLabel, totalPop)

### Washington State
perBlackPop = .041
totalPop = 7170351
### CENSUS link = http://www.census.gov/quickfacts/table/PST045215/53
waLabel = " Washington/WA"
waBlackPop = getPercentages(perBlackPop, census2, waLabel, totalPop)

### Assemble stats into a matrix, then a data frame
matStats = matrix(data=NA, nrow=6, ncol=5) 
matStats[1,] = caBlackPop
matStats[2,] = dcBlackPop
matStats[3,] = gaBlackPop
matStats[4,] = nyBlackPop
matStats[5,] = ncBlackPop
matStats[6,] = waBlackPop

rownames(matStats) = c("California", "District of Columbia", 
"Georgia", "New York", "North Carolina", "Washington")
colnames(matStats) = c("BlackTechsPer", "BlackPopPer", "RatioToPop", "TotalPop", "BlackPop")

dfStats = as.data.frame(matStats)
dfStats$TotalPop = as.integer((dfStats$TotalPop))
dfStats$BlackPop = as.integer(dfStats$BlackPop)
dfStats

### Save df in descending order of percentage of Blacks in each state's tech sector
dfStats = dfStats[order(-dfStats$BlackTechsPer),]
file = "dfStats.csv"
write.csv(dfStats, file)

