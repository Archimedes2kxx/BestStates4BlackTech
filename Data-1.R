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

numSampleObservations = dim(census2)[1]

### 5. Save census2 df into file
save(census2, numSampleObservations, file="census2.RData")
