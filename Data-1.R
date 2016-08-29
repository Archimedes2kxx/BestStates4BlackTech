### Read census data from files downloaded from U.S. Census DataWeb site PUMA 2014

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")

############
### A. Codebooks ...
###  Read in raw code files. All info raw code files is in one column ... 
###  Code in first column, some spaces, then text for label
###  Raw code files for each variable were prepared by copying the appropriate 
###    part of the full codebook into separate .txt files. 
### 
###     Ignore temporary "code" "labels" headers in first row of each file

### Note: "Hispanic" was manually added to last row of census list of races in Race-Short-Names-Code.txt file
###         ... with code = 99

### Note that codes and labels are separated by variable number of blanks, and labels for state, 
###     race, and tech also contain blanks; so we can't read into two columns with simple 
###     read.csv(blah, blah, sep=" ", blah, blah)

file = "SEX-Code.txt"
sexRawCodes = read.csv(file, header=FALSE, sep=" ", stringsAsFactors = FALSE, colClasses = "character")

file = "Race-Short-Names-Code.txt"
raceRawCodes = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE, colClasses = "character")

file = "State-Code.txt"
stateRawCodes = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE, colClasses = "character")

file = "Tech-Code.txt"
techRawCodes = read.csv(file, header=FALSE, sep="\n", stringsAsFactors = FALSE, colClasses = "character")

makeCodeBook = function(rawCodes){
    ### Split the single raw code column into two cols: code and  label
    R = nrow(rawCodes)
    mat = matrix(data=NA, nrow=R-1, ncol=2) 
    ### Initialize empty matrix, omitting temporary "header" in first row
    ###     That's why R-1, instead of R rows 
    
    for (i in 2:R) {
        line = sub("  ", "~", rawCodes[i,]) ### "~" not used in labels
        parts = strsplit(line, "~", fixed=TRUE)
        mat[i-1,] = c(unlist(parts))
    }
    colnames(mat) = c("codes", "labels")
    df = as.data.frame(mat, stringsAsFactors=FALSE)  
    return(df)
}

raceCodeBook = makeCodeBook(raceRawCodes)
stateCodeBook = makeCodeBook(stateRawCodes)
techCodeBook = makeCodeBook(techRawCodes) 
sexCodeBook = makeCodeBook(sexRawCodes)

### Drop the state initials, i.e., everything from "/" to end of line
stateCodeBook$labels <-gsub("/.*","",stateCodeBook$labels)

#########################
### B. Read original census data ... all variables read as character 
file = "Sex-Race-Hisp-InfoTechOccupations-AllStates-PersonalWeight-PUMS-2014-Data.csv"
census1 = read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE, colClasses = "character")
str(census1) ### 39692 for population weights, all states, tech occupations, all races,
        ### all Hispanic subgroups, and sex

write.csv(census1, file="census1.csv")
census2 = census1

########################
########################
### C. Convert variables to factors with labels from codebooks

### Use comfortable variable names and convert weights to integers
colnames(census2) = c("perWeight", "sex", "race", "state", "hisp", "tech")

### Add new category to race = "HISP"
### ... HISP = "1" for "not Hispanic"
### ... so change race values to 99 ("hispanic") when hisp != 1
rows <- census2$hisp != "1"
census2$race[rows] <- "99"

### Convert personal weights to integers
census2$perWeight <- as.integer(census2$perWeight)
str(census2)

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
            print(paste("Label not found for i and value = ", i, " ", xValues[i]))
            xLabels = c(xLabels, paste("Label not found for i = ", i, " ", xValues[i]))
        }
        i = i + 1
    }
    facVar = factor(xVar, levels=xValues, labels=xLabels)
    return(facVar)
}    

census2$state = sprintf("%03s", census2$state) ### states are 3-digit codes, some with leading zeros
census2$state = createFactorsWithLabels(census2$state, stateCodeBook)

census2$race = createFactorsWithLabels(census2$race, raceCodeBook)
census2$tech = createFactorsWithLabels(census2$tech, techCodeBook)
census2$sex = createFactorsWithLabels(census2$sex, sexCodeBook)
str(census2) ### 39692 obs. of  6 variables:
head(census2)

### Save census2 into files
write.csv(census2, file="census2.csv")
save(census2, file="census2.RData")

