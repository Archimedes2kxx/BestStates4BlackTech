### Functions used by other files    


##################################
### Data-1



##################################
### Data -1B

### Read manually edited codebooks 
###  ... commas added between codes and labels ... commas deleted within labels ... and 99 Hispanic added manually
file = "Codes-Race.txt"
raceCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
raceCodes

file = "Codes-State.txt"
### Note: the District of Columbia is abbreviated to "Dist of Col" to let table fit on one page without wrapping
stateCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
stateCodes$State <-gsub("/.*","",stateCodes$State) ### Drop state initials, e.g., "New York/NY
stateCodes

file="Codes-Sex.txt"
sexCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
sexCodes

file="Codes-Citizen.txt"
citizenCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
citizenCodes

file="Codes-Occupation.txt"
occupationCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
occupationCodes

file="Codes-Area.txt"
areaCodes = read.csv(file, header=TRUE, stringsAsFactors = FALSE, colClasses = "character")
areaCodes

listCodes <- list(raceCodes, stateCodes, sexCodes, citizenCodes, areaCodes, occupationCodes)

##################################
### Stats-2A


### Stats2B


### Stats-2C




save(listCodes, file="function-0.rda")
###save(raceCodes, stateCodes, sexCodes, citizenCodes, areaCodes, occupationCodes, file = "functions-0.rda")
