### Explore R6 objects
###install.packages("R6")
library(R6)
file = "census2.csv"
census2 = read.csv(file, stringsAsFactors = TRUE)

State <- R6Class("State",
    public=list(
        name = NULL,
        totalPop = NULL,
        perBlackPop = NULL,
        blackPop = NULL,
        numObs = NULL,
        df = NULL,
        initialize = function(name=NA,
                              perBlackPop = NA,
                              totalPop = NA,
                              df = NA) {
            self$name <- name
            self$perBlackPop <- perBlackPop
            self$totalPop <- totalPop
            self$blackPop <- totalPop * perBlackPop
            self$df <- df
            self$numObs <- dim(df)[1]
        }
    )
)

CA <- State$new("California", .065, 40000000, census2)
CA$blackPop
CA$name
CA$perBlackPop
CA$totalPop
CA$numObs
