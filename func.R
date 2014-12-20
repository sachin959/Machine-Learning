func <- function(Training, Validation){# exclude near zero variance features
    library(randomForest)
    nzvcol <- nearZeroVar(Training)
    Training <- Training[, -nzvcol]
    cntlength <- sapply(Training, function(x) {
        sum(!(is.na(x) | x == ""))
    })
    nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
    descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                     "cvtd_timestamp", "new_window", "num_window")
    excludecols <- c(descriptcol, nullcol)
    Training <- Training[, !names(Training) %in% excludecols]
    rfModel <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)
    rfModel
}
